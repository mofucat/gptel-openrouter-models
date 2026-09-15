;;; gptel-openrouter-models.el --- Pick OpenRouter models for gptel -*- lexical-binding: t; -*-

;; Author: mofucat
;; URL: https://github.com/mofucat/gptel-openrouter-models
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))
;; Version: 0.3.0
;; Keywords: convenience, tools
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A small package that fetches OpenRouter's `/api/v1/models', lets you
;; pick a model via `completing-read' (the standard UI that vertico and
;; friends hook into automatically), and either sets `gptel-model' to it
;; or copies its name to the kill ring.
;;
;; No API key is needed to fetch the model list (OpenRouter's /models is
;; a public endpoint).  The appearance of candidates is left to the
;; completing-read frontend (vertico / marginalia etc.) for formatting;
;; descriptions are only passed through the annotation-function of
;; `completion-extra-properties'.
;;
;; Usage:
;;   M-x gptel-openrouter-models-pick
;;   M-x gptel-openrouter-models-copy-name
;;   M-x gptel-openrouter-models-refresh
;;
;; `gptel-openrouter-models-pick' does two things beyond setting
;; `gptel-model': it copies the model's metadata (capabilities, MIME
;; types, context window, pricing) from OpenRouter onto the model symbol,
;; and it registers that symbol with the current `gptel-backend'.  Both
;; are required for the selection to actually stick -- see the commentary
;; on `gptel-openrouter-models--register-with-backend'.
;;
;; `gptel-openrouter-models-copy-name' picks a model the same way but,
;; instead of touching `gptel-model', copies just the bare model name
;; (the part after the "owner/" prefix, e.g. "gemini-2.5-flash" from
;; "google/gemini-2.5-flash") to the kill ring.  This is handy when you
;; want to paste the name into a native (non-OpenRouter) gptel backend
;; for Gemini, Anthropic, OpenAI, etc.  With a prefix argument it copies
;; the full model ID instead.
;;
;; The fetched list is cached for `gptel-openrouter-models-cache-ttl'
;; seconds; `gptel-openrouter-models-refresh' discards the cache.
;;
;; Set up `gptel-backend' as a `gptel-make-openai' backend for
;; OpenRouter beforehand.  See README.md for details.
;;
;; gptel 0.9.8 or later is required, and the floor is set by the
;; metadata this package writes rather than by any function it calls:
;; 0.9.5 made `gptel-model' a symbol instead of a string and introduced
;; :description / :capabilities / :mime-types, 0.9.6 added
;; :context-window / :input-cost / :output-cost, and 0.9.8 added tool
;; use and prompt caching, i.e. the `tool-use' and `cache' capability
;; symbols.  On older versions the model symbol is accepted and then
;; quietly ignored, which is worse than an error.

;;; Code:

(require 'url)
(require 'json)
(require 'seq)
(require 'cl-lib)
(require 'subr-x)                        ; `when-let*' on Emacs 27/28
(require 'gptel)

(defgroup gptel-openrouter-models nil
  "Pick OpenRouter models for gptel."
  :group 'gptel)

(define-error 'gptel-openrouter-models-error
  "gptel-openrouter-models: could not fetch the model list")

(defcustom gptel-openrouter-models-endpoint "https://openrouter.ai/api/v1/models"
  "OpenRouter model-list API endpoint."
  :type 'string)

(defcustom gptel-openrouter-models-timeout 15
  "Timeout in seconds for fetching the model list."
  :type 'integer)

(defcustom gptel-openrouter-models-cache-ttl 3600
  "How long, in seconds, to reuse a previously fetched model list.
The response is around 700 KB, so refetching it on every invocation
blocks Emacs for no good reason.  Set to nil to disable caching;
`gptel-openrouter-models-refresh' discards the cache explicitly."
  :type '(choice (const :tag "No caching" nil)
                 (integer :tag "Seconds")))

(defface gptel-openrouter-models-annotation-face
  '((t :inherit completions-annotations))
  "Face used for the model description shown next to each candidate.
Defaults to `completions-annotations' (typically dimmed/italic), so the
description is visually distinct from the model ID itself.")


;;;; JSON

(defun gptel-openrouter-models--json-parse (string)
  "Parse STRING as JSON, with objects as alists and arrays as lists.
Uses Emacs' native JSON parser when the build provides one (roughly an
order of magnitude faster on OpenRouter's payload), else json.el."
  (if (fboundp 'json-parse-string)
      ;; Called through `funcall' so byte-compiling on an Emacs built
      ;; without native JSON support does not warn about the function.
      (funcall 'json-parse-string string
               :object-type 'alist :array-type 'list
               :null-object nil :false-object nil)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-null nil)
          (json-false nil))
      (json-read-from-string string))))


;;;; Fetching

(defun gptel-openrouter-models--response-status ()
  "Return the HTTP status code of the response in the current buffer, or nil.
Prefer url-http's `url-http-response-status'; otherwise parse the
status line at the top of the buffer."
  (or (bound-and-true-p url-http-response-status)
      (save-excursion
        (goto-char (point-min))
        (and (looking-at "HTTP/[0-9.]+ +\\([0-9]\\{3\\}\\)")
             (string-to-number (match-string 1))))))

(defun gptel-openrouter-models--error-detail (body raw)
  "Return a human-readable error string from parsed BODY or RAW text.
BODY is the JSON body parsed as an alist (or nil); RAW is the first
chunk of the body as a string."
  (or (and (consp body)
           (let* ((err (alist-get 'error body))
                  (detail (cond ((stringp err) err)
                                ((consp err) (or (alist-get 'message err)
                                                 (alist-get 'code err)))
                                (t (alist-get 'message body)))))
             (and detail (format "%s" detail))))
      (let ((trimmed (string-trim (or raw ""))))
        (unless (string-empty-p trimmed)
          (truncate-string-to-width trimmed 200 nil nil t)))))

(defun gptel-openrouter-models--parse-buffer ()
  "Parse the current buffer as an HTTP response from OpenRouter's /models.
Move point past the response headers, then read the JSON body and
return its `data' array (a list of alists).  Signal
`gptel-openrouter-models-error' if the headers cannot be located, if
the HTTP status is not 2xx, or if the body is not valid JSON."
  (let ((status (gptel-openrouter-models--response-status)))
    (goto-char (point-min))
    (if (bound-and-true-p url-http-end-of-headers)
        (goto-char url-http-end-of-headers)
      ;; Fall back to finding the blank line between headers and body.
      ;; Accept both CRLF ("\r\n\r\n", per RFC) and bare-LF ("\n\n")
      ;; terminators.
      (unless (re-search-forward "\r?\n\r?\n" nil t)
        (signal 'gptel-openrouter-models-error
                (list "could not find end of HTTP headers"))))
    (let* ((body-text (buffer-substring-no-properties (point) (point-max)))
           ;; `url-retrieve-synchronously' hands back a unibyte buffer, so
           ;; the body is still raw UTF-8 bytes at this point.  Reading it
           ;; without decoding turns every non-ASCII character in a model
           ;; description into mojibake: a right single quote comes out as
           ;; three Latin-1 characters instead.
           (body-text (if (multibyte-string-p body-text)
                          body-text
                        (decode-coding-string body-text 'utf-8)))
           (raw (substring body-text 0 (min 500 (length body-text))))
           ;; Wrap so we can tell "parsed to nil" from "failed to parse".
           (parsed (condition-case nil
                       (list (gptel-openrouter-models--json-parse body-text))
                     (error nil)))
           (body (car parsed)))
      (when (and status (not (<= 200 status 299)))
        (signal 'gptel-openrouter-models-error
                (list (format "HTTP %d from %s: %s" status
                              gptel-openrouter-models-endpoint
                              (or (gptel-openrouter-models--error-detail body raw)
                                  "unexpected response")))))
      (unless parsed
        (signal 'gptel-openrouter-models-error
                (list "response body is not valid JSON")))
      (alist-get 'data body))))

(defun gptel-openrouter-models--fetch-raw ()
  "Fetch OpenRouter's /models and return the data array (a list of alists)."
  (let ((buffer (condition-case err
                    (url-retrieve-synchronously
                     gptel-openrouter-models-endpoint
                     t t gptel-openrouter-models-timeout)
                  ;; DNS failures, refused connections etc. signal rather
                  ;; than return nil -- funnel them into one package error.
                  (error
                   (signal 'gptel-openrouter-models-error
                           (list (format "could not fetch %s: %s"
                                         gptel-openrouter-models-endpoint
                                         (error-message-string err))))))))
    (unless (buffer-live-p buffer)
      (signal 'gptel-openrouter-models-error
              (list (format "could not fetch %s (timeout or connection failure)"
                            gptel-openrouter-models-endpoint))))
    (unwind-protect
        (with-current-buffer buffer
          (gptel-openrouter-models--parse-buffer))
      (kill-buffer buffer))))

(defvar gptel-openrouter-models--cache nil
  "Cons cell of (TIMESTAMP . MODELS) from the last successful fetch.")

(defun gptel-openrouter-models--cache-fresh-p ()
  "Return non-nil if the cached model list may still be used."
  (and gptel-openrouter-models-cache-ttl
       (consp gptel-openrouter-models--cache)
       (< (float-time (time-since (car gptel-openrouter-models--cache)))
          gptel-openrouter-models-cache-ttl)))

(defun gptel-openrouter-models--fetch (&optional force)
  "Return the OpenRouter model list, reusing the cache when it is fresh.
With FORCE non-nil, always go to the network."
  (if (and (not force) (gptel-openrouter-models--cache-fresh-p))
      (cdr gptel-openrouter-models--cache)
    (let ((models (gptel-openrouter-models--fetch-raw)))
      (setq gptel-openrouter-models--cache (cons (current-time) models))
      models)))


;;;; Model metadata

(defun gptel-openrouter-models--id (model)
  "Extract the model ID from the MODEL alist."
  (alist-get 'id model))

(defun gptel-openrouter-models--description (model)
  "Extract the description from the MODEL alist, or nil if absent."
  (alist-get 'description model))

(defun gptel-openrouter-models--bare-name (id)
  "Return the bare model name for ID, stripping any \"owner/\" prefix.
For example \"google/gemini-2.5-flash\" becomes \"gemini-2.5-flash\".
An ID without a slash is returned unchanged."
  (if (and (stringp id) (string-match "\\`[^/]+/\\(.+\\)\\'" id))
      (match-string 1 id)
    id))

(defconst gptel-openrouter-models--mime-alist
  '(("image" "image/jpeg" "image/png" "image/gif" "image/webp")
    ("file"  "application/pdf"))
  "MIME types gptel can attach, per OpenRouter input modality.

OpenRouter also reports \"audio\" and \"video\" input modalities, but
gptel's OpenAI-compatible request construction has no way to send those,
so they are deliberately absent: listing them here would advertise an
attachment type that fails at request time, which is worse than not
offering it.  Add an entry once gptel grows support for the modality.")

(defun gptel-openrouter-models--mime-types (model)
  "Return the MIME types MODEL accepts as input, as a list of strings."
  (let (mimes)
    (dolist (modality (alist-get 'input_modalities
                                 (alist-get 'architecture model)))
      (setq mimes
            (append mimes
                    (cdr (assoc modality
                                gptel-openrouter-models--mime-alist)))))
    (delete-dups mimes)))

(defun gptel-openrouter-models--price (model key)
  "Return MODEL's KEY price in US dollars per million tokens.
Return 0 when the price is missing or not positive.  OpenRouter reports
prices as strings in dollars per token."
  (let* ((value (alist-get key (alist-get 'pricing model)))
         (per-token (cond ((stringp value) (string-to-number value))
                          ((numberp value) value)
                          (t 0))))
    (if (> per-token 0) (* per-token 1e6) 0)))

(defun gptel-openrouter-models--capabilities (model)
  "Return the gptel capability symbols MODEL supports."
  (let ((params (alist-get 'supported_parameters model))
        caps)
    (when (gptel-openrouter-models--mime-types model)
      (push 'media caps))
    (when (member "tools" params)
      (push 'tool-use caps))
    (when (or (member "response_format" params)
              (member "structured_outputs" params))
      (push 'json caps))
    ;; OpenRouter spells this either way depending on the provider:
    ;; "reasoning" for models taking a reasoning config object,
    ;; "include_reasoning" for the older boolean toggle.
    (when (or (member "reasoning" params)
              (member "include_reasoning" params))
      (push 'reasoning caps))
    (when (> (gptel-openrouter-models--price model 'input_cache_read) 0)
      (push 'cache caps))
    (nreverse caps)))

(defun gptel-openrouter-models--context-window (model)
  "Return MODEL's context window in thousands of tokens, or nil."
  (let ((length (or (alist-get 'context_length (alist-get 'top_provider model))
                    (alist-get 'context_length model))))
    (and (numberp length) (> length 0) (max 1 (round length 1000)))))

(defun gptel-openrouter-models--register (model)
  "Intern MODEL's ID as a gptel model symbol and attach its metadata.
Return the symbol.

gptel reads model metadata off the symbol's property list -- see
`gptel--model-capabilities' and friends -- so a bare interned symbol
looks to gptel like a model that supports nothing: no image or file
attachments, no tool use, no context window, no pricing."
  (let ((sym (intern (gptel-openrouter-models--id model)))
        (description (gptel-openrouter-models--description model))
        (mime-types (gptel-openrouter-models--mime-types model))
        (context-window (gptel-openrouter-models--context-window model))
        (input-cost (gptel-openrouter-models--price model 'prompt))
        (output-cost (gptel-openrouter-models--price model 'completion)))
    (put sym :capabilities (gptel-openrouter-models--capabilities model))
    (when description
      (put sym :description
           (truncate-string-to-width description 200 nil nil t)))
    (when mime-types (put sym :mime-types mime-types))
    (when context-window (put sym :context-window context-window))
    (when (> input-cost 0) (put sym :input-cost input-cost))
    (when (> output-cost 0) (put sym :output-cost output-cost))
    sym))

(defun gptel-openrouter-models--register-with-backend (sym)
  "Add SYM to the model list of the current `gptel-backend'.
Return non-nil if SYM was added.

This is not cosmetic: `gptel-send' and `gptel-mode' both call
`gptel--sanitize-model', which silently resets `gptel-model' to the
backend's first model unless the current model is a member of the
backend's list.  Without this step the picked model is discarded the
moment you send anything."
  (when (and (boundp 'gptel-backend)
             (gptel-backend-p gptel-backend)
             (not (memq sym (gptel-backend-models gptel-backend))))
    (setf (gptel-backend-models gptel-backend)
          (append (gptel-backend-models gptel-backend) (list sym)))
    t))


;;;; Commands

;;;###autoload
(defun gptel-openrouter-models-list (&optional prefix force)
  "Fetch the OpenRouter model list and return it sorted by ID.
If PREFIX is non-nil, keep only model IDs matching that prefix
\(e.g. \"anthropic/\").  With FORCE non-nil, bypass the cache.
Entries without a string ID are dropped."
  (let* ((models (seq-filter (lambda (m)
                               (stringp (gptel-openrouter-models--id m)))
                             (gptel-openrouter-models--fetch force)))
         (filtered (if prefix
                       (seq-filter (lambda (m)
                                     (string-prefix-p
                                      prefix (gptel-openrouter-models--id m)))
                                   models)
                     models)))
    ;; `sort' is destructive, and the list may be the cached one.
    (sort (copy-sequence filtered)
          (lambda (a b)
            (string< (gptel-openrouter-models--id a)
                     (gptel-openrouter-models--id b))))))

(defun gptel-openrouter-models--read (prompt &optional prefix)
  "Fetch the model list and `completing-read' one model, using PROMPT.
PROMPT is passed through `format' with the candidate count as its only
argument.  If PREFIX is non-nil, only IDs matching that prefix are
offered (e.g. \"anthropic/\").  Descriptions are shown through the
completion annotation-function.  Return the selected model alist, or
nil when no models are available."
  (message "Fetching OpenRouter model list...")
  (let* ((models (gptel-openrouter-models-list prefix))
         (table (make-hash-table :test 'equal))
         (ids (mapcar (lambda (m)
                        (let ((id (gptel-openrouter-models--id m)))
                          (puthash id m table)
                          id))
                      models))
         (completion-extra-properties
          (list :annotation-function
                (lambda (id)
                  (when-let* ((model (gethash id table))
                              (desc (gptel-openrouter-models--description model)))
                    (propertize (concat "  " desc)
                                'face 'gptel-openrouter-models-annotation-face))))))
    (if (null ids)
        (progn (message "No models found") nil)
      (gethash (completing-read (format prompt (length ids)) ids nil t)
               table))))

;;;###autoload
(defun gptel-openrouter-models-pick (&optional prefix)
  "Pick a model from OpenRouter and set `gptel-model' to it.
If PREFIX is given, narrow the search to models matching that prefix
\(e.g. \"anthropic/\").  When called interactively, all models are
considered without any prefix filtering.

The model's OpenRouter metadata (capabilities, MIME types, context
window, pricing) is attached to the model symbol, and the symbol is
added to the current `gptel-backend' so that gptel does not reset
`gptel-model' on the next request.

Candidate selection is delegated to `completing-read', so completion
frontends such as vertico provide the UI automatically.  Descriptions
are not included in the candidate strings but passed through the
annotation-function, so formatting by marginalia etc. still works."
  (interactive)
  (when-let* ((model (gptel-openrouter-models--read
                      "Select OpenRouter model (%d): " prefix))
              (sym (gptel-openrouter-models--register model)))
    (gptel-openrouter-models--register-with-backend sym)
    (setq gptel-model sym)
    (message "gptel-model set to %s" sym)))

;;;###autoload
(defun gptel-openrouter-models-copy-name (&optional full prefix)
  "Pick a model from OpenRouter and copy its name to the kill ring.
By default the bare model name is copied, i.e. the part after the
\"owner/\" prefix is stripped (\"google/gemini-2.5-flash\" becomes
\"gemini-2.5-flash\").  This is what native gptel backends for Gemini,
Anthropic, OpenAI, etc. expect.

With a prefix argument (FULL non-nil), copy the full model ID instead.

PREFIX, when non-nil, narrows the search to model IDs matching it
\(e.g. \"anthropic/\")."
  (interactive "P")
  (when-let* ((model (gptel-openrouter-models--read
                      "Copy OpenRouter model name (%d): " prefix))
              (model-id (gptel-openrouter-models--id model)))
    (let ((name (if full model-id
                  (gptel-openrouter-models--bare-name model-id))))
      (kill-new name)
      (message "Copied to kill ring: %s" name))))

;;;###autoload
(defun gptel-openrouter-models-refresh ()
  "Discard the cached model list and fetch it again."
  (interactive)
  (setq gptel-openrouter-models--cache nil)
  (message "Fetched %d OpenRouter models"
           (length (gptel-openrouter-models-list nil t))))

(provide 'gptel-openrouter-models)
;;; gptel-openrouter-models.el ends here
