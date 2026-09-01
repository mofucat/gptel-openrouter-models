;;; gptel-openrouter-models.el --- Pick OpenRouter models for gptel -*- lexical-binding: t; -*-

;; Author: mofucat
;; URL: https://github.com/mofucat/gptel-openrouter-models
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
;; Version: 0.1.0
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
;;
;; `gptel-openrouter-models-copy-name' picks a model the same way but,
;; instead of touching `gptel-model', copies just the bare model name
;; (the part after the "owner/" prefix, e.g. "gemini-2.5-flash" from
;; "google/gemini-2.5-flash") to the kill ring.  This is handy when you
;; want to paste the name into a native (non-OpenRouter) gptel backend
;; for Gemini, Anthropic, OpenAI, etc.  With a prefix argument it copies
;; the full model ID instead.
;;
;; Set up `gptel-backend' as a `gptel-make-openai' backend for
;; OpenRouter beforehand.  See README.md for details.

;;; Code:

(require 'url)
(require 'json)
(require 'seq)
(require 'subr-x)                        ; `when-let*' on Emacs 27/28
(require 'gptel)

(defgroup gptel-openrouter-models nil
  "Pick OpenRouter models for gptel."
  :group 'gptel)

(define-error 'gptel-openrouter-models-error
  "gptel-openrouter-models: could not fetch the model list")

(defcustom gptel-openrouter-models-endpoint "https://openrouter.ai/api/v1/models"
  "OpenRouter model-list API endpoint."
  :type 'string
  :group 'gptel-openrouter-models)

(defcustom gptel-openrouter-models-timeout 15
  "Timeout in seconds for fetching the model list."
  :type 'integer
  :group 'gptel-openrouter-models)

(defface gptel-openrouter-models-annotation-face
  '((t :inherit completions-annotations))
  "Face used for the model description shown next to each candidate.
Defaults to `completions-annotations' (typically dimmed/italic), so the
description is visually distinct from the model ID itself."
  :group 'gptel-openrouter-models)

(defun gptel-openrouter-models--parse-buffer ()
  "Parse the current buffer as an HTTP response from OpenRouter's /models.
Move point past the response headers, then read the JSON body and
return its `data' array (a list of alists).  Signal
`gptel-openrouter-models-error' if the end of the headers cannot be
located."
  (goto-char (point-min))
  (if (bound-and-true-p url-http-end-of-headers)
      (goto-char url-http-end-of-headers)
    ;; Fall back to finding the blank line between headers and body.
    ;; Accept both CRLF ("\r\n\r\n", per RFC) and bare-LF ("\n\n")
    ;; terminators.
    (unless (re-search-forward "\r?\n\r?\n" nil t)
      (signal 'gptel-openrouter-models-error
              (list "could not find end of HTTP headers"))))
  (let ((json-object-type 'alist)
        (json-array-type 'list))
    (alist-get 'data (json-read))))

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
  (if (string-match "\\`[^/]+/\\(.+\\)\\'" id)
      (match-string 1 id)
    id))

(defun gptel-openrouter-models-list (&optional prefix)
  "Fetch the OpenRouter model list and return it sorted by ID.
If PREFIX is non-nil, keep only model IDs matching that prefix
(e.g. \"anthropic/\")."
  (let* ((models (gptel-openrouter-models--fetch-raw))
         (filtered (if prefix
                       (seq-filter (lambda (m)
                                     (string-prefix-p
                                      prefix (gptel-openrouter-models--id m)))
                                   models)
                     models)))
    (sort filtered (lambda (a b)
                     (string< (gptel-openrouter-models--id a)
                              (gptel-openrouter-models--id b))))))

(defun gptel-openrouter-models--read (prompt &optional prefix)
  "Fetch the model list and `completing-read' one ID, using PROMPT.
PROMPT is passed through `format' with the candidate count as its only
argument.  If PREFIX is non-nil, only IDs matching that prefix are
offered (e.g. \"anthropic/\").  Descriptions are shown through the
completion annotation-function.  Return the selected ID string, or nil
when no models are available."
  (message "Fetching OpenRouter model list...")
  (let* ((models (gptel-openrouter-models-list prefix))
         (desc-table (make-hash-table :test 'equal))
         (ids (mapcar (lambda (m)
                        (let ((id (gptel-openrouter-models--id m)))
                          (puthash id (gptel-openrouter-models--description m)
                                   desc-table)
                          id))
                      models))
         (completion-extra-properties
          (list :annotation-function
                (lambda (id)
                  (let ((desc (gethash id desc-table)))
                    (when desc
                      (propertize (concat "  " desc)
                                  'face 'gptel-openrouter-models-annotation-face)))))))
    (if (null ids)
        (progn (message "No models found") nil)
      (completing-read (format prompt (length ids)) ids nil t))))

;;;###autoload
(defun gptel-openrouter-models-pick (&optional prefix)
  "Pick a model from OpenRouter and set `gptel-model' to it.
If PREFIX is given, narrow the search to models matching that prefix
(e.g. \"anthropic/\").  When called interactively, all models are
considered without any prefix filtering.

Candidate selection is delegated to `completing-read', so completion
frontends such as vertico provide the UI automatically.  Descriptions
are not included in the candidate strings but passed through the
annotation-function, so formatting by marginalia etc. still works."
  (interactive)
  (when-let* ((model-id (gptel-openrouter-models--read
                        "Select OpenRouter model (%d): " prefix)))
    (setq gptel-model (intern model-id))
    (message "gptel-model set to %s" model-id)))

;;;###autoload
(defun gptel-openrouter-models-copy-name (&optional full prefix)
  "Pick a model from OpenRouter and copy its name to the kill ring.
By default the bare model name is copied, i.e. the part after the
\"owner/\" prefix is stripped (\"google/gemini-2.5-flash\" becomes
\"gemini-2.5-flash\").  This is what native gptel backends for Gemini,
Anthropic, OpenAI, etc. expect.

With a prefix argument (FULL non-nil), copy the full model ID instead.

PREFIX, when non-nil, narrows the search to model IDs matching it
(e.g. \"anthropic/\")."
  (interactive "P")
  (when-let* ((model-id (gptel-openrouter-models--read
                        "Copy OpenRouter model name (%d): " prefix)))
    (let ((name (if full model-id
                  (gptel-openrouter-models--bare-name model-id))))
      (kill-new name)
      (message "Copied to kill ring: %s" name))))

(provide 'gptel-openrouter-models)
;;; gptel-openrouter-models.el ends here
