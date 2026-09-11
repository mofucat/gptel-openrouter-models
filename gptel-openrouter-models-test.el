;;; gptel-openrouter-models-test.el --- Tests for gptel-openrouter-models -*- lexical-binding: t; coding: utf-8; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT test suite.  Run with:
;;
;;   emacs -Q --batch -l gptel-openrouter-models-test.el -f ert-run-tests-batch-and-exit
;;
;; or simply `make test'.
;;
;; `gptel' is not a hard dependency of the tests: `gptel-stub.el'
;; provides a minimal stand-in when the real package is not on
;; `load-path', so the suite runs in a bare Emacs.  No network access is
;; used -- every test that needs a model list stubs
;; `gptel-openrouter-models--fetch-raw'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path dir))
(require 'gptel-stub)                   ; real gptel when available
(require 'gptel-openrouter-models)

;;; Fixtures

(defvar gptel-openrouter-models-test--sample
  '(((id . "openai/gpt-4o") (description . "GPT-4o omni model"))
    ((id . "anthropic/claude-3.5-sonnet") (description . "Claude 3.5 Sonnet"))
    ((id . "google/gemini-2.5-flash") (description . "Gemini 2.5 Flash"))
    ((id . "google/gemini-2.5-pro"))
    ((id . "meta-llama/llama-3.1-8b-instruct:free")
     (description . "Llama 3.1 8B")))
  "A representative slice of the OpenRouter /models `data' array.")

(defvar gptel-openrouter-models-test--full
  '((id . "anthropic/claude-sonnet-4.5")
    (description . "Claude Sonnet 4.5 is Anthropic's most advanced Sonnet model.")
    (context_length . 1000000)
    (architecture (modality . "text+image+file->text")
                  (input_modalities "text" "image" "file")
                  (output_modalities "text"))
    (pricing (prompt . "0.000003")
             (completion . "0.000015")
             (input_cache_read . "0.0000003"))
    (top_provider (context_length . 1000000)
                  (max_completion_tokens . 64000))
    (supported_parameters "tools" "response_format" "structured_outputs"
                          "max_tokens" "temperature"))
  "One full /models entry, shaped like OpenRouter's real payload.")

(defmacro gptel-openrouter-models-test--with-models (models &rest body)
  "Evaluate BODY with `gptel-openrouter-models--fetch-raw' returning MODELS.
The fetch cache is bound to nil so tests do not leak state into each
other."
  (declare (indent 1))
  `(let ((gptel-openrouter-models--cache nil))
     (cl-letf (((symbol-function 'gptel-openrouter-models--fetch-raw)
                (lambda () (copy-tree ,models))))
       ,@body)))

;;; --bare-name

(ert-deftest gptel-openrouter-models-test-bare-name-strips-owner ()
  (should (equal (gptel-openrouter-models--bare-name "google/gemini-2.5-flash")
                 "gemini-2.5-flash"))
  (should (equal (gptel-openrouter-models--bare-name "anthropic/claude-3.5-sonnet")
                 "claude-3.5-sonnet")))

(ert-deftest gptel-openrouter-models-test-bare-name-keeps-suffix-and-inner-slash ()
  (should (equal (gptel-openrouter-models--bare-name "openai/gpt-4o:extended")
                 "gpt-4o:extended"))
  ;; Only the first "owner/" segment is stripped.
  (should (equal (gptel-openrouter-models--bare-name "x-ai/grok-2/variant")
                 "grok-2/variant")))

(ert-deftest gptel-openrouter-models-test-bare-name-passthrough-without-slash ()
  (should (equal (gptel-openrouter-models--bare-name "gpt-4o") "gpt-4o"))
  (should (equal (gptel-openrouter-models--bare-name "") ""))
  ;; A malformed entry must not signal a raw wrong-type-argument.
  (should (null (gptel-openrouter-models--bare-name nil))))

;;; --id / --description

(ert-deftest gptel-openrouter-models-test-accessors ()
  (let ((m '((id . "google/gemini-2.5-flash") (description . "desc"))))
    (should (equal (gptel-openrouter-models--id m) "google/gemini-2.5-flash"))
    (should (equal (gptel-openrouter-models--description m) "desc")))
  (should (null (gptel-openrouter-models--description '((id . "x"))))))

;;; --json-parse

(ert-deftest gptel-openrouter-models-test-json-parse-shapes ()
  "Objects come back as alists, arrays as lists, false/null as nil."
  (let ((parsed (gptel-openrouter-models--json-parse
                 "{\"a\":[1,2],\"b\":{\"c\":\"d\"},\"e\":false,\"f\":null}")))
    (should (equal (alist-get 'a parsed) '(1 2)))
    (should (equal (alist-get 'c (alist-get 'b parsed)) "d"))
    (should (null (alist-get 'e parsed)))
    (should (null (alist-get 'f parsed)))))

(ert-deftest gptel-openrouter-models-test-json-parse-signals-on-garbage ()
  (should-error (gptel-openrouter-models--json-parse "not json at all")))

;;; --parse-buffer / --fetch-raw

(defvar gptel-openrouter-models-test--json-body
  "{\"data\":[{\"id\":\"openai/gpt-4o\",\"description\":\"omni\"},\
{\"id\":\"anthropic/claude-3.5-sonnet\"}]}"
  "A minimal OpenRouter /models JSON payload.")

(ert-deftest gptel-openrouter-models-test-parse-buffer-crlf-headers ()
  "RFC-style CRLF header terminator is handled."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"
            gptel-openrouter-models-test--json-body)
    (should (equal (mapcar (lambda (m) (alist-get 'id m))
                           (gptel-openrouter-models--parse-buffer))
                   '("openai/gpt-4o" "anthropic/claude-3.5-sonnet")))))

(ert-deftest gptel-openrouter-models-test-parse-buffer-lf-headers ()
  "Bare-LF header terminator still works."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n"
            gptel-openrouter-models-test--json-body)
    (should (equal (mapcar (lambda (m) (alist-get 'id m))
                           (gptel-openrouter-models--parse-buffer))
                   '("openai/gpt-4o" "anthropic/claude-3.5-sonnet")))))

(ert-deftest gptel-openrouter-models-test-parse-buffer-decodes-utf-8 ()
  "A unibyte response buffer (what url.el hands back) is decoded as UTF-8.
Without decoding, a right single quotation mark in a description comes
out as three Latin-1 characters."
  (let ((body (concat "{\"data\":[{\"id\":\"a/b\","
                      "\"description\":\"Amazon’s model\"}]}")))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (encode-coding-string
               (concat "HTTP/1.1 200 OK\r\n\r\n" body) 'utf-8))
      (defvar url-http-end-of-headers)
      (let ((url-http-end-of-headers nil))
        (should (equal (alist-get 'description
                                  (car (gptel-openrouter-models--parse-buffer)))
                       "Amazon’s model"))))))

(ert-deftest gptel-openrouter-models-test-parse-buffer-keeps-multibyte-text ()
  "An already-decoded (multibyte) buffer is not double-decoded."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n\r\n"
            "{\"data\":[{\"id\":\"a/b\",\"description\":\"Amazon’s\"}]}")
    (defvar url-http-end-of-headers)
    (let ((url-http-end-of-headers nil))
      (should (equal (alist-get 'description
                                (car (gptel-openrouter-models--parse-buffer)))
                     "Amazon’s")))))

(ert-deftest gptel-openrouter-models-test-parse-buffer-uses-url-http-marker ()
  "When `url-http-end-of-headers' is set, it takes precedence."
  (with-temp-buffer
    (insert "GARBAGE-NOT-HEADERS")
    (let ((marker (point-marker)))
      (insert gptel-openrouter-models-test--json-body)
      (defvar url-http-end-of-headers)
      (let ((url-http-end-of-headers marker))
        (should (equal (mapcar (lambda (m) (alist-get 'id m))
                               (gptel-openrouter-models--parse-buffer))
                       '("openai/gpt-4o" "anthropic/claude-3.5-sonnet")))))))

(ert-deftest gptel-openrouter-models-test-parse-buffer-missing-terminator ()
  "A response with no header terminator signals a clear error."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK just headers, no body separator")
    (defvar url-http-end-of-headers)
    (let ((url-http-end-of-headers nil))
      (should-error (gptel-openrouter-models--parse-buffer)
                    :type 'gptel-openrouter-models-error))))

(ert-deftest gptel-openrouter-models-test-parse-buffer-429-json-error ()
  "A 429 JSON error response surfaces the provider's message."
  (with-temp-buffer
    (insert "HTTP/1.1 429 Too Many Requests\r\n"
            "Content-Type: application/json\r\n\r\n"
            "{\"error\":{\"message\":\"Rate limit exceeded\",\"code\":429}}")
    (defvar url-http-end-of-headers)
    (let ((url-http-end-of-headers nil))
      (let ((err (should-error (gptel-openrouter-models--parse-buffer)
                               :type 'gptel-openrouter-models-error)))
        (should (string-match-p "HTTP 429" (error-message-string err)))
        (should (string-match-p "Rate limit exceeded"
                                (error-message-string err)))))))

(ert-deftest gptel-openrouter-models-test-parse-buffer-error-code-only ()
  "An error object with only a numeric code still yields a string detail."
  (with-temp-buffer
    (insert "HTTP/1.1 402 Payment Required\r\n\r\n"
            "{\"error\":{\"code\":402}}")
    (defvar url-http-end-of-headers)
    (let ((url-http-end-of-headers nil))
      (let ((err (should-error (gptel-openrouter-models--parse-buffer)
                               :type 'gptel-openrouter-models-error)))
        (should (string-match-p "HTTP 402" (error-message-string err)))))))

(ert-deftest gptel-openrouter-models-test-parse-buffer-5xx-non-json-body ()
  "A 5xx response with a non-JSON body still reports the status and a snippet."
  (with-temp-buffer
    (insert "HTTP/1.1 503 Service Unavailable\r\n\r\n"
            "<html><body>upstream down</body></html>")
    (defvar url-http-end-of-headers)
    (let ((url-http-end-of-headers nil))
      (let ((err (should-error (gptel-openrouter-models--parse-buffer)
                               :type 'gptel-openrouter-models-error)))
        (should (string-match-p "HTTP 503" (error-message-string err)))
        (should (string-match-p "upstream down" (error-message-string err)))))))

(ert-deftest gptel-openrouter-models-test-parse-buffer-uses-url-http-response-status ()
  "`url-http-response-status' is honored when the status line is not in the buffer."
  (with-temp-buffer
    (insert "\r\n\r\n{\"data\":[{\"id\":\"openai/gpt-4o\"}]}")
    (defvar url-http-end-of-headers)
    (defvar url-http-response-status)
    (let ((url-http-end-of-headers nil)
          (url-http-response-status 200))
      (should (equal (mapcar (lambda (m) (alist-get 'id m))
                             (gptel-openrouter-models--parse-buffer))
                     '("openai/gpt-4o"))))))

(ert-deftest gptel-openrouter-models-test-parse-buffer-non-json-2xx ()
  "A 2xx response whose body is not JSON signals a parse error."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n\r\nnot json at all")
    (defvar url-http-end-of-headers)
    (let ((url-http-end-of-headers nil))
      (should-error (gptel-openrouter-models--parse-buffer)
                    :type 'gptel-openrouter-models-error))))

(ert-deftest gptel-openrouter-models-test-fetch-raw-errors-on-nil-buffer ()
  "A nil return from `url-retrieve-synchronously' (timeout/failure) errors clearly."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _) nil)))
    (let ((err (should-error (gptel-openrouter-models--fetch-raw)
                             :type 'gptel-openrouter-models-error)))
      (should (string-match-p "timeout or connection failure"
                              (error-message-string err))))))

(ert-deftest gptel-openrouter-models-test-fetch-raw-wraps-signalled-errors ()
  "A signalled network error (e.g. DNS failure) becomes a package error."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _)
               (signal 'file-error
                       '("openrouter.ai/443" "nodename nor servname provided")))))
    (let ((err (should-error (gptel-openrouter-models--fetch-raw)
                             :type 'gptel-openrouter-models-error)))
      (should (string-match-p "nodename nor servname provided"
                              (error-message-string err))))))

;;; Caching

(ert-deftest gptel-openrouter-models-test-cache-avoids-refetch ()
  "A second call within the TTL reuses the cached list."
  (let ((calls 0)
        (gptel-openrouter-models--cache nil)
        (gptel-openrouter-models-cache-ttl 3600))
    (cl-letf (((symbol-function 'gptel-openrouter-models--fetch-raw)
               (lambda () (setq calls (1+ calls))
                 (copy-tree gptel-openrouter-models-test--sample))))
      (gptel-openrouter-models-list)
      (gptel-openrouter-models-list)
      (should (= calls 1))
      ;; ... but FORCE goes to the network again.
      (gptel-openrouter-models-list nil t)
      (should (= calls 2)))))

(ert-deftest gptel-openrouter-models-test-cache-ttl-nil-disables-caching ()
  (let ((calls 0)
        (gptel-openrouter-models--cache nil)
        (gptel-openrouter-models-cache-ttl nil))
    (cl-letf (((symbol-function 'gptel-openrouter-models--fetch-raw)
               (lambda () (setq calls (1+ calls))
                 (copy-tree gptel-openrouter-models-test--sample))))
      (gptel-openrouter-models-list)
      (gptel-openrouter-models-list)
      (should (= calls 2)))))

(ert-deftest gptel-openrouter-models-test-cache-expires ()
  "A cache older than the TTL is discarded."
  (let ((calls 0)
        (gptel-openrouter-models-cache-ttl 60)
        (gptel-openrouter-models--cache nil))
    (cl-letf (((symbol-function 'gptel-openrouter-models--fetch-raw)
               (lambda () (setq calls (1+ calls))
                 (copy-tree gptel-openrouter-models-test--sample))))
      (gptel-openrouter-models-list)
      (setcar gptel-openrouter-models--cache
              (time-subtract (current-time) 120))
      (gptel-openrouter-models-list)
      (should (= calls 2)))))

(ert-deftest gptel-openrouter-models-test-refresh-clears-cache ()
  (let ((calls 0)
        (gptel-openrouter-models-cache-ttl 3600)
        (gptel-openrouter-models--cache nil))
    (cl-letf (((symbol-function 'gptel-openrouter-models--fetch-raw)
               (lambda () (setq calls (1+ calls))
                 (copy-tree gptel-openrouter-models-test--sample))))
      (gptel-openrouter-models-list)
      (gptel-openrouter-models-refresh)
      (should (= calls 2)))))

;;; -list

(ert-deftest gptel-openrouter-models-test-list-sorted-by-id ()
  (gptel-openrouter-models-test--with-models gptel-openrouter-models-test--sample
    (should (equal (mapcar #'gptel-openrouter-models--id
                           (gptel-openrouter-models-list))
                   '("anthropic/claude-3.5-sonnet"
                     "google/gemini-2.5-flash"
                     "google/gemini-2.5-pro"
                     "meta-llama/llama-3.1-8b-instruct:free"
                     "openai/gpt-4o")))))

(ert-deftest gptel-openrouter-models-test-list-does-not-mutate-source ()
  "Sorting must not reorder the list `--fetch-raw' (or the cache) holds."
  (let* ((source (copy-tree gptel-openrouter-models-test--sample))
         (gptel-openrouter-models--cache nil))
    (cl-letf (((symbol-function 'gptel-openrouter-models--fetch-raw)
               (lambda () source)))
      (gptel-openrouter-models-list)
      (should (equal (mapcar #'gptel-openrouter-models--id source)
                     (mapcar #'gptel-openrouter-models--id
                             gptel-openrouter-models-test--sample))))))

(ert-deftest gptel-openrouter-models-test-list-prefix-filter ()
  (gptel-openrouter-models-test--with-models gptel-openrouter-models-test--sample
    (should (equal (mapcar #'gptel-openrouter-models--id
                           (gptel-openrouter-models-list "google/"))
                   '("google/gemini-2.5-flash" "google/gemini-2.5-pro")))
    (should (null (gptel-openrouter-models-list "no-such-owner/")))))

(ert-deftest gptel-openrouter-models-test-list-drops-entries-without-id ()
  "A malformed entry is skipped instead of signalling."
  (gptel-openrouter-models-test--with-models
      '(((name . "no id here")) ((id . "openai/gpt-4o")))
    (should (equal (mapcar #'gptel-openrouter-models--id
                           (gptel-openrouter-models-list))
                   '("openai/gpt-4o")))
    (should (equal (mapcar #'gptel-openrouter-models--id
                           (gptel-openrouter-models-list "openai/"))
                   '("openai/gpt-4o")))))

;;; Metadata extraction

(ert-deftest gptel-openrouter-models-test-mime-types-from-modalities ()
  (should (equal (gptel-openrouter-models--mime-types
                  gptel-openrouter-models-test--full)
                 '("image/jpeg" "image/png" "image/gif" "image/webp"
                   "application/pdf")))
  ;; Text-only models get no MIME types at all.
  (should (null (gptel-openrouter-models--mime-types
                 '((architecture (input_modalities "text")))))))

(ert-deftest gptel-openrouter-models-test-capabilities ()
  (should (equal (gptel-openrouter-models--capabilities
                  gptel-openrouter-models-test--full)
                 '(media tool-use json cache)))
  (should (null (gptel-openrouter-models--capabilities
                 '((architecture (input_modalities "text")))))))

(ert-deftest gptel-openrouter-models-test-context-window-in-thousands ()
  (should (= (gptel-openrouter-models--context-window
              gptel-openrouter-models-test--full)
             1000))
  ;; Falls back to the top-level context_length.
  (should (= (gptel-openrouter-models--context-window
              '((context_length . 128000)))
             128))
  (should (null (gptel-openrouter-models--context-window '((id . "a/b"))))))

(ert-deftest gptel-openrouter-models-test-price-per-million-tokens ()
  (should (= (gptel-openrouter-models--price
              gptel-openrouter-models-test--full 'prompt)
             3.0))
  (should (= (gptel-openrouter-models--price
              gptel-openrouter-models-test--full 'completion)
             15.0))
  ;; Free and missing prices are reported as 0, never as a negative number.
  (should (= (gptel-openrouter-models--price '((pricing (prompt . "0"))) 'prompt) 0))
  (should (= (gptel-openrouter-models--price '((pricing (prompt . "-1"))) 'prompt) 0))
  (should (= (gptel-openrouter-models--price '((id . "a/b")) 'prompt) 0)))

;;; --register

(ert-deftest gptel-openrouter-models-test-register-attaches-metadata ()
  "gptel reads model metadata off the symbol plist, so it must be set."
  (let ((sym (gptel-openrouter-models--register
              gptel-openrouter-models-test--full)))
    (should (eq sym 'anthropic/claude-sonnet-4.5))
    (should (equal (get sym :capabilities) '(media tool-use json cache)))
    (should (member "image/png" (get sym :mime-types)))
    (should (= (get sym :context-window) 1000))
    (should (= (get sym :input-cost) 3.0))
    (should (= (get sym :output-cost) 15.0))
    (should (string-prefix-p "Claude Sonnet 4.5" (get sym :description)))))

(ert-deftest gptel-openrouter-models-test-register-with-backend-adds-model ()
  "Picked models must end up in the backend list or gptel resets them."
  (let ((gptel-backend (gptel--make-backend :name "OpenRouter"
                                            :models '(openrouter/free))))
    (should (gptel-openrouter-models--register-with-backend 'a/b))
    (should (memq 'a/b (gptel-backend-models gptel-backend)))
    ;; The backend's original first model stays first, and re-picking the
    ;; same model does not add it twice.
    (should (eq (car (gptel-backend-models gptel-backend)) 'openrouter/free))
    (should (null (gptel-openrouter-models--register-with-backend 'a/b)))
    (should (= (length (gptel-backend-models gptel-backend)) 2))))

(ert-deftest gptel-openrouter-models-test-register-with-backend-tolerates-no-backend ()
  (let ((gptel-backend nil))
    (should (null (gptel-openrouter-models--register-with-backend 'a/b)))))

;;; --read

(ert-deftest gptel-openrouter-models-test-read-passes-count-and-candidates ()
  (gptel-openrouter-models-test--with-models gptel-openrouter-models-test--sample
    (let (seen-prompt seen-collection)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt collection &rest _)
                   (setq seen-prompt prompt seen-collection collection)
                   "google/gemini-2.5-flash")))
        ;; `--read' returns the whole model alist, not just the ID.
        (should (equal (gptel-openrouter-models--id
                        (gptel-openrouter-models--read "Pick (%d): "))
                       "google/gemini-2.5-flash"))
        (should (equal seen-prompt "Pick (5): "))
        (should (equal seen-collection
                       '("anthropic/claude-3.5-sonnet"
                         "google/gemini-2.5-flash"
                         "google/gemini-2.5-pro"
                         "meta-llama/llama-3.1-8b-instruct:free"
                         "openai/gpt-4o")))))))

(ert-deftest gptel-openrouter-models-test-read-prefix-narrows ()
  (gptel-openrouter-models-test--with-models gptel-openrouter-models-test--sample
    (let (seen-prompt)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _) (setq seen-prompt prompt) "google/gemini-2.5-pro")))
        (gptel-openrouter-models--read "Pick (%d): " "google/")
        (should (equal seen-prompt "Pick (2): "))))))

(ert-deftest gptel-openrouter-models-test-read-annotation-function ()
  (gptel-openrouter-models-test--with-models gptel-openrouter-models-test--sample
    (let (annotate)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _)
                   (setq annotate (plist-get completion-extra-properties
                                             :annotation-function))
                   "openai/gpt-4o")))
        (gptel-openrouter-models--read "Pick (%d): ")
        (should (functionp annotate))
        (should (string-match-p "GPT-4o omni model"
                                (funcall annotate "openai/gpt-4o")))
        ;; No description -> no annotation.
        (should (null (funcall annotate "google/gemini-2.5-pro")))))))

(ert-deftest gptel-openrouter-models-test-read-returns-nil-when-empty ()
  (gptel-openrouter-models-test--with-models nil
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (error "completing-read must not be called"))))
      (should (null (gptel-openrouter-models--read "Pick (%d): "))))))

;;; -pick

(ert-deftest gptel-openrouter-models-test-pick-sets-gptel-model ()
  (cl-letf (((symbol-function 'gptel-openrouter-models--read)
             (lambda (&rest _) gptel-openrouter-models-test--full)))
    (let ((gptel-model 'placeholder)
          (gptel-backend (gptel--make-backend :name "OpenRouter"
                                              :models '(openrouter/free))))
      (gptel-openrouter-models-pick)
      (should (eq gptel-model 'anthropic/claude-sonnet-4.5))
      ;; ... and the model survives gptel's `gptel--sanitize-model'.
      (should (memq gptel-model (gptel-backend-models gptel-backend)))
      (should (equal (get gptel-model :capabilities)
                     '(media tool-use json cache))))))

(ert-deftest gptel-openrouter-models-test-pick-noop-when-read-returns-nil ()
  (cl-letf (((symbol-function 'gptel-openrouter-models--read)
             (lambda (&rest _) nil)))
    (let ((gptel-model 'placeholder)
          (gptel-backend (gptel--make-backend :name "OpenRouter"
                                              :models '(openrouter/free))))
      (gptel-openrouter-models-pick)
      (should (eq gptel-model 'placeholder))
      (should (equal (gptel-backend-models gptel-backend) '(openrouter/free))))))

;;; -copy-name

(ert-deftest gptel-openrouter-models-test-copy-name-copies-bare-name ()
  (cl-letf (((symbol-function 'gptel-openrouter-models--read)
             (lambda (&rest _) '((id . "google/gemini-2.5-flash")))))
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (gptel-openrouter-models-copy-name)
      (should (equal (current-kill 0) "gemini-2.5-flash")))))

(ert-deftest gptel-openrouter-models-test-copy-name-full-with-prefix-arg ()
  (cl-letf (((symbol-function 'gptel-openrouter-models--read)
             (lambda (&rest _) '((id . "google/gemini-2.5-flash")))))
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (gptel-openrouter-models-copy-name t)
      (should (equal (current-kill 0) "google/gemini-2.5-flash")))))

(ert-deftest gptel-openrouter-models-test-copy-name-does-not-touch-gptel-model ()
  (cl-letf (((symbol-function 'gptel-openrouter-models--read)
             (lambda (&rest _) '((id . "google/gemini-2.5-flash")))))
    (let ((kill-ring nil) (kill-ring-yank-pointer nil)
          (gptel-model 'placeholder))
      (gptel-openrouter-models-copy-name)
      (should (eq gptel-model 'placeholder)))))

(ert-deftest gptel-openrouter-models-test-copy-name-noop-when-read-returns-nil ()
  (cl-letf (((symbol-function 'gptel-openrouter-models--read)
             (lambda (&rest _) nil)))
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (gptel-openrouter-models-copy-name)
      (should (null kill-ring)))))

(provide 'gptel-openrouter-models-test)
;;; gptel-openrouter-models-test.el ends here
