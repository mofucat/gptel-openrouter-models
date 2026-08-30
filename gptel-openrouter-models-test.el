;;; gptel-openrouter-models-test.el --- Tests for gptel-openrouter-models -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT test suite.  Run with:
;;
;;   emacs -Q --batch -l gptel-openrouter-models-test.el -f ert-run-tests-batch-and-exit
;;
;; or simply `make test'.
;;
;; `gptel' is not a hard dependency of the tests: a minimal stub is
;; provided when the real package is not on `load-path', so the suite
;; runs in a bare Emacs.  No network access is used -- every test that
;; needs a model list stubs `gptel-openrouter-models--fetch-raw'.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Provide a stub for `gptel' if the real package is unavailable, so the
;; library's top-level `(require 'gptel)' succeeds.
(unless (require 'gptel nil t)
  (defvar gptel-model 'placeholder)
  (provide 'gptel))

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path dir))
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

(defmacro gptel-openrouter-models-test--with-models (models &rest body)
  "Evaluate BODY with `gptel-openrouter-models--fetch-raw' returning MODELS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'gptel-openrouter-models--fetch-raw)
              (lambda () ,models)))
     ,@body))

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
  (should (equal (gptel-openrouter-models--bare-name "") "")))

;;; --id / --description

(ert-deftest gptel-openrouter-models-test-accessors ()
  (let ((m '((id . "google/gemini-2.5-flash") (description . "desc"))))
    (should (equal (gptel-openrouter-models--id m) "google/gemini-2.5-flash"))
    (should (equal (gptel-openrouter-models--description m) "desc")))
  (should (null (gptel-openrouter-models--description '((id . "x"))))))

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

(ert-deftest gptel-openrouter-models-test-list-prefix-filter ()
  (gptel-openrouter-models-test--with-models gptel-openrouter-models-test--sample
    (should (equal (mapcar #'gptel-openrouter-models--id
                           (gptel-openrouter-models-list "google/"))
                   '("google/gemini-2.5-flash" "google/gemini-2.5-pro")))
    (should (null (gptel-openrouter-models-list "no-such-owner/")))))

;;; --read

(ert-deftest gptel-openrouter-models-test-read-passes-count-and-candidates ()
  (gptel-openrouter-models-test--with-models gptel-openrouter-models-test--sample
    (let (seen-prompt seen-collection)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt collection &rest _)
                   (setq seen-prompt prompt seen-collection collection)
                   "google/gemini-2.5-flash")))
        (should (equal (gptel-openrouter-models--read "Pick (%d): ")
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
             (lambda (&rest _) "google/gemini-2.5-flash")))
    (let ((gptel-model 'placeholder))
      (gptel-openrouter-models-pick)
      (should (eq gptel-model 'google/gemini-2.5-flash)))))

(ert-deftest gptel-openrouter-models-test-pick-noop-when-read-returns-nil ()
  (cl-letf (((symbol-function 'gptel-openrouter-models--read)
             (lambda (&rest _) nil)))
    (let ((gptel-model 'placeholder))
      (gptel-openrouter-models-pick)
      (should (eq gptel-model 'placeholder)))))

;;; -copy-name

(ert-deftest gptel-openrouter-models-test-copy-name-copies-bare-name ()
  (cl-letf (((symbol-function 'gptel-openrouter-models--read)
             (lambda (&rest _) "google/gemini-2.5-flash")))
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (gptel-openrouter-models-copy-name)
      (should (equal (current-kill 0) "gemini-2.5-flash")))))

(ert-deftest gptel-openrouter-models-test-copy-name-full-with-prefix-arg ()
  (cl-letf (((symbol-function 'gptel-openrouter-models--read)
             (lambda (&rest _) "google/gemini-2.5-flash")))
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (gptel-openrouter-models-copy-name t)
      (should (equal (current-kill 0) "google/gemini-2.5-flash")))))

(ert-deftest gptel-openrouter-models-test-copy-name-noop-when-read-returns-nil ()
  (cl-letf (((symbol-function 'gptel-openrouter-models--read)
             (lambda (&rest _) nil)))
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (gptel-openrouter-models-copy-name)
      (should (null kill-ring)))))

(provide 'gptel-openrouter-models-test)
;;; gptel-openrouter-models-test.el ends here
