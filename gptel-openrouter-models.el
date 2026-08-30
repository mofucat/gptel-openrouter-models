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
;; friends hook into automatically), and sets `gptel-model' to it.
;;
;; No API key is needed to fetch the model list (OpenRouter's /models is
;; a public endpoint).  The appearance of candidates is left to the
;; completing-read frontend (vertico / marginalia etc.) for formatting;
;; descriptions are only passed through the annotation-function of
;; `completion-extra-properties'.
;;
;; Usage:
;;   M-x gptel-openrouter-models-pick
;;
;; Set up `gptel-backend' as a `gptel-make-openai' backend for
;; OpenRouter beforehand.  See README.md for details.

;;; Code:

(require 'url)
(require 'json)
(require 'gptel)

(defgroup gptel-openrouter-models nil
  "Pick OpenRouter models for gptel."
  :group 'gptel)

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

(defun gptel-openrouter-models--fetch-raw ()
  "Fetch OpenRouter's /models and return the data array (a list of alists)."
  (with-current-buffer (url-retrieve-synchronously
                         gptel-openrouter-models-endpoint
                         t t gptel-openrouter-models-timeout)
    (goto-char (point-min))
    (unless (search-forward "\n\n" nil t)
      (error "gptel-openrouter-models: could not find end of HTTP headers"))
    (let* ((json-object-type 'alist)
           (json-array-type 'list)
           (parsed (json-read)))
      (kill-buffer)
      (alist-get 'data parsed))))

(defun gptel-openrouter-models--id (model)
  "Extract the model ID from the MODEL alist."
  (alist-get 'id model))

(defun gptel-openrouter-models--description (model)
  "Extract the description from the MODEL alist, or nil if absent."
  (alist-get 'description model))

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
        (message "No models found")
      (let ((model-id (completing-read
                        (format "Select OpenRouter model (%d): " (length ids))
                        ids nil t)))
        (setq gptel-model (intern model-id))
        (message "gptel-model set to %s" model-id)))))

(provide 'gptel-openrouter-models)
;;; gptel-openrouter-models.el ends here
