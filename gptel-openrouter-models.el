;;; gptel-openrouter-models.el --- Pick OpenRouter models for gptel -*- lexical-binding: t; -*-

;; Author: mofucat
;; URL: https://github.com/mofucat/gptel-openrouter-models
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
;; Version: 0.1.0
;; Keywords: convenience, tools
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; OpenRouter の `/api/v1/models' を取得し、`completing-read'
;; (vertico 等が自動でフックする標準UI)でモデルを選んで
;; `gptel-model' に設定するための小さなパッケージ。
;;
;; モデル一覧の取得に API キーは不要(OpenRouter の /models は
;; 公開エンドポイント)。候補の見た目は completing-read の
;; フロントエンド(vertico / marginalia 等)の整形にまかせる設計で、
;; 説明文は `completion-extra-properties' の annotation-function
;; 経由で渡すのみ。
;;
;; 使い方:
;;   M-x gptel-openrouter-models-pick
;;
;; 事前に `gptel-backend' を OpenRouter 用の `gptel-make-openai'
;; バックエンドにしておくこと。詳細は README.md を参照。

;;; Code:

(require 'url)
(require 'json)
(require 'gptel)

(defgroup gptel-openrouter-models nil
  "Pick OpenRouter models for gptel."
  :group 'gptel)

(defcustom gptel-openrouter-models-endpoint "https://openrouter.ai/api/v1/models"
  "OpenRouter のモデル一覧APIエンドポイント。"
  :type 'string
  :group 'gptel-openrouter-models)

(defcustom gptel-openrouter-models-timeout 15
  "モデル一覧取得のタイムアウト秒数。"
  :type 'integer
  :group 'gptel-openrouter-models)

(defface gptel-openrouter-models-annotation-face
  '((t :inherit completions-annotations))
  "Face used for the model description shown next to each candidate.
Defaults to `completions-annotations' (typically dimmed/italic), so the
description is visually distinct from the model ID itself."
  :group 'gptel-openrouter-models)

(defun gptel-openrouter-models--fetch-raw ()
  "OpenRouter の /models を取得し、data 配列(alistのリスト)を返す。"
  (with-current-buffer (url-retrieve-synchronously
                         gptel-openrouter-models-endpoint
                         t t gptel-openrouter-models-timeout)
    (goto-char (point-min))
    (unless (search-forward "\n\n" nil t)
      (error "gptel-openrouter-models: HTTPヘッダの終端が見つかりません"))
    (let* ((json-object-type 'alist)
           (json-array-type 'list)
           (parsed (json-read)))
      (kill-buffer)
      (alist-get 'data parsed))))

(defun gptel-openrouter-models--id (model)
  "MODEL alist からモデルIDを取り出す。"
  (alist-get 'id model))

(defun gptel-openrouter-models--description (model)
  "MODEL alist から説明文を取り出す。無ければ nil。"
  (alist-get 'description model))

(defun gptel-openrouter-models-list (&optional prefix)
  "OpenRouter のモデル一覧を取得し、ID順にソートして返す。
PREFIX が非nilなら、そのプレフィックスに一致するモデルIDのみに絞る
(例: \"anthropic/\")。"
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
  "OpenRouter のモデルから選んで `gptel-model' に設定する。
PREFIX を渡すと、そのプレフィックスに一致するモデルだけに絞って検索する
(例えば \"anthropic/\" など)。対話的に呼んだ場合はプレフィックス絞り込み
なしで全モデルを対象にする。

候補の選択自体は `completing-read' に委ねているので、vertico などの
補完フロントエンドが自動でUIを提供する。説明文は候補文字列に含めず
annotation-function 経由で渡すため、marginalia 等の整形もそのまま効く。"
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
