;;; gptel-stub.el --- Minimal gptel stub for CI -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; gptel-openrouter-models needs `gptel' at load time, but CI (and a bare
;; Emacs) does not have it installed.  This file provides just enough of
;; gptel's interface -- the `gptel-model' and `gptel-backend' variables
;; and the `gptel-backend' struct -- for byte-compilation and the test
;; suite to run.  It is a no-op when the real gptel is available.
;;
;; This file is only used by `make compile' and by the test suite; it is
;; not part of the package.

;;; Code:

(require 'cl-lib)

(unless (require 'gptel nil t)
  (defvar gptel-model nil)
  (defvar gptel-backend nil)
  (cl-defstruct (gptel-backend (:constructor gptel--make-backend)
                               (:copier gptel--copy-backend))
    name host header protocol stream
    endpoint key models url request-params
    curl-args coding-system)
  (provide 'gptel))

(provide 'gptel-stub)
;;; gptel-stub.el ends here
