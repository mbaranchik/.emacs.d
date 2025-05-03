;;; -*- lexical-binding: t -*-

;;(straight-use-package
;; '(EmacsAmazonLibs :type git
;;                   :host nil
;;                   :build t
;;                   :post-build (copy-file "lisp/brazil-path-cache-artifacts"
;;                                          (straight--build-dir "EmacsAmazonLibs"))
;;      :repo "ssh://git.amazon.com:2222/pkg/EmacsAmazonLibs"))

;;(setq my/amz-repo-location (format "%s/EmacsAmazonLibs" user-emacs-directory))

;; (use-package amz-common
;;     :straight `(amz-common
;;                    :local-repo ,my/amz-repo-location
;;                    :type nil
;;                    :build t
;;                    :files ("emacs-amazon-libs/amz-common.el")))

;; (use-package amz-mw
;;     :straight `(amz-mw
;;                    :local-repo ,my/amz-repo-location
;;                    :type nil
;;                    :build t
;;                    :files ("emacs-amazon-libs/amz-mw.el"))
;;     :after amz-common
;;     :demand t)

(provide 'my/amazon)
