(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

; (add-to-list
; 'package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;                    ("org" . "http://orgmode.org/elpa/")
;                    ("melpa" . "http://melpa.org/packages/")
;                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
; package-archive-priorities '(("melpa" . 1)))

(package-initialize)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(require 'use-package)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))
;; (global-set-key (kbd "C-x g") 'magit-status)

  
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (progn
    (when (load "flycheck" nil t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode))))

;; don't know how to enable flycheck-mode with above method, so enable right now
;; (elpy-enable)


;; retired use Metals
;; (use-package ensime
;; 	     :ensure t
;; 	     :pin melpa-stable)

(use-package afternoon-theme
  :ensure t
  :config
  (load-theme 'afternoon t))

;; (setq inferior-lisp-program "sbcl")

;; (use-package rust-mode
;;   :ensure t
;;   :init
;;   (setq rust-format-on-save t)
;;   ;; :hook
;;   ;; ((rust-mode . racer-mode)
;;   ;;  (racer-mode . eldoc-mode)
;;   ;;  (racer-mode . company-mode)
;;   ;;  )
;;   :mode ("\\.rs\\'" ))

(use-package cmake-mode
  :ensure t)
(use-package cmake-font-lock
  :ensure t
  :after (cmake-mode))

(use-package flycheck
  :ensure t)

;; require install gopls: go get golang.org/x/tools/gopls@latest
;; and setup GOPATH and PATH:
;; export GOPATH=$(go env GOPATH)
;; export PATH=$GOPATH/bin:$PATH
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  (go-mode . lsp-deferred)
  ;; (typescript-mode . lsp-deferred)
  )

(use-package typescript-mode
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package company-lsp
  :ensure t
  :commands company-lsp)
;; lsp-ui-doc will block some
;; lsp-ui is annoying
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-enable nil))

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package rustic
  :ensure t
  :init
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-format-on-save t)
  ;; (setq rustic-lsp-format t)
  :hook
  (('rustic-mode . 'yas-minor-mode))
  ;; :bind (("C-c C-b" . 'rustic-cargo-build)
  ;;        ("C-c C-k" . 'rustic-cargo-clean)
  ;;        ("C-c C-r" . 'rustic-cargo-run)
  ;;        ("C-c C-f" . 'rustic-format-buffer)
  ;;        ("C-c C-e" . 'lsp-rust-analyzer-expand-macro))
  )

;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :ensure t
  :mode ("\\.go$")
  :init
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  :hook
  (('go-mode . 'yas-minor-mode)))

(use-package graphql-mode
  :ensure t)

(use-package mmm-mode
  :ensure t
  :after (graphql-mode) 
  :config
  ;; https://emacs.stackexchange.com/questions/37918/how-to-highlight-graphql-template-literals-gql-in-jsx-files
  (mmm-add-classes '((js-graphql
                      :submode graphql-mode
                      :face mmm-declaration-submode-face
                      :front "[^a-zA-Z]gql`[\n\r]*"
                      :back "`$")))
  (mmm-add-mode-ext-class 'js-mode nil 'js-graphql)
  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 0)
  )

(use-package vue-mode
  :ensure t
  :mode ("\\.vue$" )
  ;; fix for https://github.com/AdamNiederer/vue-mode/issues/74#issuecomment-528560608
  :config
  (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil))))

(global-set-key (kbd "C-x g") 'magit-status)

(global-display-line-numbers-mode)

;; (require 'django-mode)

(use-package django-mode
  :ensure t)

;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; (setq web-mode-engines-alist
;;       '(("django" . "\\.html\\'")))
;; (add-hook 'web-mode-hook
;;           (lambda() (local-set-key (kbd "C-c /") #'web-mode-element-close)))
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"
         "\\.djhtml\\'")
  :init
  (setq web-mode-engines-alist
        '(("django" . "\\.html\\'")))
  (add-hook 'web-mode-hook
            (lambda() (local-set-key (kbd "C-c /") #'web-mode-element-close)))
  )


(use-package julia-mode
  :ensure t
  :mode ("\\.jl\\'")
  )
(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode) 
  )

(use-package lispy
  :ensure t
  :hook
  (lisp-mode . lispy-mode)
  (emacs-lisp-mode . lispy-mode))

(use-package slime
  :ensure t
  :init 
  (setq inferior-lisp-program "sbcl")
  ;; may need to check existent of slime-helper
  (let ((slime-helper-file "~/quicklisp/slime-helper.el"))
    (if (file-exists-p slime-helper-file)
        (load (expand-file-name slime-helper-file))))


  :config
  (add-to-list 'slime-contribs 'slime-fancy)
  :hook
  (lisp-mode . prettify-symbols-mode)
  (lisp-mode . company-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'"
         "\\.yml'\\'"))

;; need to install grip
(use-package grip-mode
  :ensure t
  :mode ("\\.md\\'"))

(use-package realgud
  :ensure t)

(use-package realgud-lldb
  :ensure t)
;; (add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))
;; (require 'julia-repl)
;; (add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode


(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; (add-hook 'elpy-mode-hook  ;; C-c C-/ is interpreted in emacs as C-c C-_
;;           (lambda() (local-set-key (kbd "C-c C-_") #'comment-or-uncomment-region)))

(add-hook 'lisp-mode-hook
	      (lambda () (show-paren-mode 1)))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (show-paren-mode 1)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :config
  (defun my-tide-setup-hook ()
    (tide-setup)
    (tide-hl-identifier-mode +1)
    (el-mode)
    (flycheck-mode)

    (prettier-js-mode)

    (set (make-local-variable 'company-backends)
         '((company-tide company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (setq typescript-indent-level 2)
  (flycheck-add-next-checker 'typescript-tide
                             'typescript-tslint)
  (setq tide-completion-detailed t)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package dockerfile-mode
  :ensure t)
(use-package docker-compose-mode
  :ensure t)

;; (setq jedi:environment-root "jedi")
;; (setq jedi:environment-virtualenv
;;       (append python-environment-virtualenv
;;               '("--python" "/usr/local/bin/python3")))

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'django-mode 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;; using local version
;; (add-to-list 'load-path "~/.emacs.d/mode/rust-mode")
;; (autoload 'rust-mode "rust-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))


;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)
;; (require 'rust-mode)

;; (add-to-list `load-path "~/.emacs.d/mode/vue-mode")
;; (autoload 'vue-mode "vue-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.vue$" . vue-mode))

(add-to-list `load-path "~/.emacs.d/mode")
(autoload 'verilog-mode "verilog-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vl?$" . verilog-mode))

;; (add-to-list `load-path "~/.emacs.d/mode/bb-mode")
;; (autoload 'bb-mode "bb-mode" nil t)
;; (require 'bb-mode)
;; (add-to-list 'auto-mode-alist '("\\.bb$" . bb-mode))
;; (add-to-list 'auto-mode-alist '("\\.inc$" . bb-mode))
;; (add-to-list 'auto-mode-alist '("\\.bbappend$" . bb-mode))
;; (add-to-list 'auto-mode-alist '("\\.bbclass$" . bb-mode))
;; (add-to-list 'auto-mode-alist '("\\.conf$" . bb-mode))

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt"))
