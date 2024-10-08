(require 'package)
(setq byte-compile-warnings '(cl-functions))

;; need to updateGPG keys used by ELPA package manager
;; refer to: https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
;; Mx package-install RET gnu-elpa-keyring-update
;; or if keys are already too old
;; gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40

;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (proto (if no-ssl "http" "https")))
;;   (setq package-archives nil)
;;   ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
;;   (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;   ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;   (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))
;;   )

(setq
 package-archives '(
                    ;; ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                    ;; ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                    ;; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                    ;; ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
                    ("gnu" . "https://elpa.gnu.org/packages/")
                    ("org" . "https://orgmode.org/elpa/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/")
                    ;; ("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")
                    ))

;(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;			 ("org" . "https://orgmode.org/elpa/")
;			 ("melpa" . "https://melpa.org/packages/")
;			 ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(require 'use-package)

(use-package quelpa
  :ensure t
  :init
  (setq quelpa-update-melpa-p nil))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"
   ))
(require 'quelpa-use-package)

;; elixir related conf from:
;; https://medium.com/@victor.nascimento/elixir-emacs-development-2023-edition-1a6ccc40629
;; not well working
(use-package emacs
  :ensure nil
  :custom

  (major-mode-remap-alist
   '((elixir-mode . elixir-ts-mode))))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (heex "https://github.com/phoenixframework/tree-sitter-heex")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")))

(use-package eglot
  :ensure nil
  :config (add-to-list 'eglot-server-programs `((elixir-ts-mode heex-ts-mode) "/home/hongbo/.local/elixir-ls/language_server.sh")))

(use-package
 elixir-ts-mode
 :hook (elixir-ts-mode . eglot-ensure)
 (elixir-ts-mode
  .
  (lambda ()
    (push '(">=" . ?\u2265) prettify-symbols-alist)
    (push '("<=" . ?\u2264) prettify-symbols-alist)
    (push '("!=" . ?\u2260) prettify-symbols-alist)
    (push '("==" . ?\u2A75) prettify-symbols-alist)
    (push '("=~" . ?\u2245) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("->" . ?\u2192) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("|>" . ?\u25B7) prettify-symbols-alist)))
 (before-save . eglot-format))

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

;; (global-set-key (kbd "C-x g") 'magit-status)

;; setup project .dir-locals.el in Windows like following
;; ((python-mode . ((conda-anaconda-home . "h:/anaconda3")
;;                  (conda-env-home-directory . "h:/anaconda3")
;;                  (conda-project-env-name . "bvh")
;;                  (flycheck-python-flake8-executable . "python")
;;                  (flycheck-python-pycompile-executable . "python")
;;                  (flycheck-python-pylint-executable . "python")))
;; )

;; (use-package conda
;;   :ensure t
;;   :init
;;   (conda-env-initialize-interactive-shells)
;;   (conda-env-initialize-eshell)
;;   (conda-env-autoactivate-mode t)
;;   )
  
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

(use-package cmake-mode
  :ensure t)
(use-package cmake-font-lock
  :ensure t
  :after (cmake-mode))

(use-package dts-mode
  :ensure t)

(use-package flycheck
  :ensure t)

;; require install gopls: go get golang.org/x/tools/gopls@latest
;; and setup GOPATH and PATH:
;; export GOPATH=$(go env GOPATH)
;; export PATH=$GOPATH/bin:$PATH
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor\\'")
  (add-to-list 'lsp-file-watch-ignored-directories  "[/\\\\]\\.yarn\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]target\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]tests\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]ci\\'")
  (with-eval-after-load "rustic-mode"
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection
                       (lambda ()
                         `(,(or (executable-find
                                 (cl-first lsp-rust-analyzer-server-command))
                                (lsp-package-path 'rust-analyzer)
                                "rust-analyzer")
                           ,@(cl-rest lsp-rust-analyzer-server-args))))
      :remote? t
      :major-modes '(rust-mode rustic-mode)
      :initialization-options 'lsp-rust-analyzer--make-init-options
      :notification-handlers (ht<-alist lsp-rust-notification-handlers)
      :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
      :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
      :after-open-fn (lambda ()
                       (when lsp-rust-analyzer-server-display-inlay-hints
                         (lsp-rust-analyzer-inlay-hints-mode)))
      :ignore-messages nil
      :server-id 'rust-analyzer-remote)))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "/usr/local/bin/erlang_ls")
                    :major-modes '(erlang-mode)
                    :remote? t
                    :server-id 'erlang-remote))
  :hook
  (go-mode . lsp-deferred)
  (julia-mode . lsp-deferred)
  ;; npm i -g typescript-language-server
  (typescript-mode . lsp-deferred)
  ;; yarn add -D eslint eslint-plugin-vue@next
  ;; yarn eslint --init
  ;; yarn add -D eslint-plugin-vue@latest @typescript-eslint/eslint-plugin@latest @typescript-eslint/parser@latest
  ;; still not working with vue-language-server
  ;; (vue-mode . lsp-deferred)
  (erlang-mode . lsp-deferred)
  (dart-mode . lsp-deferred)
  (rust-mode . lsp-deferred)
  )

(use-package typescript-mode
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; company-lsp has been removed, use lsp-mode instead
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)

;; lsp-ui-doc will block some
;; lsp-ui is annoying
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-enable nil))

(use-package ccls
  :ensure t
  :hook
  (((c-mode c++-mode objc-mode cuda-mode) .
    (lambda () (require 'ccls) (lsp-deferred)))
   ((c-mode c++-mode objc-mode cuda-mode) . yas-minor-mode)))

;; install rust-analyser
;; rustup component add rust-src
;; build from source
;; git clone https://github.com/rust-analyzer/rust-analyzer.git
;; cd rust-analyzer
;; cargo xtask install

(use-package rust-mode
  :ensure t
  :init
  (setq rust-format-on-save t)
  ;; :hook
  ;; ((rust-mode . racer-mode)
  ;;  (racer-mode . eldoc-mode)
  ;;  (racer-mode . company-mode)
  ;;  )
  :mode ("\\.rs\\'" ))

;; (use-package rustic
;;   :ensure t
;;   :init
;;   ;; (setq rustic-lsp-server 'rust-analyzer) ; this seems conflicting with rustic-lsp-format ?
;;   (setq rustic-format-on-save t)
;;   (setq rustic-lsp-format t)
;;   :hook
;;   (('rustic-mode . 'yas-minor-mode))
;;   ;; :bind (("C-c C-b" . 'rustic-cargo-build)
;;   ;;        ("C-c C-k" . 'rustic-cargo-clean)
;;   ;;        ("C-c C-r" . 'rustic-cargo-run)
;;   ;;        ("C-c C-f" . 'rustic-format-buffer)
;;   ;;        ("C-c C-e" . 'lsp-rust-analyzer-expand-macro))
;;   )

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

(use-package kconfig-mode
  :ensure t)

(use-package mmm-mode
  :ensure t
  :init
  (setq mmm-global-mode 'maybe)
  :config
  ;; https://vxlabs.com/2014/04/08/syntax-highlighting-markdown-fenced-code-blocks-in-emacs/
  ;; https://emacs.stackexchange.com/questions/37918/how-to-highlight-graphql-template-literals-gql-in-jsx-files
  (mmm-add-classes '((graphql-tag
                      :submode graphql-mode
                      :face mmm-declaration-submode-face
                      :front "[^a-zA-Z]gql`"
                      :back "`")))
  (mmm-add-mode-ext-class 'js-mode nil 'graphql-tag)
  (mmm-add-mode-ext-class 'vue-mode nil 'graphql-tag))

;; npm i -g prettier
(use-package prettier
  :ensure t)

(use-package pug-mode
  :ensure t)

(use-package vue-mode
  :ensure t
  :mode ("\\.vue$" )
  ;; fix for https://github.com/AdamNiederer/vue-mode/issues/74#issuecomment-528560608
  ;; :config
  ;; (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  ;; (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  ;; (add-hook 'js-mode-hook (lambda () (setq syntax-ppss-table nil)))
  ;; (add-hook 'mmm-mode-hook (lambda () (set-face-background 'mmm-default-submode-face nil)))
  )
;; (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;; (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;; (add-hook 'vue-mode-hook (lambda () (setq syntax-ppss-table nil)))

;;  not fixed issue in vue-mode
;; use prettier to format code by prettier-js on save
(use-package prettier
  :ensure t)
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
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  )

;; as creator of julia-repl commented (https://github.com/gcv/julia-snail/issues/1#issuecomment-597038660)
;; julia-repl shall be replaced by something   
;; Julia-snail usage is not tested yet
(use-package vterm
  :ensure t)
(use-package julia-snail
  :ensure t
  :hook
  (julia-mode . julia-snail-mode))


(use-package lsp-julia
  :quelpa (lsp-julia
           ;; :fetcher github
           ;; :repo "non-Jedi/lsp-julia"
           ;; :files (:defaults "languageserver")
           )
  ;; :init
  ;; (setq lsp-julia-package-dir nil)
  )

;; ref: https://tamaspapp.eu/post/emacs-julia-customizations/
(defun customize-julia-mode ()
  "Customize julia-mode"
  (interactive)
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|QUESTION\\|NOTE\\)"
                             1 font-lock-warning-face t)))
  )

(use-package julia-mode
  :ensure t
  :mode ("\\.jl\\'")
  :hook
  (julia-mode . customize-julia-mode))

;; (use-package julia-repl
;;   :ensure t
;;   :hook (julia-mode . julia-repl-mode) 
;;   )

(use-package tree-sitter
  :ensure t)
;; (use-package tsi
;;   :ensure t)
(use-package origami
  :ensure t)
;; (use-package lsp-origami
;;   :ensure t
;;   :quelpa (lsp-origami :fetcher github :repo "emacs-lsp/lsp-origami"))
(use-package lsp-origami
  :ensure t)
;; (use-package buttercup
;;   :ensure t)
(use-package tsi
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el"))
;; (use-package tsx-mode
;;   :quelpa (tsx-mode :fetcher github :repo "orzechowskid/tsx-mode.el"))

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

;; Make a keybinding: `C-c C-c g'
(define-key markdown-mode-command-map (kbd "g") #'grip-mode)

;; need to install grip `pip3 install grip`
(use-package grip-mode
  :ensure t
  ;; :hook ((markdown-mode org-mode) . grip-mode)
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))) 

; (use-package realgud
;   :ensure t)
; 
; (use-package realgud-lldb
;   :after (realgud)
;   :ensure t)
; ;; (add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))
; ;; (require 'julia-repl)
; ;; (add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode


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
(use-package protobuf-mode
  :ensure t)

(use-package ack
  :ensure t)

(use-package bitbake
  :ensure t)

(use-package php-mode
  :ensure t)

(use-package erlang
  :hook
  (;; (erlang-mode . linum-mode)
   (erlang-mode . column-number-mode)
   )
  :ensure t
  :mode (("\\.erl?$" . erlang-mode)
         ("rebar\\.config$" . erlang-mode)
         ("relx\\.config$" . erlang-mode)
         ("sys\\.config\\.src$" . erlang-mode)
         ("\\.config\\.src?$" . erlang-mode)
         ("\\.config.script?$" . erlang-mode)
         ("\\.hrl?$" . erlang-mode)
         ("\\.app?$" . erlang-mode)
         ("\\.app.src?$" . erlang-mode)
         ("\\Emakefile" . erlang-mode)
         )
  )

;; (use-package elixir-ts-mode
;;   :ensure t
;;   )
;; (use-package csharp-mode
;;   :ensure t
;;   )

(use-package dart-mode
  :ensure t)

(use-package lsp-dart
  :ensure t)

(use-package lsp-treemacs
  :ensure t)
(use-package company
  :ensure t)
(use-package hover
  :ensure t)

(use-package lsp-origami
  :hook
  (
   (origami-mode . lsp-origami)
   (erlang-mode . origami-mode)
   )
  )

(use-package lua-mode
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

(use-package python-mode
  :ensure t
  :config
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter-args "-i --simple-prompt")))



