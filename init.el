(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

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

(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa" . 1)))

(package-initialize)
(when (not package-archive-contents)
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
  (advice-add 'python-mode :before 'elpy-enable))

;; don't know how to enable flycheck-mode with above method, so enable right now
;; (elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


(use-package ensime
	     :ensure t
	     :pin melpa-stable)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(custom-safe-themes
   (quote
    ("d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "fad9c3dbfd4a889499f6921f54f68de8857e6846a0398e89887dbe5f26b591c0" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default)))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (lispy rust-mode vue-mode octave-mode tide tidal julia-repl color-theme-sanityinc-tomorrow zenburn-theme tango-2-theme dracula-theme julia-mode web-mode jinja2-mode flycheck elpy realgud php-mode git scss-mode django-snippets django-mode sass-mode json-mode typescript-mode docker-compose-mode dockerfile-mode yaml-mode ensime ecb magit cargo company racer slime)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background-mode nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; (setq inferior-lisp-program "sbcl")

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

(use-package vue-mode
  :ensure t
  :mode ("\\.vue$" ))

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
  (load (expand-file-name "~/quicklisp/slime-helper.el"))

  :config
  (add-to-list 'slime-contribs 'slime-fancy)
  :hook
  (lisp-mode . prettify-symbols-mode)
  (lisp-mode . company-mode))

;; (add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))
;; (require 'julia-repl)
;; (add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode


(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; (add-hook 'elpy-mode-hook  ;; C-c C-/ is interpreted in emacs as C-c C-_
;;           (lambda() (local-set-key (kbd "C-c C-_") #'comment-or-uncomment-region)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq company-tooltip-align-annotations t)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(global-set-key (kbd "C-c C-_") 'comment-or-uncomment-region)

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

(global-set-key (kbd "C-x C-\\") 'set-input-method)
(global-set-key (kbd "C-x v") 'view-buffer)

(define-key key-translation-map (kbd "C-c p") (kbd "π"))
(define-key key-translation-map (kbd "C-c a") (kbd "α"))
(define-key key-translation-map (kbd "C-c d") (kbd "Δ"))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt"))

(set-face-foreground 'minibuffer-prompt "blue")
