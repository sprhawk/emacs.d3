(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq packages (expand-file-name "~/.emacs.d/packages.el"))
(load packages)

(setq-default c-basic-offset 2)

(global-set-key (kbd "C-c C-_") 'comment-or-uncomment-region)
;; (global-set-key (kbd "C-x 0")
;;                 (lambda ()
;;                   (interactive)
;;                   (other-window -1)))

(global-set-key (kbd "C-x C-\\") 'set-input-method)
(global-set-key (kbd "C-x v") 'view-buffer)

(define-key key-translation-map (kbd "C-c p") (kbd "π"))
(define-key key-translation-map (kbd "C-c a") (kbd "α"))
(define-key key-translation-map (kbd "C-c d") (kbd "Δ"))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default js-indent-level 2)

;; (set-face-foreground 'minibuffer-prompt "blue")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(afternoon))
 '(custom-safe-themes
   '("57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "fad9c3dbfd4a889499f6921f54f68de8857e6846a0398e89887dbe5f26b591c0" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default))
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(inhibit-startup-screen t)
 '(lsp-vetur-server-command '("yarn bin vls"))
 '(lsp-vetur-use-workspace-dependencies t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(package-selected-packages
   '(python-mode realgud-ipdb julia-snail vterm lua-mode prettier emacsql-sqlite3 quelpa-use-package lsp-julia quelpa ack gnu-elpa-keyring-update protobuf-mode graphql-mode lsp-ui go-mode cmake-font-lock ccls company-lsp lsp-mode grip-mode afternoon-theme lispy rust-mode vue-mode octave-mode tidal julia-repl color-theme-sanityinc-tomorrow zenburn-theme tango-2-theme dracula-theme julia-mode web-mode jinja2-mode flycheck elpy php-mode git scss-mode django-snippets django-mode sass-mode json-mode docker-compose-mode dockerfile-mode yaml-mode ensime ecb magit company racer slime))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   '((c-default-style . "linux")
     (c-default-style . "bsd")
     (lsp-rust-analyzer-cargo-watch-args . "--target-dir ./build/check")
     (c-intent-level . 4)
     (intent-tabs-mode)
     (eval lexical-let
           ((project-directory
             (car
              (dir-locals-find-file default-directory))))
           (set
            (make-local-variable 'flycheck-javascript-eslint-executable)
            (concat project-directory ".yarn/sdks/eslint/bin/eslint.js"))
           (eval-after-load 'lsp-clients
             '(progn
                (plist-put lsp-deps-providers :local
                           (list :path
                                 (lambda
                                   (path)
                                   (concat project-directory ".yarn/sdks/" path))))))
           (lsp-dependency 'typescript-language-server
                           '(:local "typescript-language-server/lib/cli.js"))
           (lsp-dependency 'typescript
                           '(:local "typescript/bin/tsserver")))
     (lsp-enabled-clients ts-ls eslint)))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background-mode nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
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
