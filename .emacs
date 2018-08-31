;; Hacker mode
(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Font
(set-default-font "EnvyCodeR-12")

;; To each his own
(when window-system (set-frame-size (selected-frame) 85 45))

;; Don't leave junk files everywhere
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Automatic shit
(require 'package)
(require 'cl)
(require 'format-spec)

;; Scrolling Smooth
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(defvar elpa-packages '(
                        company
			magit
			web-mode
			company-web
			yaml-mode
                        ruby-mode
                        rhtml-mode
			markdown-mode
			key-chord
			helm
                        flycheck
                        tide
                        projectile
                        helm-projectile
                        neotree
                        flymake-ruby
                        terraform-mode
                        salt-mode
                        nvm
                        groovy-mode
                        doom-themes
                        solidity-mode
                        ))

(defun cfg:install-packages ()
  (let ((pkgs (remove-if #'package-installed-p elpa-packages)))
    (when pkgs
      (package-refresh-contents)
      (dolist (p elpa-packages)
        (package-install p)))))

;(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(cfg:install-packages)

;; Trust


;; Theme
(load-theme 'doom-one t)
(require 'key-chord)

;; Customizations
(key-chord-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq scroll-step 1) ;; keyboard scroll one line at a time
(global-set-key (kbd "C-c C-g") 'solidity-estimate-gas-at-point) ;; solidity gas estimate
(setq solidity-flycheck-solc-checker-active t)
(setq solidity-solc-path "/usr/bin/solc")

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x g") 'magit-status)

(global-company-mode 1)

(setq company-idle-delay 0.1)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-flip-when-above t)
(setq company-dabbrev-downcase nil)

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(projectile-global-mode)
(require 'helm-projectile)
(helm-projectile-on)
(global-set-key [f8] 'neotree-toggle)

(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(require 'web-mode)

;(add-to-list 'load-path "~/.emacs.d/custom")
;(require 'flycheck-typescript-tslint)
(defun setup-tide-mode ()
  (require 'nvm)
  (nvm-use "v10.1.0")
  (interactive)
  (tide-setup)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t)
  (setq typescript-indent-level 2))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(setq js-indent-level 2)

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 1))

(add-hook 'web-mode-hook  'my-web-mode-hook)
(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (vcl-mode go-mode go django-mode dockerfile-mode solidity-mode php-mode doom-themes projectile flycheck helm ruby-mode yaml-mode company web-mode ujelly-theme tide terraform-mode sourcerer-theme salt-mode rhtml-mode neotree markdown-mode magit key-chord inkpot-theme helm-projectile groovy-mode flymake-ruby danneskjold-theme company-web))))
