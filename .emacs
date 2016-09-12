;; Hacker mode
(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; HiDPI
(set-default-font "Monospace-14")

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

(defvar elpa-packages '(
                        company
			alchemist
			magit
			web-mode
			company-web
			yaml-mode
			markdown-mode
			reykjavik-theme
			key-chord
			helm
                        flycheck
                        tide
                        projectile
                        helm-projectile
                        neotree
                        ))

(defun cfg:install-packages ()
  (let ((pkgs (remove-if #'package-installed-p elpa-packages)))
    (when pkgs
      (package-refresh-contents)
      (dolist (p elpa-packages)
        (package-install p)))))

;(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(cfg:install-packages)

;; Trust
(custom-set-variables
 '(custom-safe-themes
   (quote
    ("01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" default))))

;; Theme
(load-theme 'reykjavik)
(require 'key-chord)

;; Customizations
(key-chord-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq scroll-step 1) ;; keyboard scroll one line at a time

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-company-mode 1)

(setq company-idle-delay 0.1)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-flip-when-above t)

(projectile-global-mode)
(require 'helm-projectile)
(helm-projectile-on)
(global-set-key [f8] 'neotree-toggle)

(require 'elixir-mode)
(require 'alchemist)

(setq-default indent-tabs-mode nil)

(require 'web-mode)

;(add-to-list 'load-path "~/.emacs.d/custom")
;(require 'flycheck-typescript-tslint)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)
(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
