;;;; STRAIGHT bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; USE-PACKAGE
(straight-use-package 'use-package)

;;;; HELM configuration
(use-package helm
  :ensure t
  :straight t)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-split-window-in-side-p t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)

;;;; SLIME
(use-package slime
  :ensure t
  :straight t)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; Programming customizations
(add-hook 'prog-mode-hook 'linum-mode)

;;;; ORG-MODE
(use-package org
  :ensure t
  :straight t
  :config
  (setq org-log-done 'time)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75))
  (add-hook 'org-mode-hook
	    (lambda ()
	      (org-toggle-latex-fragment)))
  (add-hook 'org-mode-hook
	    (lambda ()
	      (add-hook 'after-save-hook 'org-latex-preview nil 'make-it-local))))

(setq org-startup-latex-with-latex-preview t)
(setq org-latex-create-formula-image-program 'dvisvgm)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width '(300))      

(setq org-agenda-files (list "~/org/unr.org"
			     "~/org/crypto.org"
			     "~/org/personal.org"))
(setq org-agenda-window-setup 'current-window)
(setq org-hide-emphasis-markers t)

(use-package org-bullets
  :ensure t
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-todo-keywords
      '((sequence "TODO"
		  "STARTED"
		  "DONE")))

(setq org-todo-keyword-faces
      '(("TODO" . "red")
	("STARTED" . "blue")
	("DONE" . "dark green")))

;;;; ORG-JOURNAL
(use-package org-journal
  :ensure t
  :straight t
  :init
  (setq org-journal-prefix-key "C-c j ")
  :bind (("C-c C-j" . org-journal-new-entry))
  :config
  (setq org-journal-dir "~/journal"
	org-journal-date-format "%A, %d %B %Y"))

;;;; ORG-ROAM
(use-package websocket
  :ensure t
  :straight t)

(use-package simple-httpd
  :ensure t
  :straight t)

(use-package f
  :ensure t
  :straight t)  

(use-package json
  :ensure t
  :straight t)

(use-package org-roam
  :ensure t
  :straight t
  :custom
  (org-roam-directory (file-truename "/home/richard/my-wiki"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ;; dailies
	 ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup))

(add-hook 'org-mode-hook 'org-indent-mode)

;;;; BIBTEX
(setq bibtex-dialect 'biblatex)

(setq bibtex-user-optional-fields '(("keywords" "Keywords to describe entry." "")
				    ("file" "Link to document file." ":"))
      bibtex-align-at-equal-sign t
      bibtex-completion-pdf-open-function
      (lambda (fpath)
	(call-process "evince" nil 0 nil fpath)))

(setq bib-files-directory "~/bib/articles.bib"
      pdf-files-directory (concat (getenv "HOME") "/Dropbox/pdf"))

(use-package helm-bibtex
  :ensure t
  :straight t
  :config
  (setq bibtex-completion-bibliography bib-files-directory
	bibtex-completion-library-path pdf-files-directory
	bibtex-completion-pdf-field "File"
	bibtex-completion-notes-path org-directory
	bibtex-completion-additional-search-fields '(keywords))
  :bind
  (("C-c n B" . helm-bibtex)))  

(use-package org-ref
  :ensure t
  :straight t)

(use-package org-roam-bibtex
  :ensure t
  :straight t
  :after (org-roam helm-bibtex)
  :bind (:map org-mode-map ("C-c n b" . orb-note-actions))
  :config
  (require 'org-ref))
(org-roam-bibtex-mode)

;;;; ORG-ROAM-UI
(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))

(setq gdb-many-windows t)

(use-package cmake-mode
  :ensure t
  :straight t)  

(use-package rustic
  :ensure t
  :straight t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package lsp-mode
  :ensure t
  :straight t
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure t
  :straight t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last))
  (:map company-mode-map
	("<tab>". tab-indent-or-complete)
	("TAB". tab-indent-or-complete)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))	      

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package flycheck
  :ensure t
  :straight t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
