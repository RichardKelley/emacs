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
      pdf-files-directory (concat (getenv "HOME") "/pdf"))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (use-package org-journal org-roam glsl-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)
;;;; .emacs ends here
