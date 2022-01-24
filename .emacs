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

;;;; HELM configuration
(straight-use-package 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-split-window-in-side-p t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)

;;;; SLIME
(straight-use-package 'slime)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;;; USE-PACKAGE
(straight-use-package 'use-package)

;; Programming customizations
(add-hook 'prog-mode-hook 'linum-mode)

;;;; ORG-MODE
(use-package org
  :ensure t
  :straight t
  :config
  (setq org-log-done 'time)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (add-hook 'org-mode-hook
	    (lambda ()
	      (org-toggle-latex-fragment)))
  (add-hook 'org-mode-hook
	    (lambda ()
	      (add-hook 'after-save-hook 'org-latex-preview nil 'make-it-local))))

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
		  "DONE"
		  "CANCELLED")))

(setq org-todo-keyword-faces
      '(("TODO" . "red")
	("STARTED" . "blue")
	("DONE" . "dark green")
	("CANCELLED" . "dark orange" )))

;;;; ORG-JOURNAL
(use-package org-journal
  :ensure t
  :straight t
  :init
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/journal"
	org-journal-date-format "%A, %d %B %Y"))

;;;; ORG-ROAM
(straight-use-package 'websocket)
(straight-use-package 'simple-httpd)
(straight-use-package 'f)
(straight-use-package 'json)

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
