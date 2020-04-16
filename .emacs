;;; package --- Summary
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/notes/illness.org" "~/notes/inbox.org" "~/notes/unr.org" "~/tmp/testorg/main.org")) t)
 '(package-selected-packages
   (quote
    (cmake-ide cmake-mode cmake-project counsel-etags gnu-elpa-keyring-update flycheck-rust flycheck-rtags company flycheck counsel ivy ivy-bibtex helm cargo deft julia-shell julia-repl julia-mode rust-mode slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;; Set up melpa
(require 'package)
(let ((proto "https"))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

;;;; Set up slime
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;;; Set up Julia
(add-to-list 'load-path "/home/richard/opt/julia-1.1.1/bin/julia")
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode

;;;; Set up deft
(setq deft-recursive t)
(setq deft-extensions '("org"))
(setq deft-directory "~/notes")

;;;; Set up org-mode
(setq org-directory "~/notes")
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-agenda-files
      (list "~/notes/inbox.org"
	    "~/notes/unr.org"))

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

(setq org-log-done 'time)
		
;;;; Set startup
(setq inhibit-splash-screen t)
(if (= (length command-line-args) 1)
    (progn
      (org-agenda-list)
      (delete-other-windows)
      (make-frame-command)
      (let ((first-frame (car (frame-list)))
	    (second-frame (cadr (frame-list))))
	(let ((first-window (car (window-list first-frame))))
	  (split-window first-window (/ (window-width first-window) 2) 'right)
	  (with-selected-window first-window
	    (find-file "~/notes/unr.org")))
	(with-selected-frame second-frame
	  (switch-to-buffer "*scratch*")))))

;;;; Ivy setup
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;;;; Programming customizations
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(setq gdb-many-windows t)

(use-package rtags
  :ensure t
  :hook (c++-mode . rtags-start-process-unless-running)
  :config (setq rtags-completions-enabled t
		rtags-path "/home/richard/opt/rtags/src/rtags.el"
		rtags-rc-binary-name "/home/richard/opt/rtags/bin/rc"
		rtags-rdm-binary-name "/home/richard/opt/rtags/bin/rdm")
  :bind (("C-c E" . rtags-find-symbol)
  	 ("C-c e" . rtags-find-symbol-at-point)
  	 ("C-c O" . rtags-find-references)
  	 ("C-c o" . rtags-find-references-at-point)
  	 ("C-c s" . rtags-find-file)
  	 ("C-c v" . rtags-find-virtuals-at-point)
  	 ("C-c F" . rtags-fixit)
  	 ("C-c f" . rtags-location-stack-forward)
  	 ("C-c b" . rtags-location-stack-back)
  	 ("C-c n" . rtags-next-match)
  	 ("C-c p" . rtags-previous-match)
  	 ("C-c P" . rtags-preprocess-file)
  	 ("C-c R" . rtags-rename-symbol)
  	 ("C-c x" . rtags-show-rtags-buffer)
  	 ("C-c T" . rtags-print-symbol-info)
  	 ("C-c t" . rtags-symbol-type)
  	 ("C-c I" . rtags-include-file)
  	 ("C-c i" . rtags-get-include-file-for-symbol)))

(cmake-ide-setup)
(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)



(provide '.emacs)
;;; .emacs ends here
