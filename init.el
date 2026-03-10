;;; init.el --- Minimal VS Code-like startup layout -*- lexical-binding: t; -*-

;; Basic startup behavior.
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; Make the default font a little larger than Emacs ships with.
(set-face-attribute 'default nil :height 140)

;; Trim some default UI chrome.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

(setq ring-bell-function #'ignore)
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

(require 'package)
(require 'project)
(require 'shell)
(require 'seq)
(require 'vc-dir)

(defconst richard/left-panel-ratio 0.216
  "Fraction of the frame width used for the file tree.")

(defconst richard/right-panel-ratio 0.216
  "Fraction of the frame width used for the right-side panel.")

(defconst richard/bottom-panel-ratio 0.10
  "Fraction of the frame height used for the bottom shell panel.")

(defconst richard/min-left-panel-width 25
  "Minimum width of the left sidebar in columns.")

(defconst richard/max-left-panel-width 43
  "Maximum width of the left sidebar in columns.")

(defconst richard/min-right-panel-width 25
  "Minimum width of the right-side panel in columns.")

(defconst richard/max-right-panel-width 43
  "Maximum width of the right-side panel in columns.")

(defconst richard/min-bottom-panel-height 4
  "Minimum height of the shell panel in lines.")

(defconst richard/max-bottom-panel-height 10
  "Maximum height of the shell panel in lines.")

(defconst richard/min-left-column-panel-height 8
  "Minimum height for either left-column panel in lines.")

(defconst richard/max-left-column-panel-height 24
  "Maximum height for either left-column panel in lines.")

(defconst richard/git-refresh-idle-seconds 2
  "Seconds of idle time before refreshing visible Git status buffers.")

(defvar richard/launch-directory
  (file-name-as-directory
   (expand-file-name
    (or (and (boundp 'command-line-default-directory)
             command-line-default-directory)
        default-directory)))
  "Directory Emacs was launched from.")

(defconst richard/frame-edge-margin 16
  "Margin in pixels between the frame and the monitor workarea.")

(defconst richard/frame-top-margin 0
  "Top margin in pixels for startup frame placement.")

(defconst richard/frame-right-margin 28
  "Extra right-side margin in pixels to avoid overshooting the screen edge.")

(defconst richard/frame-bottom-margin 20
  "Extra bottom margin in pixels to account for window chrome.")

(defun richard/clamp (value minimum maximum)
  "Clamp VALUE between MINIMUM and MAXIMUM."
  (max minimum (min value maximum)))

(defun richard/frame-workarea (&optional frame)
  "Return FRAME's monitor workarea as (X Y WIDTH HEIGHT)."
  (or (and (fboundp 'frame-monitor-workarea)
           (frame-monitor-workarea frame))
      (let ((attrs (frame-monitor-attributes frame)))
        (or (alist-get 'workarea attrs)
            (alist-get 'geometry attrs)))
      (list 0 0 (display-pixel-width) (display-pixel-height))))

(defun richard/fit-frame-to-workarea (&optional frame)
  "Size FRAME to fill the monitor workarea without using native maximize."
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (pcase-let* ((`(,x ,y ,width ,height) (richard/frame-workarea frame))
                   (left-margin richard/frame-edge-margin)
                   (top-margin richard/frame-top-margin)
                   (right-margin richard/frame-right-margin)
                   (bottom-margin richard/frame-bottom-margin)
                   (target-width (max 200 (- width left-margin right-margin)))
                   (target-height (max 200 (- height top-margin bottom-margin))))
        (set-frame-position frame (+ x left-margin) (+ y top-margin))
        (set-frame-size frame target-width target-height t)))))

(defun richard/activate-emacs-app (&optional frame)
  "Ask macOS to bring Emacs to the foreground for FRAME."
  (let ((frame (or frame (selected-frame))))
    (when (and (eq system-type 'darwin)
               (display-graphic-p frame)
               (fboundp 'ns-do-applescript))
      (run-at-time
       0 nil
       (lambda ()
         (ignore-errors
           (ns-do-applescript
            "tell application \"Emacs\" to activate")))))))

(defun richard/left-panel-width (&optional frame)
  "Return the autoscaled width for the left sidebar in FRAME."
  (richard/clamp
   (floor (* (frame-width frame) richard/left-panel-ratio))
   richard/min-left-panel-width
   richard/max-left-panel-width))

(defun richard/right-panel-width (&optional frame)
  "Return the autoscaled width for the right panel in FRAME."
  (richard/clamp
   (floor (* (frame-width frame) richard/right-panel-ratio))
   richard/min-right-panel-width
   richard/max-right-panel-width))

(defun richard/bottom-panel-height (&optional frame)
  "Return the autoscaled height for the shell panel in FRAME."
  (richard/clamp
   (floor (* (frame-height frame) richard/bottom-panel-ratio))
   richard/min-bottom-panel-height
   richard/max-bottom-panel-height))

(defun richard/left-column-panel-height (&optional frame)
  "Return the height for each left-column panel in FRAME."
  (let ((frame (or frame (selected-frame))))
    (richard/clamp
     (floor (/ (- (frame-height frame)
                  (richard/bottom-panel-height frame))
               2))
     richard/min-left-column-panel-height
     richard/max-left-column-panel-height)))

(defun richard/notes-buffer ()
  "Return the startup notes buffer."
  (let ((buffer (get-buffer-create "*notes*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'text-mode)
        (text-mode))
      (setq-local header-line-format " Notes "))
    buffer))

(defun richard/right-panel-buffer ()
  "Return the placeholder buffer for the right-side panel."
  (let ((buffer (get-buffer-create "*right-panel*")))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Right panel\n\nReserved for whatever you want to put here later.\n")))
    buffer))

(defun richard/no-git-buffer ()
  "Return a placeholder buffer for the git panel."
  (let ((buffer (get-buffer-create "*git-panel*")))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Git panel\n\nNo Git repository found for the current directory.\n")))
    buffer))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-archive-priorities
      '(("gnu" . 30)
        ("nongnu" . 20)
        ("melpa" . 10)))

(package-initialize)

(defun richard/ensure-package-installed (package-name)
  "Install PACKAGE-NAME if it is not already available."
  (unless (package-installed-p package-name)
    (condition-case nil
        (progn
          (unless package-archive-contents
            (package-refresh-contents))
          (package-install package-name))
      (error
       (package-refresh-contents)
       (package-install package-name)))))

(defun richard/ensure-treemacs ()
  "Install and load Treemacs, returning non-nil on success."
  (condition-case err
      (progn
        (richard/ensure-package-installed 'treemacs)
        (require 'treemacs)
        (setq treemacs-width (richard/left-panel-width)
              treemacs-persist-file nil
              treemacs-width-is-initially-locked nil
              treemacs-expand-after-init t
              treemacs-expand-added-projects t
              treemacs-collapse-dirs 0
              treemacs-display-in-side-window t
              treemacs-position 'left
              treemacs-default-visit-action
              #'treemacs-visit-node-in-most-recently-used-window)
        (when (fboundp 'treemacs-follow-mode)
          (treemacs-follow-mode 1))
        (when (and (fboundp 'treemacs-git-mode)
                   (executable-find "git"))
          (treemacs-git-mode 'simple)
          (when (fboundp 'treemacs-hide-gitignored-files-mode)
            (treemacs-hide-gitignored-files-mode 1)))
        t)
    (error
     (message "Treemacs setup failed: %s" (error-message-string err))
     nil)))

(defun richard/treemacs-project-name (directory)
  "Return a display name for DIRECTORY in Treemacs."
  (file-name-nondirectory
   (directory-file-name (file-name-as-directory directory))))

(defun richard/expand-treemacs-root ()
  "Ensure the current Treemacs root is expanded."
  (when (derived-mode-p 'treemacs-mode)
    (goto-char (point-min))
    (let ((button (treemacs-current-button)))
      (when (and button
                 (not (treemacs-is-node-expanded? button)))
        (treemacs--expand-root-node button)))))

(defun richard/configure-treemacs-workspace (directory)
  "Make DIRECTORY the sole Treemacs project in the current workspace."
  (let* ((path (if (fboundp 'treemacs-canonical-path)
                   (treemacs-canonical-path directory)
                 (file-name-as-directory (expand-file-name directory))))
         (project (treemacs-project->create!
                   :name (richard/treemacs-project-name path)
                   :path path
                   :path-status (if (fboundp 'treemacs--get-path-status)
                                    (treemacs--get-path-status path)
                                  'local-readable)))
         (workspace (treemacs-workspace->create!
                     :name "Default"
                     :projects (list project)))
         (scope (and (fboundp 'treemacs-current-scope)
                     (treemacs-current-scope)))
         (existing-shelf (and scope (assoc scope treemacs--scope-storage)))
         (buffer (and existing-shelf
                      (treemacs-scope-shelf->buffer (cdr existing-shelf))))
         (shelf (and (fboundp 'treemacs-scope-shelf->create!)
                     (treemacs-scope-shelf->create!
                      :buffer buffer
                      :workspace workspace))))
    (setq treemacs--disabled-workspaces nil
          treemacs--workspaces (list workspace))
    (when (and scope shelf)
      (if existing-shelf
          (setcdr existing-shelf shelf)
        (push (cons scope shelf) treemacs--scope-storage)))))

(defun richard/shell-buffer ()
  "Return a live shell buffer."
  (let ((buffer (get-buffer "*shell*")))
    (unless (comint-check-proc buffer)
      (save-window-excursion
        (shell "*shell*"))
      (setq buffer (get-buffer "*shell*")))
    buffer))

(defun richard/show-side-window (buffer side slot size)
  "Display BUFFER in a dedicated side window on SIDE using SLOT and SIZE."
  (let* ((window
          (display-buffer-in-side-window
           buffer
           `((side . ,side)
             (slot . ,slot)
             ,@(if (memq side '(left right))
                   `((window-width . ,size))
                 `((window-height . ,size)))
             (window-parameters
              . ((no-delete-other-windows . t)
                 (no-other-window . nil)))))))
    (set-window-dedicated-p window t)
    window))

(defun richard/center-window (&optional frame)
  "Return the main editing window for FRAME."
  (let ((window (frame-parameter frame 'richard-center-window)))
    (when (window-live-p window)
      window)))

(defun richard/sidebar-window (&optional frame)
  "Return the file-tree window for FRAME."
  (let ((window (frame-parameter frame 'richard-sidebar-window)))
    (when (window-live-p window)
      window)))

(defun richard/editor-window-p (&optional window)
  "Return non-nil when WINDOW is a regular editor window."
  (let ((window (or window (selected-window))))
    (and (window-live-p window)
         (not (window-minibuffer-p window))
         (not (window-parameter window 'window-side)))))

(defun richard/remember-editor-window ()
  "Remember the currently selected editor window for this frame."
  (when (richard/editor-window-p)
    (set-frame-parameter nil 'richard-center-window (selected-window))))

(defun richard/first-editor-window (&optional frame)
  "Return the first live editor window in FRAME."
  (seq-find #'richard/editor-window-p (window-list frame 'no-minibuffer)))

(defun richard/treemacs-window (&optional frame)
  "Return the Treemacs window for FRAME."
  (let ((window
         (seq-find
          (lambda (candidate)
            (with-current-buffer (window-buffer candidate)
              (derived-mode-p 'treemacs-mode)))
          (window-list frame 'no-minibuffer))))
    (when (window-live-p window)
      window)))

(defun richard/git-window (&optional frame)
  "Return the Git status window for FRAME."
  (let ((window
         (seq-find
          (lambda (candidate)
            (with-current-buffer (window-buffer candidate)
              (derived-mode-p 'vc-dir-mode)))
          (window-list frame 'no-minibuffer))))
    (when (window-live-p window)
      window)))

(defun richard/resize-window-width (window width)
  "Resize WINDOW horizontally to WIDTH columns."
  (when (window-live-p window)
    (let ((delta (- width (window-total-width window))))
      (unless (zerop delta)
        (condition-case nil
            (window-resize window delta t)
          (error nil))))))

(defun richard/resize-window-height (window height)
  "Resize WINDOW vertically to HEIGHT lines."
  (when (window-live-p window)
    (let ((delta (- height (window-total-height window))))
      (unless (zerop delta)
        (condition-case nil
            (window-resize window delta nil)
          (error nil))))))

(defun richard/git-root (&optional directory)
  "Return the Git root for DIRECTORY, or nil if none exists."
  (let ((directory (file-name-as-directory
                    (expand-file-name (or directory default-directory)))))
    (locate-dominating-file directory ".git")))

(defun richard/show-git-panel (&optional frame)
  "Display a Git status buffer in the lower-left panel of FRAME."
  (let* ((frame (or frame (selected-frame)))
         (directory (or (frame-parameter frame 'richard-sidebar-directory)
                        default-directory))
         (repo (richard/git-root directory))
         (window (richard/show-side-window
                  (if repo
                      (get-buffer-create "*vc-dir*")
                    (richard/no-git-buffer))
                  'left 1
                  (richard/left-panel-width frame))))
    (set-window-dedicated-p window nil)
    (richard/resize-window-height window (richard/left-column-panel-height frame))
    (set-frame-parameter frame 'richard-git-window window)
    (if repo
        (with-selected-window window
          (let ((default-directory repo))
            (vc-dir repo)
            (set-frame-parameter frame 'richard-git-window (selected-window))))
      (set-window-buffer window (richard/no-git-buffer)))
    (let ((git-window (frame-parameter frame 'richard-git-window)))
      (when (window-live-p git-window)
        git-window))))

(defun richard/sync-git-panel (&optional frame)
  "Ensure the git panel in FRAME matches the current directory state."
  (let* ((frame (or frame (selected-frame)))
         (directory (or (frame-parameter frame 'richard-sidebar-directory)
                        default-directory))
         (repo (richard/git-root directory))
         (window (or (richard/git-window frame)
                     (frame-parameter frame 'richard-git-window))))
    (cond
     ((and repo
           (or (not (window-live-p window))
               (with-current-buffer (window-buffer window)
                 (not (derived-mode-p 'vc-dir-mode)))))
      (richard/show-git-panel frame))
     ((and (not repo)
           (window-live-p window)
           (with-current-buffer (window-buffer window)
             (derived-mode-p 'vc-dir-mode)))
      (richard/show-git-panel frame)))))

(defvar richard/git-refresh-timer nil
  "Idle timer used to refresh visible Git status buffers.")

(defun richard/refresh-git-panels ()
  "Refresh git panels and visible Git status buffers."
  (dolist (frame (frame-list))
    (when (frame-live-p frame)
      (condition-case nil
          (richard/sync-git-panel frame)
        (error nil))))
  (dolist (window (window-list nil 'no-minibuffer))
    (with-current-buffer (window-buffer window)
      (when (derived-mode-p 'vc-dir-mode)
        (condition-case nil
            (revert-buffer)
          (error nil))))))

(defun richard/start-git-refresh-timer ()
  "Start the idle refresh timer for visible Git status buffers."
  (unless (timerp richard/git-refresh-timer)
    (setq richard/git-refresh-timer
          (run-with-idle-timer richard/git-refresh-idle-seconds t
                               #'richard/refresh-git-panels))))

(defun richard/show-treemacs-sidebar (&optional frame)
  "Display Treemacs for the current directory in FRAME."
  (let* ((frame (or frame (selected-frame)))
         (directory
          (file-name-as-directory
           (expand-file-name
            (or (frame-parameter frame 'richard-sidebar-directory)
                richard/launch-directory)))))
    (when (richard/ensure-treemacs)
      (with-selected-frame frame
        (set-frame-parameter frame 'richard-sidebar-directory directory)
        (let ((default-directory directory))
          (setq treemacs-width (richard/left-panel-width frame))
          (when (and (fboundp 'treemacs-workspace->create!)
                     (fboundp 'treemacs-project->create!))
            (richard/configure-treemacs-workspace directory))
          (cond
           ((richard/treemacs-window frame)
            (when (fboundp 'treemacs--show-single-project)
              (treemacs--show-single-project
               directory
               (richard/treemacs-project-name directory))))
           ((fboundp 'treemacs--init)
            ;; Seed the exact startup directory as Treemacs creates its buffer.
            ;; This avoids the root-picker prompt and lets Treemacs render the
            ;; initial tree in its normal initialization path.
            (treemacs--init))
           (t
            (message "Treemacs startup directory sync failed for %s: no supported init function found"
                     directory)))
          (when (richard/treemacs-window frame)
            (with-current-buffer (window-buffer (richard/treemacs-window frame))
              (richard/expand-treemacs-root))))
        (let ((window (or (richard/treemacs-window frame)
                          (richard/sidebar-window frame))))
          (when (window-live-p window)
            (set-frame-parameter frame 'richard-sidebar-window window)
            window))))))

(defun richard/refresh-layout-sizes (&optional frame)
  "Resize the side panels for FRAME using the configured ratios."
  (let ((frame (or frame (selected-frame))))
    (when (richard/center-window frame)
      (with-selected-frame frame
        (let ((sidebar-window (richard/treemacs-window frame)))
          (when (window-live-p sidebar-window)
            (set-frame-parameter frame 'richard-sidebar-window sidebar-window)
            (setq treemacs-width (richard/left-panel-width frame))))
        (let ((git-window (or (richard/git-window frame)
                              (frame-parameter frame 'richard-git-window))))
          (when (window-live-p git-window)
            (set-frame-parameter frame 'richard-git-window git-window)
            (richard/resize-window-width git-window
                                         (richard/left-panel-width frame))
            (richard/resize-window-height git-window
                                          (richard/left-column-panel-height frame))))
        (richard/show-side-window
         (richard/shell-buffer)
         'bottom 0
         (richard/bottom-panel-height frame))
        (richard/show-side-window
         (richard/right-panel-buffer)
         'right 0
         (richard/right-panel-width frame))
        (let ((center-window (richard/center-window frame)))
          (when (window-live-p center-window)
            (select-window center-window)))))))

(defun richard/maybe-refresh-layout (frame)
  "Refresh layout sizes for FRAME when its dimensions change."
  (let ((size (cons (frame-width frame) (frame-height frame))))
    (unless (equal size (frame-parameter frame 'richard-layout-size))
      (set-frame-parameter frame 'richard-layout-size size)
      (condition-case nil
          (richard/refresh-layout-sizes frame)
        (error nil)))))

(defun richard/center-buffer ()
  "Pick the main editing buffer."
  (or (seq-find
       (lambda (buffer)
         (and (buffer-live-p buffer)
              (buffer-file-name buffer)))
       (buffer-list))
      (and (buffer-file-name (current-buffer))
           (current-buffer))
      (richard/notes-buffer)))

(defun richard/command-line-file-args-p ()
  "Return non-nil when Emacs was started with a file argument."
  (seq-some
   (lambda (arg)
     (and (stringp arg)
          (not (string-prefix-p "-" arg))
          (not (member arg '("emacs" "Emacs")))))
   command-line-args))

(defun richard/initial-buffer-choice ()
  "Choose the initial buffer for startup."
  (if (richard/command-line-file-args-p)
      (current-buffer)
    (richard/notes-buffer)))

(defun richard/apply-startup-layout (&optional frame)
  "Build the startup layout in FRAME."
  (let ((frame (or frame (selected-frame))))
    (with-selected-frame frame
      (let ((center (richard/center-buffer)))
        (richard/fit-frame-to-workarea frame)
        (delete-other-windows)
        (set-frame-parameter frame 'richard-sidebar-directory richard/launch-directory)
        (switch-to-buffer center)
        (set-frame-parameter frame 'richard-center-window (selected-window))
        (set-frame-parameter frame 'richard-layout-size
                             (cons (frame-width frame) (frame-height frame)))
        (richard/show-treemacs-sidebar frame)
        (richard/show-git-panel frame)
        (richard/show-side-window
         (richard/shell-buffer)
         'bottom 0
         (richard/bottom-panel-height frame))
        (richard/show-side-window
         (richard/right-panel-buffer)
         'right 0
         (richard/right-panel-width frame))
        (select-window (get-buffer-window center frame))))))

(setq initial-buffer-choice #'richard/initial-buffer-choice)

(unless noninteractive
  (add-hook 'emacs-startup-hook #'richard/activate-emacs-app)
  (add-hook 'emacs-startup-hook #'richard/apply-startup-layout)
  (add-hook 'emacs-startup-hook #'richard/start-git-refresh-timer)
  (add-hook 'after-make-frame-functions #'richard/activate-emacs-app)
  (add-hook 'after-make-frame-functions #'richard/apply-startup-layout)
  (add-hook 'window-size-change-functions #'richard/maybe-refresh-layout)
  (add-hook 'after-save-hook #'richard/refresh-git-panels)
  (add-hook 'post-command-hook #'richard/remember-editor-window))

(provide 'init)
;;; init.el ends here
