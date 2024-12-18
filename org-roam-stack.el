;;; org-roam-stack.el --- organize org roam in stack -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020  Free Software Foundation, Inc.

;; Author: Gunther Bachmann <gunther.bachmann@web.de>
;; Maintainer: gunther.bachmann@web.de
;; Package: org-roam-stack
;; Homepage: https://github.com/gunther-bachmann/org-roam-slack
;; Version: 0.0.1
;; Package-Requires: ((emacs "27"))
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; https://github.com/gunther-bachmann/org-roam-stack

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'dash)
(require 'windmove)

(require 'ol)
(require 'org)
(require 'org-roam)
(require 'org-protocol)
(require 'counsel)
(require 'bind-key)
(require 'async)

(defgroup org-roam-stack nil
  "organize org roam cards in a stack"
  :group 'org-roam)

(defface org-roam-stack-inactive-face '((t :slant italic :foreground "DimGray"))
  "Face used to mark inactive buffers"
  :group 'font-lock-faces)

(defvar org-roam-stack--stack-height (frame-height)
  "height of the stack used for balance window calculations")

(defvar org-roam-stack--buffer-list '()
  "list of buffers that form the stack.
they are ordered from bottom to top => adding a buffer to the 
bottom of the stack puts it at the head of this list.
this list is kept in sync with the visual display of the stack.
if by some commands the list gets out of sync, 
org-roam-stack--restore-stack-view can be used to build up the 
visual according the this list")

(defvar org-roam-stack--buffer-open-resize-strategy 'maximize
  "either 'maximize or 'balance")

(defcustom org-roam-stack--link-adjustments nil
  "do some link font locking in org files"
  :type 'boolean
  :group 'org-roam-stack)

(defcustom org-roam-stack--focused
  t
  "stay focused, dim other cards in the stack"
  :type 'boolean
  :group 'org-roam-stack)

(defcustom org-roam-stack--open-ro nil
  "open all org roam files read only"
  :type 'boolean
  :group 'org-roam-stack)

(defcustom org-roam-stack--maximize-steps '(6 3 2 1 1)
  "animation steps (divisors) for max height"
  :type 'list
  :group 'org-roam-stack)

(defcustom org-roam-stack--local-keybindings
  '(( "C-x C-k"         . org-roam-stack--remove-current-buffer-from-stack)
    ( "C-x k"           . org-roam-stack--remove-current-buffer-from-stack)
    ( "C-x C-f"         . org-roam-stack--find-file)
    ( "s-f"             . org-roam-stack--buffer-or-recent)
    ( "<S-s-return>"    . org-roam-stack--interactive-maximize-current-buffer)
    ( "<C-s-return>"    . org-roam-stack--interactive-maximize-current-buffer)
    ( "<S-s-left>"      . org-roam-stack--void) ;; unbind the given merge behaviour
    ( "<S-s-right>"     . org-roam-stack--void) ;; unbind the given merge behaviour
    ( "<S-s-up>"        . org-roam-stack--merge-current-with-above)
    ( "<S-s-backspace>" . org-roam-stack--interactive-balance-stack)
    ( "<S-s-down>"      . org-roam-stack--merge-current-with-below)
    ( "<M-s-up>"        . org-roam-stack--move-buffer-up)
    ( "<M-s-down>"      . org-roam-stack--move-buffer-down)
    ( "C-x 1"           . org-roam-stack--void) ;; make sure windows are not rearranged into an unknown constellation
    ( "C-x 2"           . org-roam-stack--void) ;; make sure windows are not rearranged into an unknown constellation
    ( "C-x 3"           . org-roam-stack--void) ;; make sure windows are not rearranged into an unknown constellation
    ( "C-x 4"           . org-roam-stack--void) ;; make sure windows are not rearranged into an unknown constellation
    ( "C-x 5"           . org-roam-stack--void) ;; make sure windows are not rearranged into an unknown constellation
    ( "<return>"        . org-roam-stack--return-dwim) ;; make sure to open links even if in view mode
    ( "C-c C-o"         . org-roam-stack--return-dwim) ;; make sure to open links even if in view mode
    )
  "key redefinitions to make sure window constellation of stack is not disrupted.
is a list of pairs '(( KEY_BINDING . FUNCTION ) ...).
e.g. '(( \"C-x C-k\" . org-roam-stack--remove-current-buffer-from-stack ))"
  :type 'list
  :group 'org-roam-stack)

(defvar org-roam-stack--current-card nil
  "card, currently/last viewed")

(defcustom org-roam-stack--roam-link-color
  "pale goldenrod"
  "Color of roam file links."
  :type 'string
  :group 'org-roam-stack)

(defcustom org-roam-stack--pre-anim-maximize-function nil
  "function run before the maximize animation is executed"
  :type 'symbol
  :group 'org-roam-stack)

(defcustom org-roam-stack--post-anim-maximize-function nil
  "function run after the maximize animation was executed"
  :type 'symbol
  :group 'org-roam-stack)

(defface org-roam-stack--roam-link-face
  `((t (:inherit org-link :foreground ,org-roam-stack--roam-link-color)))
  "Face for roam links in org-roam-stack.")

(defvar org-roam-stack--roam-link-re
  "\\(file\\|deft\\|id\\):\\([a-zA-Z0-9_:\\./-]+,?\\)+"
  "Regexp for roam file links.
Group 1 contains the link type.
Group 2 contains the path.")

(defvar org-roam-stack--font-lock-keyword-for-roam-link
  '((org-roam-stack--match-roam-file-link (0  'org-roam-stack--roam-link-face t))))

(defun org-roam-stack--find-file ()
  (interactive)
  (when-let ((file (counsel--find-file-1 "Find file: " nil
                                         nil
                                         'counsel-find-file)))
    (org-roam-stack--open-any-file file)))

(defun org-roam-stack--buffer-or-recent ()
  ""
  (interactive)
  (when-let ((file (ivy-read "Buffer File or Recentf: " (counsel-buffer-or-recentf-candidates)
                             :require-match t
                             :caller 'counsel-buffer-or-recentf)))
    (org-roam-stack--open-any-file file)))

(defun org-roam-stack--return-dwim ()
  "execute return as defined in org mode map!"
  (interactive)
  ;; (org-roam-stack--log 10 "org-roam-stack--return-dwim")
  (ignore-errors
    (org-roam-stack--remove-find-file-advices)
    (unwind-protect
        (progn
          (let* ((context (org-element-context))
                 (type    (org-element-property :type context))
                 (path    (org-element-property :path context)))
            ;; (org-roam-stack--log 10 "org-roam-stack--return-dwim: on id?")
            (cond ((string= type "id")
                   (let ((node (org-roam-populate (org-roam-node-create :id path))))
                     ;; (org-roam-stack--log 10 "org-roam-stack--return-dwim: on id!")
                     (cond
                      ((org-roam-node-file node)
                       (org-mark-ring-push)
                       (org-roam-stack--open (org-roam-node-file node))
                       t)
                      (t nil))))
                  ((and (string= type "file")
                      (org-roam-stack--is-roam-file-p path)
                      (org-roam-file-p path))
                   (org-roam-stack--open-in-stack path))
                  (t
                   (funcall (lookup-key org-mode-map (kbd "<RET>")))))))
      (org-roam-stack--advice-find-file-functions))))

(defun org-roam-stack--execute-buffer-open-resize-strategy ()
  (cl-case org-roam-stack--buffer-open-resize-strategy
    (maximize (org-roam-stack--animated-maximize-current-buffer))
    (balance (org-roam-stack--balance-stack))
    (t nil))
  (recenter)
  (when org-roam-stack--focused
    (org-roam-stack--buffer-change-hook)
    (async-start #'garbage-collect 'ignore) ;; call explicitly since focusing seems to waste a lot of memory
    ))

(defun org-roam-stack--cleanup-stack-list ()
  "remove any elements of the stack list that are no longer valid"
  (setq org-roam-stack--buffer-list
        (--filter (and (not (null it))
                     (bufferp it))
                  org-roam-stack--buffer-list)))

(defun org-roam-stack--buffer-not-in-stack-p (buffer)
  "is the given buffer in the stack?"
  (org-roam-stack--cleanup-stack-list)
  (or
   (null buffer)
   (not (memq buffer org-roam-stack--buffer-list))))

(defun org-roam-stack--file-in-stack (file)
  (org-roam-stack--cleanup-stack-list)
  (--first (string-equal (buffer-file-name it) file) org-roam-stack--buffer-list))

(defun org-roam-stack--buffer-in-stack-p (buffer)
  (org-roam-stack--cleanup-stack-list)
  (not (org-roam-stack--buffer-not-in-stack-p buffer)))

(defun org-roam-stack--insert-buffer-after-existing (existing-buffer inserted-buffer)
  (org-roam-stack--cleanup-stack-list)
  (when-let* ((idx (-elem-index existing-buffer org-roam-stack--buffer-list)))
    (setq org-roam-stack--buffer-list (-insert-at idx inserted-buffer org-roam-stack--buffer-list))))

(defun org-roam-stack--insert-buffer-before-existing (existing-buffer inserted-buffer)
  (org-roam-stack--cleanup-stack-list)
  (when-let* ((idx (-elem-index existing-buffer org-roam-stack--buffer-list)))
    (setq org-roam-stack--buffer-list (-insert-at (1+ idx) inserted-buffer org-roam-stack--buffer-list))))

(defun org-roam-stack--exchange-buffers-in-list (idx-a idx-b mylist)
  "exchange elements in list at position a with position b
idx-a < idx-b!"
  (let* ((elt-a (elt mylist idx-a))
         (elt-b (elt mylist idx-b))
         (list-wo (-remove-at idx-a (-remove-at idx-b mylist))))
    (-insert-at idx-b elt-a (-insert-at idx-a elt-b list-wo))))

(defun org-roam-stack--reset-stack ()
  "close all buffer windows of the stack and clear it"
  (interactive)
  (org-roam-stack--cleanup-stack-list)
  (--each org-roam-stack--buffer-list
    (ignore-errors (delete-window (get-buffer-window it))))
  (setq org-roam-stack--buffer-list '()))

(defvar org-roam-stack--log-level 100 "max level to log messages")
(defun org-roam-stack--log (level msg)
  "log this message if the level is actually wanted"
  (when (<= level org-roam-stack--log-level)
    (message msg)))

(defconst org-roam-stack--TRACE 40 "trace level")

(defun org-roam-stack--open-in-stack (roam-file &optional dir)
  (org-roam-stack--log org-roam-stack--TRACE (format "org-roam-stack--open-in-stack %s" roam-file))
  (if (org-roam-stack--buffer-not-in-stack-p (current-buffer))
      (org-roam-stack--open roam-file)
    (if (org-roam-stack--buffer-in-stack-p (get-file-buffer roam-file))
        (progn
          (org-roam-stack--log org-roam-stack--TRACE (format "org-roam-stack--open-in-stack %s - select-window" roam-file))
          (select-window (get-buffer-window (get-file-buffer roam-file))))
      (org-roam-stack--log org-roam-stack--TRACE (format "org-roam-stack--open-in-stack %s - split window below" roam-file))
      (split-window-below)
      (let ((buffer (current-buffer)))
        (when (eq dir 'below)
          (org-roam-stack--log org-roam-stack--TRACE (format "org-roam-stack--open-in-stack %s - other window, because below" roam-file))
          (other-window 1))
        (org-roam-stack--open-file roam-file)
        (set-window-dedicated-p (selected-window) t)
        (if (eq dir 'below)
            (org-roam-stack--insert-buffer-after-existing buffer (current-buffer))
          (org-roam-stack--insert-buffer-before-existing buffer (current-buffer)))
        (org-roam-stack--execute-buffer-open-resize-strategy)))))

(defun org-roam-stack--open-file (roam-file)
  "open the file and register key bindings and file open hooks"
  (org-roam-stack--log org-roam-stack--TRACE (format "org-roam-stack--open-file %s" roam-file))
  (find-file roam-file)
  (org-roam-stack--register-local-keybindings)
  (org-roam-stack--register-org-roam-stack-find-file)
  (set-window-dedicated-p (selected-window) t)
  (when (and org-roam-stack--open-ro (> (buffer-size (current-buffer)) 0))
    (read-only-mode 1)))

(defun org-roam-stack--open-file-below (roam-file)
  (org-roam-stack--open-in-stack roam-file 'below))

(defun org-roam-stack--open-file-above (roam-file)
  (org-roam-stack--open-in-stack roam-file 'above))

(defun org-roam-stack--get-stack-window-for (roam-file-or-buffer)
  "get the window within the stack (if existent) of the given roam file or buffer"
  (let ((file-buffer (if (bufferp roam-file-or-buffer)
                         roam-file-or-buffer
                       (get-file-buffer roam-file-or-buffer))))
    (when (and (org-roam-stack--buffer-in-stack-p file-buffer)
             (window-valid-p (get-buffer-window file-buffer)))
      (get-buffer-window file-buffer))))

(defun org-roam-stack--enter-stack-from-outside (roam-file)
  "strategy to ensure stack layout if not within the stack currently"
  (org-roam-stack--cleanup-stack-list)
  (if org-roam-stack--buffer-list
      (if-let ((stack-window (org-roam-stack--get-stack-window-for roam-file)))
          (select-window stack-window)
        (when-let ((bottom-most-stack-window (org-roam-stack--get-stack-window-for (car org-roam-stack--buffer-list))))
          (select-window bottom-most-stack-window)
          (ignore-errors
            (enlarge-window 5)) ;; make sure it can be split, ignore errors when sole window
          (split-window-below)
          (other-window 1)))
    (when (windmove-find-other-window 'left)
      (windmove-left))
    (when (not (windmove-find-other-window 'right))
      (when-let (orb (get-buffer-window org-roam-buffer))
        (delete-window orb))
      (split-window-horizontally)
      (when-let (ndw (windmove-find-other-window 'right))
        (with-selected-window ndw
          (with-selected-window
              (split-window-below (round (* 0.7 (window-height))))
            (pop-to-buffer-same-window org-roam-buffer)))))))

(defun org-roam-stack--open-any-file (file)
  (if (org-roam-stack--is-roam-file-p file)
      (org-roam-stack--open file)
    (windmove-right)
    (find-file file)))

(defun org-roam-stack--open (roam-file)
  "open the given file in stack"
  (org-roam-stack--log org-roam-stack--TRACE (format "org-roam-stack--open %s" roam-file))
  (org-roam-stack--cleanup-stack-list)
  (org-roam-stack--enter-stack-from-outside roam-file)
  (when (org-roam-stack--buffer-not-in-stack-p (get-file-buffer roam-file))
    (org-roam-stack--log org-roam-stack--TRACE (format "org-roam-stack--open %s - buffer not in stack" roam-file))
    (org-roam-stack--open-file roam-file)
    (push (get-file-buffer roam-file) org-roam-stack--buffer-list))
  (org-roam-stack--execute-buffer-open-resize-strategy))

(defun org-roam-stack--register-org-roam-stack-find-file ()
  "ensure that org roam stack open is used as find file"
  (setq-local org-link-frame-setup
              (cons '(file . org-roam-stack--open-any-file)
                    (--remove (equal 'file (car it)) org-link-frame-setup))))

(defun org-roam-stack--move-buffer-down ()
  (interactive)
  (org-roam-stack--cleanup-stack-list)
  (when (org-roam-stack--quick-in-stack-p)
    (when-let* ((buffer-below (org-roam-stack--get-buffer-below-existing (current-buffer)))
                (buffer-name-below (buffer-file-name buffer-below))
                (buffer-below-idx (-elem-index buffer-below org-roam-stack--buffer-list))
                (c-buffer-name (buffer-file-name (current-buffer)))
                (c-buffer-idx (-elem-index (current-buffer) org-roam-stack--buffer-list)))
      (org-roam-stack--remove-find-file-advices)
      (unwind-protect
          (progn
            (find-file buffer-name-below)
            (other-window 1)
            (find-file c-buffer-name)
            (setq org-roam-stack--buffer-list (org-roam-stack--exchange-buffers-in-list buffer-below-idx c-buffer-idx org-roam-stack--buffer-list)))
        (org-roam-stack--advice-find-file-functions))
      (org-roam-stack--execute-buffer-open-resize-strategy))))

(defun org-roam-stack--move-buffer-up ()
  (interactive)
  (org-roam-stack--cleanup-stack-list)
  (when (org-roam-stack--quick-in-stack-p)
    (when-let* ((buffer-up (org-roam-stack--get-buffer-above-existing (current-buffer)))
                (buffer-up-name (buffer-file-name buffer-up))
                (window-up (get-buffer-window buffer-up))
                (buffer-up-idx (-elem-index buffer-up org-roam-stack--buffer-list))
                (c-buffer-name (buffer-file-name (current-buffer)))
                (c-buffer-idx (-elem-index (current-buffer) org-roam-stack--buffer-list)))
      (org-roam-stack--remove-find-file-advices)
      (unwind-protect
          (progn
            (find-file buffer-up-name)
            ;;(other-window -1)
            (select-window window-up)
            (find-file c-buffer-name)
            (setq org-roam-stack--buffer-list (org-roam-stack--exchange-buffers-in-list c-buffer-idx buffer-up-idx org-roam-stack--buffer-list)))
        (org-roam-stack--advice-find-file-functions))
      (org-roam-stack--execute-buffer-open-resize-strategy))))

(defun org-roam-stack--buffer-change-hook ()
  (when (org-roam-stack--quick-in-stack-p)
    (when org-roam-stack--focused
      (org-roam-stack--dim-other-buffers)
      (org-roam-stack--undim-buffer (current-buffer)))
    (setq org-roam-stack--current-card (current-buffer))))

(defun org-roam-stack--register-local-keybindings ()
  (use-local-map (copy-keymap (current-local-map)))
  (--each org-roam-stack--local-keybindings
    (local-set-key (kbd (car it)) (cdr it))))

(defun org-roam-stack--report ()
  (message (mapconcat 'identity (--map (buffer-name it) org-roam-stack--buffer-list) ", ")))

(defun org-roam-stack--remove-buffer-from-view-and-stack (buffer)
  "kill the buffer and remove it from view and stack 
if kill is successful return t, return nil otherwise"
  (org-roam-stack--cleanup-stack-list)
  (when-let ((idx-before (-elem-index buffer org-roam-stack--buffer-list)))
    (org-roam-stack--remove-buffer-from-list buffer)
    (let ((win (get-buffer-window buffer))
          (killed (kill-buffer buffer)))
      (if killed
          (progn
            (delete-window win)
            t)
        (setq org-roam-stack--buffer-list (-insert-at idx-before buffer org-roam-stack--buffer-list))
        nil))))

(defun org-roam-stack--merge-current-with-above ()
  (interactive)
  (when (org-roam-stack--quick-in-stack-p)
    (let ((buffer (org-roam-stack--get-buffer-above-existing (current-buffer))))
      (if (buffer-modified-p buffer)
          (message (format "cannot remove modified card: %s" (buffer-name buffer)))
        (if-let (upper-window (windmove-find-other-window 'up))
            (when (equal upper-window (get-buffer-window buffer))
              (org-roam-stack--remove-buffer-from-view-and-stack buffer)
              (org-roam-stack--execute-buffer-open-resize-strategy)))))))

(defun org-roam-stack--merge-current-with-below ()
  (interactive)
  (when (org-roam-stack--quick-in-stack-p)
    (let ((buffer (org-roam-stack--get-buffer-below-existing (current-buffer))))
      (if (buffer-modified-p buffer)
          (message (format "cannot remove modified card: %s" (buffer-name buffer)))
        (if-let (lower-window (windmove-find-other-window 'down))
            (when (equal lower-window (get-buffer-window buffer))
              (org-roam-stack--remove-buffer-from-view-and-stack buffer)
              (org-roam-stack--execute-buffer-open-resize-strategy)))))))

(defun org-roam-stack--void ()
  "do nothing"
  (interactive))

(defun org-roam-stack--get-buffer-below-existing (buffer)
  "get the buffer below the given one (remember list organisation to be reverse)"
  (org-roam-stack--cleanup-stack-list)
  (when-let* ((idx (-elem-index buffer org-roam-stack--buffer-list))
              (pidx (1- idx)))
    (when (>= pidx 0)
      (elt org-roam-stack--buffer-list pidx))))

(defun org-roam-stack--get-buffer-above-existing (buffer)
  "get the buffer above the given one (remember list organisation to be reverse)"
  (org-roam-stack--cleanup-stack-list)
  (when-let* ((idx (-elem-index buffer org-roam-stack--buffer-list))
              (nidx (1+ idx)))
    (when (< nidx (length org-roam-stack--buffer-list))
      (elt org-roam-stack--buffer-list nidx))))

(defun org-roam-stack--remove-buffer-from-list (buffer)
  "remove the given buffer from the stack list"
  (org-roam-stack--cleanup-stack-list)
  (when buffer
    (setq org-roam-stack--buffer-list (remove buffer org-roam-stack--buffer-list))))

(defun org-roam-stack--remove-current-buffer-from-stack (_)
  "remove current buffer from stack, making space for others"
  (interactive "P")
  (when (org-roam-stack--quick-in-stack-p)
      (org-roam-stack--remove-buffer-from-view-and-stack (current-buffer))
      (org-roam-stack--execute-buffer-open-resize-strategy)))

(defun org-roam-stack--interactive-maximize-current-buffer ()
  "maximize buffer and keep this balancing choice for next resize actions"
  (interactive)
  (when (org-roam-stack--quick-in-stack-p)
    (setq org-roam-stack--stack-height (frame-height))
    (if (eq org-roam-stack--buffer-open-resize-strategy 'maximize)
        (setq org-roam-stack--buffer-open-resize-strategy nil)
      (setq org-roam-stack--buffer-open-resize-strategy 'maximize))
    (org-roam-stack--animated-maximize-current-buffer)))

(defun org-roam-stack--animated-maximize-current-buffer ()
  "maximize current buffer, reduce all other stack windows to minimum"
  (run-hooks 'org-roam-stack--pre-anim-maximize-function)
  (-each org-roam-stack--maximize-steps
    (lambda (divisor)
      (setq org-roam-stack--stack-height (/ (frame-height) divisor))
      (ignore-errors
        (when (< 1 (cl-list-length org-roam-stack--buffer-list))
          (enlarge-window org-roam-stack--stack-height)
          (redisplay t)))))
  (run-hooks 'org-roam-stack--post-anim-maximize-function))

(defun org-roam-stack--interactive-balance-stack ()
  "balance stack and keep this balancing choice for next resize actions"
  (interactive)
  (when (org-roam-stack--quick-in-stack-p)
    (if (eq org-roam-stack--buffer-open-resize-strategy 'balance)
        (setq org-roam-stack--buffer-open-resize-strategy nil)
      (setq org-roam-stack--buffer-open-resize-strategy 'balance))
    (org-roam-stack--balance-stack)))

(defun org-roam-stack--balance-stack ()
  "balance (height of) all buffers in the stack"
  (org-roam-stack--cleanup-stack-list)
  (when org-roam-stack--buffer-list
    (ignore-errors
      (setq org-roam-stack--stack-height (frame-height))
      (--dotimes 2 ;; until resize is stable
        (let ((card-window-height (/ (- org-roam-stack--stack-height (cl-list-length org-roam-stack--buffer-list)) (cl-list-length org-roam-stack--buffer-list))))
          (--each (-butlast (reverse org-roam-stack--buffer-list))
            (let* ((its-window (get-buffer-window it))
                   (lines (window-height its-window))
                   (delta (- card-window-height lines)))
              (window-resize (get-buffer-window it) delta))))))))

(defun org-roam-stack--buffer-list-wo-current ()
  "get the list of stack buffers without the current buffer"
  (org-roam-stack--cleanup-stack-list)
  (let ((buffer (current-buffer)))
    (--filter (not (equal buffer it)) org-roam-stack--buffer-list)))

(defun org-roam-stack--dim-other-buffers ()
  "dim all (non selected) stack buffer"
  (interactive)
  (org-roam-stack--cleanup-stack-list)
  (--each (org-roam-stack--buffer-list-wo-current)
    (org-roam-stack--dim-buffer it)))

(defun org-roam-stack--undim-other-buffers ()
  "undim all (non selected) stack buffers"
  (interactive)
  (org-roam-stack--cleanup-stack-list)
  (--each (org-roam-stack--buffer-list-wo-current)
    (org-roam-stack--undim-buffer it)))

(defun org-roam-stack--dim-buffer-old (buffer)
  "put a dim overlay on the visible part of the buffer"
  (with-current-buffer buffer
    (let* ((w (get-buffer-window buffer))
           (focus-overlay (make-overlay (window-start w) (window-end w))))
      (overlay-put focus-overlay 'face 'org-roam-stack-inactive-face)
      (overlay-put focus-overlay 'org-roam-stack-dim t))))

(defun org-roam-stack--dim-buffer (buffer)
  "put a dim overlay on the visible part of the buffer"
  (save-restriction
    (with-current-buffer buffer
      (let* ((w (get-buffer-window buffer)))
        (org-roam-stack--dim-buffer-lines-from (window-start w) (window-end w))))))

(defun org-roam-stack--dim-buffer-lines-from (start end)
  (goto-char start)
  (while (< (point) (min (point-max) end))
    (org-roam-stack--dim-buffer-line (point) end)))

(defun org-roam-stack--dim-buffer-line (start max-end)  
  (beginning-of-line)  
  (if (and (eq ?* (char-after))
         (re-search-forward "^\\*+ " max-end t))
      (org-roam-stack--dim-to-end-of-line (- (point) 2))
    (org-roam-stack--dim-to-end-of-line (point))))

(defun org-roam-stack--dim-to-end-of-line (start)
  (end-of-line)
  (let* ((focus-overlay (make-overlay start (point))))
    (overlay-put focus-overlay 'face 'org-roam-stack-inactive-face)
    (overlay-put focus-overlay 'org-roam-stack-dim t))
  (forward-line)
  (beginning-of-line))

(defun org-roam-stack--undim-buffer (buffer)
  "remove dim overlay of the given buffer"
  (with-current-buffer buffer
    (remove-overlays (point-min) (point-max) 'org-roam-stack-dim t)))

(defun org-roam-stack--protocol-open-file (info)
  (let ((file (plist-get info :file)))
    (org-roam-stack--open-any-file file)))

(defun org-roam-stack--register-open-file-protocol ()
  (setq org-protocol-protocol-alist (--remove (string-equal "roam-file" (plist-get (cdr it) :protocol)) org-protocol-protocol-alist))
  (push '("org-roam-file"  :protocol "roam-file"   :function org-roam-stack--protocol-open-file) org-protocol-protocol-alist))

(defun org-roam-stack--register-open-file-protocol-advice (&rest _)
  (org-roam-stack--register-open-file-protocol))

(defun org-roam-stack--unregister-open-file-protocol ()
  (setq org-protocol-protocol-alist (--remove (string-equal "roam-file" (plist-get (cdr it) :protocol)) org-protocol-protocol-alist))
  (push '("org-roam-file"  :protocol "roam-file"   :function org-roam-protocol-open-file) org-protocol-protocol-alist))

(when (fboundp 'notdeft-note-mode)
  (defun org-roam-stack--notdeft-find-file-advice (_ &rest args)
    (prog1 (org-roam-stack--open (car args))
      (notdeft-note-mode 1))))

(defun org-roam-stack--delete-window-advice (orig-fun &rest args)
  "make sure to delete the buffer of the killed window 
from the stack list of buffers, but only if really killed"
  (org-roam-stack--cleanup-stack-list)
  (if (org-roam-stack--quick-in-stack-p)
      (let* ((window (car args))
             (buffer (window-buffer window)))
        (if (org-roam-stack--buffer-not-in-stack-p buffer)
            (ignore-errors (apply orig-fun args))
          (when-let ((idx-before (-elem-index buffer org-roam-stack--buffer-list))
                     (filename (buffer-file-name buffer)))
            (org-roam-stack--remove-buffer-from-list buffer)
            (ignore-errors (apply orig-fun args))
            ;; check for still active window for the buffer
            (if-let* ((re-buffer (get-file-buffer filename))
                      (re-win (get-buffer-window re-buffer)))
                (setq org-roam-stack--buffer-list (-insert-at idx-before re-buffer org-roam-stack--buffer-list))
              (ignore-errors (kill-buffer-if-not-modified buffer))
              (when (org-roam-stack--quick-in-stack-p)
                (org-roam-stack--execute-buffer-open-resize-strategy))))))
    (ignore-errors (apply orig-fun args))))

(defun org-roam-stack--quick-in-stack-p ()
  "check quickly whether I'm in the org roam stack"
  (and (derived-mode-p 'org-mode) (org-roam-stack--buffer-in-stack-p (current-buffer))))

(defun org-roam-stack--windmove-advice (orig-func &rest args)
  "adjust window in stack actually moved to:
- do resize strategy (if applicable)
- make r/o"
  (let ((should-open-previous-card (and (not (org-roam-stack--quick-in-stack-p))
                                      org-roam-stack--current-card)))
    (apply orig-func args)
    (when (org-roam-stack--quick-in-stack-p)
      (when (and should-open-previous-card
               org-roam-stack--current-card
               (org-roam-stack--buffer-in-stack-p org-roam-stack--current-card))
        (when-let ((sel-win (get-buffer-window org-roam-stack--current-card)))
          (select-window sel-win)))
      (org-roam-stack--execute-buffer-open-resize-strategy)
      (when org-roam-stack--open-ro
        (read-only-mode +1)))))

;; --------------------------------------------------------------------------------

(defun org-roam-stack--get-roam-link-info-at-point ()
  "get information about the roam link at point"
  "unknown")

;;;###autoload
(defun org-roam-stack--is-roam-file-p (&optional file-name)
  "is the given file part of the org roam repo"
  (let ((filename-used (or file-name (buffer-file-name))))
    (and filename-used
       (string-prefix-p org-roam-directory (expand-file-name filename-used))
       (not (string-prefix-p "CAPTURE-" filename-used)))))

;; thanks to the great org-ref package
(defun org-roam-stack--match-roam-file-link (&optional limit)
  "Search forward to next link up to LIMIT."
  (when (and (derived-mode-p 'org-mode)
             (re-search-forward org-roam-stack--roam-link-re limit t)
             ;; make sure we are not in a comment
             (save-excursion
               (beginning-of-line)
               (not (looking-at "# "))))
    (when (version< org-version "9.7.1") (forward-char -2))
    (let ((this-link (org-element-context)))
      (if (and (-contains? '("file" "deft" "id") (org-element-property :type this-link))
             (or (org-roam-stack--is-roam-file-p (org-element-property :path this-link))
                (org-roam-stack--is-roam-file-p (org-roam-node-file (org-roam-node-create :id (org-element-property :path this-link))))))
          (progn
            ;; (add-text-properties
            ;;  (org-element-property :begin this-link)
            ;;  (- (org-element-property :end this-link)
            ;;     (org-element-property :post-blank this-link))
            ;;  (list
            ;;   'help-echo (lambda (window object position)
            ;;                (save-excursion
            ;;                  (goto-char position)
            ;;                  ;; Here we wrap the citation string to a reasonable size.
            ;;                  (let ((s (org-roam-stack--get-roam-link-info-at-point)))
            ;;                    (with-temp-buffer
            ;;                      (insert s)
            ;;                      (fill-paragraph)
            ;;                      (buffer-string)))))))
            (set-match-data
             (list (org-element-property :begin this-link)
                   (- (org-element-property :end this-link)
                      (org-element-property :post-blank this-link))))
            (goto-char (org-element-property :end this-link)))
        ;; Must be a false match.
        (org-roam-stack--match-roam-file-link limit)))))

(defun org-roam-stack--unregister-additional-keywords ()
  (font-lock-remove-keywords 'org-mode org-roam-stack--font-lock-keyword-for-roam-link))

(defun org-roam-stack--register-additional-keywords ()
  "add keywords for colorizing org roam links"
  (font-lock-add-keywords
   'org-mode
   org-roam-stack--font-lock-keyword-for-roam-link
   t))

(defun org-roam-stack--only-notdeft-windows-left-p ()
  (--all? (string= notdeft-buffer it)
          (--map (buffer-name (window-buffer it))
                 (window-list))))

(defun org-roam-stack--view-quit-advice (orig-func &rest args)
  "make sure that quitting view mode within 
org roam stack file, actually removes the stack!"
  (if (org-roam-stack--quick-in-stack-p)
      (let ((next-focus-buffer (or (org-roam-stack--get-buffer-above-existing (current-buffer))
                                  (org-roam-stack--get-buffer-below-existing (current-buffer)))))
        (org-roam-stack--move-buffer-down)
        (org-roam-stack--remove-buffer-from-view-and-stack (current-buffer))
        (unless org-roam-stack--buffer-list
          (when (get-buffer-window org-roam-buffer)
            (org-roam-buffer-toggle)))
        (when (and org-roam-stack--buffer-list
                 next-focus-buffer)
          (org-roam-stack--open-in-stack (buffer-file-name next-focus-buffer)))
        (org-roam-stack--execute-buffer-open-resize-strategy)
        (when (and (< 1 (length (window-list)))
                 (org-roam-stack--only-notdeft-windows-left-p))
          (delete-window (car (window-list)))))
    (ignore-errors (apply orig-func args))))

(defun org-roam-stack--browse-url-advice (orig-func &rest args)
  "make sure browser opened is in rightmost window"
  (when (org-roam-stack--quick-in-stack-p)
    (windmove-right))
  (apply orig-func args))

(defun org-roam-stack--register-browse-url-advice ()
  "install advice around browse url"
  (if (listp browse-url-browser-function)
      (--each browse-url-browser-function
        (advice-add (cdr it) :around #'org-roam-stack--browse-url-advice))
    (advice-add browse-url-browser-function :around #'org-roam-stack--browse-url-advice))
  (when (and (version< emacs-version "28")
           (boundp 'browse-url-handlers)
           (listp browse-url-handlers))
    (--each browse-url-handlers
        (advice-add (cdr it) :around #'org-roam-stack--browse-url-advice))))

(defun org-roam-stack--unregister-browse-url-advice ()
  "remove advice around browse url"
  (if (listp browse-url-browser-function)
      (--each browse-url-browser-function
        (advice-remove (cdr it) #'org-roam-stack--browse-url-advice))
    (advice-remove browse-url-browser-function #'org-roam-stack--browse-url-advice))
  (when (and (version< emacs-version "28")
             (boundp 'browse-url-handlers)
             (listp browse-url-handlers))
    (--each browse-url-handlers
        (advice-remove (cdr it) #'org-roam-stack--browse-url-advice))))

(defun org-roam-stack--advice-find-file-functions ()
  "advice relevant find file functions with org roam stack find file advice"
  (advice-add 'find-file :around #'org-roam-stack--find-file-advice)
  (advice-add 'find-file-noselect :around #'org-roam-stack--find-file-advice))

(defun org-roam-stack--remove-find-file-advices ()
  "remove previously added org roam stack find file functions advice"
    (advice-remove 'find-file #'org-roam-stack--find-file-advice)
    (advice-remove 'find-file-noselect #'org-roam-stack--find-file-advice))

(defun org-roam-stack--find-file-advice (orig-func &rest args)
  "find file on org roam file will use other open file"
  (if (and (file-exists-p (car args)) ;; org roam new nodes are not existent at this point
         (org-roam-stack--is-roam-file-p (car args)))
      (progn
        (org-roam-stack--remove-find-file-advices)
        (org-roam-stack--open (car args))
        (org-roam-stack--advice-find-file-functions))
    (apply orig-func args)))

(defun org-roam-stack--cleanup-kill-buffers ()
  "remove buffers from org-roam-stack--buffer-list that are not live any more"
  (org-roam-stack--cleanup-stack-list)
  (setq org-roam-stack--buffer-list
        (--filter (and (buffer-live-p it)
                     (window-live-p (get-buffer-window it)))
                  org-roam-stack--buffer-list)))

(defun org-roam-stack--restore-stack-view ()
  "restore view of org roam stack"
  (interactive)
  (org-roam-stack--cleanup-kill-buffers)
  (let ((files (--map (buffer-file-name it) org-roam-stack--buffer-list))
        (file (when org-roam-stack--current-card (buffer-file-name org-roam-stack--current-card))))
    (--each (window-list (selected-frame))
      (when (with-selected-window it
              (and (stringp mode-name)
                 (string-equal "Org" mode-name )
                 (not (org-roam-stack--file-in-stack (buffer-file-name (window-buffer it))))))
        (delete-window it)))
    (when (and (fboundp 'notdeft)
             (not (--first (string-equal "NotDeft" (with-selected-window it (if (stringp mode-name) mode-name "")))
                         (window-list (selected-frame)))))
      (notdeft nil))
    (--each (reverse files) (org-roam-stack--file-in-stack it))
    (when (and file
             (org-roam-stack--file-in-stack file))
      (org-roam-stack--open file))))

(defun org-roam-stack--switch-to-rw-mode (&rest args)
  "switch org roam stack buffer to r/w mode"
  (read-only-mode -1))

(defun org-roam-stack--node-visit (node &optional other-window force)
  "From the current buffer, visit NODE."
  (interactive (list (org-roam-node-at-point t) current-prefix-arg t))
  (org-roam-stack--remove-find-file-advices)
  (unwind-protect
      (org-roam-stack--open-in-stack (org-roam-node-file node))
    (org-roam-stack--advice-find-file-functions)))

;; allow to wrap org source edit, making editing source in same window possible
(defun org-roam-stack--release-window-dedication (&rest args)
  (set-window-dedicated-p (selected-window) nil))

(defun org-roam-stack--set-window-dedication (&rest args)
  (set-window-dedicated-p (selected-window) t))

;; --------------------------------------------------------------------------------

;;;###autoload
(define-minor-mode org-roam-stack-mode
  "Minor mode to organize org roam cards in a stack"
  :global t
  (if org-roam-stack-mode
      (progn
        (org-roam-stack--register-open-file-protocol)
        (advice-add 'org-edit-special :before #'org-roam-stack--release-window-dedication)
        (advice-add 'org-edit-src-exit :after #'org-roam-stack--set-window-dedication)
        (advice-add 'delete-window :around #'org-roam-stack--delete-window-advice)
        (advice-add 'View-quit :around #'org-roam-stack--view-quit-advice)
        (advice-add 'org-ctrl-c-ctrl-c :before #'org-roam-stack--switch-to-rw-mode)
        (when org-roam-stack--link-adjustments
          (org-roam-stack--register-additional-keywords))
        (when (functionp 'windmove-up)
          (advice-add 'windmove-up :around #'org-roam-stack--windmove-advice)
          (advice-add 'windmove-right :around #'org-roam-stack--windmove-advice)
          (advice-add 'windmove-left :around #'org-roam-stack--windmove-advice)
          (advice-add 'windmove-down :around #'org-roam-stack--windmove-advice))
        (bind-key "s-d" #'org-roam-stack--restore-stack-view)
        (bind-key "RET" #'org-roam-stack--node-visit 'org-roam-node-map)
        (org-roam-stack--advice-find-file-functions)
        (setq org-pretty-entities t)
        (setq org-pretty-entities-include-sub-superscripts t))

    (org-roam-stack--unregister-open-file-protocol)
    (advice-remove 'org-edit-special #'org-roam-stack--release-window-dedication)
    (advice-remove 'org-edit-src-exit #'org-roam-stack--set-window-dedication)
    (advice-remove 'delete-window #'org-roam-stack--delete-window-advice)
    (advice-remove 'View-quit #'org-roam-stack--view-quit-advice)
    (advice-remove 'org-ctrl-c-ctrl-c #'org-roam-stack--switch-to-rw-mode)
    (org-roam-stack--unregister-additional-keywords)
    (when (functionp 'windmove-up)
      (advice-remove 'windmove-up #'org-roam-stack--windmove-advice)
      (advice-remove 'windmove-right #'org-roam-stack--windmove-advice)
      (advice-remove 'windmove-left #'org-roam-stack--windmove-advice)
      (advice-remove 'windmove-down #'org-roam-stack--windmove-advice))
    (when (fboundp 'notdeft)
      (bind-key "s-d" #'notdeft))
    (bind-key "RET" #'org-roam-node-visit 'org-roam-node-map)
    (org-roam-stack--remove-find-file-advices)
 ))

(provide 'org-roam-stack)
