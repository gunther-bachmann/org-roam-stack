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
   (require 'cl))
(require 'dash)
(require 's)

(require 'windmove)

(require 'ol)
(require 'org-protocol)
(require 'notdeft)

(defgroup org-roam-stack nil
  "organize org roam cards in a stack"
  :group 'org-roam)

(defvar org-roam-stack--stack-height (window-height (get-buffer-window))
  "height of the stack used for balance window calculations")

(defvar org-roam-stack--buffer-list '()
  "list of buffers that form the stack.
they are ordered from bottom to top => adding a buffer to the bottom of the stack puts it at the head of this list.
this list is kept in sync with the visual display of the stack.
if by some commands the list gets out of sync, org-roam-stack--restore-stack can be used to build up the visual according the this list")

(defvar org-roam-stack--buffer-open-resize-strategy 'maximize
  "either 'maximize or 'balance")

(defvar org-roam-stack--local-keybindings
  '(( "C-x C-k"         . org-roam-stack--remove-current-buffer-from-stack)
    ( "C-x k"           . org-roam-stack--remove-current-buffer-from-stack)
    ( "<S-s-return>"    . org-roam-stack--maximize-current-buffer)
    ( "<C-s-return>"    . org-roam-stack--maximize-current-buffer)
    ( "<S-s-left>"      . org-roam-stack--void)   ;; unbind the given merge behaviour
    ( "<S-s-right>"     . org-roam-stack--void) ;; unbind the given merge behaviour
    ( "<S-s-up>"        . org-roam-stack--merge-current-with-above)
    ( "<S-s-backspace>" . org-roam-stack--balance-stack)
    ( "<S-s-down>"      . org-roam-stack--merge-current-with-below)
    ( "<M-s-up>"        . org-roam-stack--move-buffer-up)
    ( "<M-s-down>"      . org-roam-stack--move-buffer-down)))

(defvar org-roam-stack--focused t
  "stay focused, dim other cards in the stack")

(defun org-roam-stack--execute-buffer-open-resize-strategy ()
  (case org-roam-stack--buffer-open-resize-strategy
    ('(maximize) (org-roam-stack--maximize-current-buffer))
    ('(balance) (org-roam-stack--balance-stack))))

(defun org-roam-stack--buffer-not-in-stack-p (buffer)
  "is the given buffer in the stack?"
  (--none-p (s-equals-p (buffer-name buffer)
                        (buffer-name it))
            org-roam-stack--buffer-list))

(defun org-roam-stack--buffer-in-stack-p (buffer)
  (not (org-roam-stack--buffer-not-in-stack-p buffer)))

(defun org-roam-stack--insert-buffer-after-existing (existing-buffer inserted-buffer)
  (when-let* ((idx (-elem-index existing-buffer org-roam-stack--buffer-list)))
    (setq org-roam-stack--buffer-list (-insert-at idx inserted-buffer org-roam-stack--buffer-list))))

(defun org-roam-stack--insert-buffer-before-existing (existing-buffer inserted-buffer)
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
  (--each org-roam-stack--buffer-list
    (ignore-errors (delete-window (get-buffer-window it))))
  (setq org-roam-stack--buffer-list '()))

(defun org-roam-stack--open-in-stack (roam-file &optional dir)
  (if (org-roam-stack--buffer-not-in-stack-p (current-buffer))
      (org-roam-stack--open roam-file)
    (split-window-below)
    (let ((buffer (current-buffer)))
      (when (eq dir 'below)
        (other-window 1))
      (org-roam-stack--open-file roam-file)
      (if (eq dir 'below)
          (org-roam-stack--insert-buffer-after-existing buffer (current-buffer))
        (org-roam-stack--insert-buffer-before-existing buffer (current-buffer)))
      (org-roam-stack--execute-buffer-open-resize-strategy))))

(defun org-roam-stack--open-file (roam-file)
  "open the file and register key bindings and file open hooks"
  (find-file roam-file)
  (org-roam-stack--register-local-keybindings)
  (org-roam-stack--register-org-roam-stack-find-file))

(defun org-roam-stack--open-file-below (roam-file)
  (org-roam-stack--open-in-stack roam-file 'below))

(defun org-roam-stack--open-file-above (roam-file)
  (org-roam-stack--open-in-stack roam-file 'above))

(defun org-roam-stack--enter-stack-from-outside ()
  "strategy to ensure stack layout if not within the stack currently"
  (if org-roam-stack--buffer-list
      (progn
        (when-let ((bwin (get-buffer-window (car org-roam-stack--buffer-list))))
          (when (window-valid-p bwin)
            (select-window bwin)
            (enlarge-window 5) ;; make sure it can be split
            (split-window-below)
            (other-window 1))))
    (when (windmove-find-other-window 'left)
      (windmove-left))
    (when (not (windmove-find-other-window 'right))
      (split-window-horizontally))))

(defun org-roam-stack--open (roam-file)
  "open the given file in stack"
  (org-roam-stack--enter-stack-from-outside)
  (org-roam-stack--open-file roam-file)
  (push (get-file-buffer roam-file) org-roam-stack--buffer-list)
  (org-roam-stack--execute-buffer-open-resize-strategy))

(defun org-roam-stack--register-org-roam-stack-find-file ()
  "ensure that org roam stack open is used as find file"
  (setq-local org-link-frame-setup
              (cons '(file . org-roam-stack--open)
                    (--remove (equal 'file (car it)) org-link-frame-setup))))

(defun org-roam-stack--move-buffer-down ()
  (interactive)
  (when-let* ((buffer-below (org-roam-stack--get-buffer-below-existing (current-buffer)))
              (buffer-name-below (buffer-file-name buffer-below))
              (buffer-below-idx (-elem-index buffer-below org-roam-stack--buffer-list))
              (c-buffer-name (buffer-file-name (current-buffer)))
              (c-buffer-idx (-elem-index (current-buffer) org-roam-stack--buffer-list)))
    (find-file buffer-name-below)
    (other-window 1)
    (find-file c-buffer-name)
    (setq org-roam-stack--buffer-list (org-roam-stack--exchange-buffers-in-list buffer-below-idx c-buffer-idx org-roam-stack--buffer-list))
    (org-roam-stack--execute-buffer-open-resize-strategy)))

(defun org-roam-stack--move-buffer-up ()
  (interactive)
  (when-let* ((buffer-up (org-roam-stack--get-buffer-above-existing (current-buffer)))
              (buffer-up-name (buffer-file-name buffer-up))
              (buffer-up-idx (-elem-index buffer-up org-roam-stack--buffer-list))
              (c-buffer-name (buffer-file-name (current-buffer)))
              (c-buffer-idx (-elem-index (current-buffer) org-roam-stack--buffer-list)))
    (find-file buffer-up-name)
    (other-window -1)
    (find-file c-buffer-name)
    (setq org-roam-stack--buffer-list (org-roam-stack--exchange-buffers-in-list c-buffer-idx buffer-up-idx org-roam-stack--buffer-list))
    (org-roam-stack--execute-buffer-open-resize-strategy)))

(defun org-roam-stack--buffer-change-hook ()
  (when (and org-roam-stack--focused (org-roam-stack--buffer-in-stack-p (current-buffer)))
    (org-roam-stack--dim-other-buffers)
    (org-roam-stack--undim-buffer (current-buffer))))

(defun org-roam-stack--register-local-keybindings ()
  (use-local-map (copy-keymap (current-local-map)))
  (--each org-roam-stack--local-keybindings
    (local-set-key (kbd (car it)) (cdr it))))

(defun org-roam-stack--restore-stack ()
  (interactive)
  (when org-roam-stack--buffer-list
    (delete-other-windows)
    (split-window-horizontally)
    (find-file (buffer-file-name (car (last org-roam-stack--buffer-list))))
    (--each (-drop 1 (reverse org-roam-stack--buffer-list))
      (ignore-errors
        (enlarge-window 10) ;; ensure split is possible
        (split-window-vertically)
        (other-window 1)
        (find-file (buffer-file-name it))))
    (org-roam-stack--balance-stack)))

(defun org-roam-stack--report ()
  (message (s-join ", " (--map (buffer-name it) org-roam-stack--buffer-list))))

(defun org-roam-stack--remove-buffer-from-view-and-stack (buffer)
  "kill the buffer and remove it from view and stack if kill is successful return t, return nil otherwise"
  (when-let ((idx-before (-elem-index buffer org-roam-stack--buffer-list)))
    (org-roam-stack--remove-buffer-from-list buffer)
    (let ((killed (kill-buffer buffer)))
      (if killed
          (progn
            (delete-window (get-buffer-window buffer))
            t)
        (setq org-roam-stack--buffer-list (-insert-at idx-before buffer org-roam-stack--buffer-list))
        nil))))

(defun org-roam-stack--merge-current-with-above ()
  (interactive)
  (let ((buffer (org-roam-stack--get-buffer-above-existing (current-buffer))))
    (if (buffer-modified-p buffer)
        (message (format "cannot remove modified card: %s" (buffer-name buffer)))
      (if-let (upper-window (windmove-find-other-window 'up))
          (when (equal upper-window (get-buffer-window buffer))
            (org-roam-stack--remove-buffer-from-view-and-stack buffer))))))

(defun org-roam-stack--merge-current-with-below ()
  (interactive)
  (let ((buffer (org-roam-stack--get-buffer-below-existing (current-buffer))))
    (if (buffer-modified-p buffer)
        (message (format "cannot remove modified card: %s" (buffer-name buffer)))
      (if-let (lower-window (windmove-find-other-window 'down))
          (when (equal lower-window (get-buffer-window buffer))
            (org-roam-stack--remove-buffer-from-view-and-stack buffer))))))

(defun org-roam-stack--void ()
  "do nothing"
  (interactive))

(defun org-roam-stack--get-buffer-below-existing (buffer)
  "get the buffer below the given one (remember list organisation to be reverse)"
  (when-let* ((idx (-elem-index buffer org-roam-stack--buffer-list))
              (pidx (1- idx)))
    (when (>= pidx 0)
      (elt org-roam-stack--buffer-list pidx))))

(defun org-roam-stack--get-buffer-above-existing (buffer)
  "get the buffer above the given one (remember list organisation to be reverse)"
  (when-let* ((idx (-elem-index buffer org-roam-stack--buffer-list))
              (nidx (1+ idx)))
    (when (< nidx (length org-roam-stack--buffer-list))
      (elt org-roam-stack--buffer-list nidx))))

(defun org-roam-stack--remove-buffer-from-list (buffer)
  "remove the given buffer from the stack list"
  (when buffer
    (setq org-roam-stack--buffer-list (remove buffer org-roam-stack--buffer-list))))

(defun org-roam-stack--remove-current-buffer-from-stack (_)
  "remove current buffer from stack, making space for others"
  (interactive "P")
  (org-roam-stack--remove-buffer-from-view-and-stack (current-buffer)))

(defun org-roam-stack--maximize-current-buffer ()
  "maximize current buffer, reduce all other stack windows to minimum"
  (interactive)
  (ignore-errors
    (--dotimes 2
      (when (< 1 (cl-list-length org-roam-stack--buffer-list))
        (enlarge-window org-roam-stack--stack-height)))))

(defun org-roam-stack--balance-stack ()
  "balance (height of) all buffers in the stack"
  (interactive)
  (--dotimes 2
    (let ((card-window-height (/ (- org-roam-stack--stack-height (cl-list-length org-roam-stack--buffer-list)) (cl-list-length org-roam-stack--buffer-list))))
      (--each (-butlast (reverse org-roam-stack--buffer-list))
        (let* ((its-window (get-buffer-window it))
               (lines (window-height its-window))
               (delta (- card-window-height lines)))
          (message (format "buffer: %s, current: %d, new: %d, delta: %d" (buffer-name it) lines card-window-height delta))
          (window-resize (get-buffer-window it) delta))))))

(defun org-roam-stack--dim-other-buffers ()
  "dim all (non selected) stack windows"
  (interactive)
  (let* ((buffer (current-buffer))
         (list-without-current (--filter (not (equal buffer it)) org-roam-stack--buffer-list)))
    (--each list-without-current
      (org-roam-stack--dim-buffer it))))

(defun org-roam-stack--dim-buffer (buffer)
  (with-current-buffer buffer
    (let ((focus-overlay (make-overlay (point-min) (point-max))))
      (overlay-put focus-overlay 'face 'font-lock-comment-face)
      (overlay-put focus-overlay 'org-roam-stack-dim t))))

(defun org-roam-stack--undim-buffer (buffer)
  (with-current-buffer buffer
    (remove-overlays (point-min) (point-max) 'org-roam-stack-dim t)))

(defun org-roam-stack--undim-other-buffers ()
  "dim all (non selected) stack windows"
  (interactive)
  (let* ((buffer (current-buffer))
         (list-without-current (--filter (not (equal buffer it)) org-roam-stack--buffer-list)))
    (--each list-without-current
      (org-roam-stack--undim-buffer it))))

(defun org-roam-stack--protocol-open-file (info)
  (let ((file (plist-get info :file)))
    (message (format "org-roam-protocol-open-file %s" file))
    (org-roam-stack--open file)))

(defun org-roam-stack--register-open-file-protocol ()
  (setq org-protocol-protocol-alist (--remove (string-equal "roam-file" (plist-get (cdr it) :protocol)) org-protocol-protocol-alist))
  (push '("org-roam-file"  :protocol "roam-file"   :function org-roam-stack--protocol-open-file) org-protocol-protocol-alist))

(defun org-roam-stack--register-open-file-protocol-advice (&rest _)
  (org-roam-stack--register-open-file-protocol))

(defun org-roam-stack--unregister-open-file-protocol ()
  (setq org-protocol-protocol-alist (--remove (string-equal "roam-file" (plist-get (cdr it) :protocol)) org-protocol-protocol-alist))
  (push '("org-roam-file"  :protocol "roam-file"   :function org-roam-protocol-open-file) org-protocol-protocol-alist))

(defun org-roam-stack--notdeft-find-file-advice (_ &rest args)
  (message "called")
  (prog1 (org-roam-stack--open (car args))
    (notdeft-note-mode 1)))

;; experimental code
(when nil
  (org-roam-stack--register-open-file-protocol)

  (setq-local org-link-frame-setup (cons '(file . org-roam-stack--open) org-link-frame-setup))

  (pop org-roam-stack--buffer-list)
  (setq org-roam-stack--buffer-list nil)
  (org-roam-stack--report)
  (org-roam-stack--register-open-file-protocol)

  (org-roam-stack--open "~/documents/roam/dunst.org")
  (org-roam-stack--open "~/documents/roam/machine-setup.org")
  (org-roam-stack--open "~/documents/roam/jvm.org")
  (org-roam-stack--open "~/documents/roam/racket.org")
  (org-roam-stack--open "~/documents/roam/elisp.org")

  (org-roam-stack--balance-stack))

;;;###autoload
(define-minor-mode org-roam-stack-mode
  "Minor mode to organize org roam cards in a stack"
  :lighter " ors"
  :global t
  (if org-roam-stack-mode
      (progn
        (org-roam-stack--register-open-file-protocol)
        (add-hook 'buffer-list-update-hook #'org-roam-stack--buffer-change-hook)
        (when (functionp 'notdeft-find-file)
          (advice-add 'notdeft-find-file :around 'org-roam-stack--notdeft-find-file-advice))
        ;; make sure that starting the org roam server does not place another :roam-file entry!
        (when (functionp 'org-roam-server-mode)
          (advice-add 'org-roam-server-mode :after 'org-roam-stack--register-open-file-protocol-advice)))

    (org-roam-stack--unregister-open-file-protocol)
    (remove-hook 'buffer-list-update-hook #'org-roam-stack--buffer-change-hook)
    (when (functionp 'notdeft-find-file)
      (advice-remove 'notdeft-find-file 'org-roam-stack--notdeft-find-file-advice))))

(provide 'org-roam-stack)