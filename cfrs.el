;;; cfrs.el --- child-frame based read-string -*- lexical-binding: t -*-

;; Copyright (C) 2019 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "25.2") (dash "2.11.0") (s "1.10.0") (posframe "0.4.3"))
;; Package-Version: 1.1
;; Homepage: https://github.com/Alexander-Miller/cfrs

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
;;; Simple implementation of reading a string with child-frames.
;;; Synchronous control is maintained by using `recursive-edit'. When finished the
;;; entered text is read from the input buffer and the child-frame is hidden.

;;; Code:

(require 's)
(require 'dash)
(require 'posframe)

;;;###autoload
(defun cfrs-read (prompt &optional initial-input)
  "Read a string using a pos-frame with given PROMPT and INITIAL-INPUT."
  (-let [buffer (get-buffer-create " *Pos-Frame-Read*")]
    (posframe-show buffer
      :height 1
      :width (+ 40 (length prompt))
      :internal-border-width 1
      :string "")
    (-let [posfr (-> buffer (get-buffer-window :all-frame) (window-frame))]
      (x-focus-frame posfr)
      (add-hook 'delete-frame-functions #'cfrs--on-frame-kill nil :local)
      (with-current-buffer buffer
        (display-line-numbers-mode -1)
        (cfrs-input-mode)
        (-each (overlays-in (point-min) (point-max)) #'delete-overlay)
        (erase-buffer)
        (-doto (make-overlay 1 2)
          (overlay-put 'before-string (propertize prompt 'face 'minibuffer-prompt))
          (overlay-put 'rear-nonsticky t)
          (overlay-put 'read-only t))
        (when initial-input
          (insert initial-input))
        (recursive-edit)
        (cfrs--hide)
        (s-trim (buffer-string))))))

(defun cfrs--hide ()
  "Hide the current cfrs frame."
  (when (eq major-mode 'cfrs-input-mode)
    (posframe-hide (current-buffer))
    (x-focus-frame (frame-parent (selected-frame)))))

(defun cfrs--on-frame-kill (frame)
  "Redirect focus after FRAME is killed."
  (-let [parent (or (frame-parent frame) (selected-frame))]
    (x-focus-frame parent)))

(defun cfrs-finish ()
  "Finish the cfrs read, returning the entered string."
  (interactive)
  (exit-recursive-edit))

(defun cfrs-cancel ()
  "Cancel the `cfrs-read' call and the function that called it."
  (interactive)
  (cfrs--hide)
  (abort-recursive-edit))

(defvar cfrs-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'cfrs-finish)
    (define-key map [return] #'cfrs-finish)
    (define-key map [remap keyboard-quit] #'cfrs-cancel)
    map))

(define-derived-mode cfrs-input-mode fundamental-mode "Child Frame Read String"
  "Simple mode for buffers displayed in cfrs's input frames.")

(provide 'cfrs)

;;; cfrs.el ends here
