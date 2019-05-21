;;; pfr.el --- pos-frame-based read-string -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "25.2") (dash "2.11.0") (s "1.10.0") (posframe "0.4.3"))
;; Package-Version: 0.1
;; Homepage: https://github.com/Alexander-Miller/treemacs

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
;;; Simple implementation of reasing a string with child-frames.
;;; Synchronous control is maintained by using `recursive-edit'. When finished the
;;; entered value is saved in `pfr--slot' and then returned when the recursive edit
;;; is finished.

;;; Code:

(require 's)
(require 'dash)
(require 'posframe)

(defvar pfr--slot nil)

(defun pfr-read (prompt &optional initial-input)
  "Read a string using a pos-frame with given PROMPT and INITIAL-INPUT."
  (-let [buffer (get-buffer-create " *Pos-Frame-Read*")]
    (posframe-show buffer
      :height 1
      :width (+ 40 (length prompt))
      :internal-border-width 1
      :string "")
    (-let [posfr (-> buffer (get-buffer-window :all-frame) (window-frame))]
      (x-focus-frame posfr)
      (with-current-buffer buffer
        (display-line-numbers-mode -1)
        (pfr-input-mode)
        (-each (overlays-in (point-min) (point-max)) #'delete-overlay)
        (erase-buffer)
        (-doto (make-overlay 1 2)
          (overlay-put 'before-string (propertize prompt 'face 'minibuffer-prompt))
          (overlay-put 'rear-nonsticky t)
          (overlay-put 'read-only t))
        (when initial-input
          (insert initial-input))
        (recursive-edit)
        pfr--slot))))

(defun pfr--hide ()
  "Hide the current pfr frame."
  (when (eq major-mode 'pfr-input-mode)
    (posframe-hide (current-buffer))
    (x-focus-frame (frame-parent (selected-frame)))))

(defun pfr-finish ()
  "Finish the pfr read, returning the entered string."
  (interactive)
  (let ((txt (buffer-string)))
    (pfr--hide)
    (setq pfr--slot txt)
    (exit-recursive-edit)))

(defun pfr-cancel ()
  "Cancel the `pfr-read' call and the function that called it."
  (interactive)
  (pfr--hide)
  (abort-recursive-edit))

(defvar pfr-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pfr-finish)
    (define-key map [return] #'pfr-finish)
    (define-key map [remap keyboard-quit] #'pfr-cancel)
    map))

(define-derived-mode pfr-input-mode fundamental-mode "Pos Frame Completing Read"
  "Simple mode for buffers displayed in pfr's input frames.")

(provide 'pfr)

;;; pfr.el ends here
