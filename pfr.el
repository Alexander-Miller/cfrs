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
;;; TODO

;;; Code:

(require 's)
(require 'dash)
(require 'posframe)

(defvar pfr--slot nil)
(defvar-local pfr--prompt-ov nil)

(defun pfr-read (prompt &optional initial-input)
  "Read a string using a pos-frame with given PROMPT and INITIAL-INPUT."
  (-let [buffer (get-buffer-create "*Pos-Frame-Read*")]
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
        (-let [ov (setq-local pfr--prompt-ov (make-overlay 1 2))]
          (put-text-property 0 (length prompt) 'face 'minibuffer-prompt prompt)
          (overlay-put ov 'before-string prompt)
          (overlay-put ov 'rear-nonsticky t)
          (overlay-put ov 'read-only t))
        (when initial-input
          (insert initial-input))
        (recursive-edit)
        pfr--slot))))

(defun pfr-finish ()
  "Finish the pfr read, returning the entered string."
  (interactive)
  (let ((parent (frame-parent (selected-frame)))
        (txt (buffer-string)))
    (posframe-hide (current-buffer))
    (delete-overlay pfr--prompt-ov)
    (x-focus-frame parent)
    (setq pfr--slot txt)
    (exit-recursive-edit)))

(defvar pfr-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pfr-finish)
    (define-key map [return]        #'pfr-finish)
    map))

(define-derived-mode pfr-input-mode fundamental-mode "Pos Frame Completing Read"
  "Simple mode for buffers displayed in pfr's input frames.")

(provide 'pfr)

;;; pfr.el ends here
