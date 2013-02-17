;;; lv.el --- Log filter and highlighter

;; Copyright (C) 2012-2013 Lluís Vilanova

;; Author: Lluís Vilanova <vilanova@ac.upc.edu>
;; URL: https://github.com/llvilanova/lv
;; Keywords: log, visualizer
;; Version: 1.0

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
;;
;; This extension eases the visualisation of logs by showing lines and
;; highlighting their contents based on regular expressions.
;;
;; To activate it:
;;   (require 'lv)
;;   (lv-mode t)
;;
;; To use it, see the help of `lv-mode'.

;;; Code:



;;;; Customization

(defgroup lv nil
  "Log filter and highlighter."
  :prefix "lv-"
  :group 'convenience)

(defcustom lv-colors '("yellow" "DeepPink" "cyan" "MediumPurple1" "SpringGreen1"
                       "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab")
  "List of colors to use for highlighting."
  :group 'lv)



;;;; Internal buffer state

(defvar lv--active t
  "Whether `lv-mode' is currently active on this buffer.")
(make-variable-buffer-local 'lv--active)

(defvar lv--elems nil
  "List of elements to filter/highlight.
Each element is a cons cell with a regexp and an index to `lv-colors'.")
(make-variable-buffer-local 'lv--elems)

(defvar lv--count 0
  "Number of elements used so far.
This number is used by `lv-add' to uniquely assign an element of
`lv-colors' to each element in `lv--elems'.")
(make-variable-buffer-local 'lv--count)

(defvar lv--limit nil
  "List of regexps to select which lines matching `lv--elems' must be shown.
When empty, shows all lines matching `lv--elems'.")
(make-variable-buffer-local 'lv--limit)



;;;; Machinery

(defun lv--prepare ()
  "Prepare call to `lv--update'.
Must be called before `lv--update' and before changing `lv--elems'. Unfilters
all lines and unhighlights all elements."
  (mapc
   (lambda (elem) (delete-overlay elem))
   (overlays-in (point-min) (point-max)))
  (when lv--active
    (mapc
     (lambda (elem) (unhighlight-regexp (car elem)))
     lv--elems)))

(defun lv--update ()
  "Apply the current settings in `lv--elems' and `lv--limit'."
  (when (> (length lv--elems) 0)
    (when lv--active
      (let ((expr (mapconcat (lambda (entry) (format "\\(%s\\)" (car entry)))
                             lv--elems
                             "\\|"))
            (limit (mapconcat (lambda (entry) (format "\\(%s\\)" entry))
                              lv--limit
                              "\\|")))
        (save-excursion
          (goto-char (point-min))
          (let ((prev (point))
                this-start this-end prev-start prev-end text)
            (while (re-search-forward expr nil t)
              (setq this-start (line-beginning-position)
                    this-end   (1+ (line-end-position))
                    prev-start prev
                    prev-end   (line-beginning-position)
                    prev       (1+ (line-end-position))
                    text       (buffer-substring this-start this-end))
              (overlay-put (make-overlay prev-start prev-end) 'invisible t)
              (when (> (length limit) 0)
                (unless (string-match limit text)
                  (overlay-put (make-overlay this-start this-end)
                               'invisible t)))
              (goto-char this-end))
            (overlay-put (make-overlay prev (point-max)) 'invisible t)))))
    (mapc
     (lambda (elem)
       (let* ((re    (car elem))
              (idx   (cdr elem))
              (midx  (mod (cdr elem) (length lv-colors)))
              (color (nth midx lv-colors))
              (face  (make-face (make-symbol (format "lv--%s" color)))))
         (set-face-foreground face "black")
         (set-face-background face color)
         (highlight-regexp re face)))
     lv--elems)))

(defun lv-add (re)
  "Add an element to `lv--elems'."
  (lv-del re)
  (add-to-list 'lv--elems (cons re lv--count))
  (setq lv--count (1+ lv--count))
  (lv--prepare)
  (lv--update))

(defun lv-del (re)
  "Remove an element from `lv--elems'."
  (lv--prepare)
  (setq lv--elems
        (delq nil
              (mapcar (lambda (elem) (if (string= (car elem) re) nil elem))
                      lv--elems)))
  (lv--update))



;;;; Interface

(defvar lv--re nil
  "List of regexps to select and highlight lines.")
(make-variable-buffer-local 'lv--re)

(defvar lv--re-history nil
  "List of arguments to `lv-add-re' so far.")

;;;###autoload
(defun lv-add-re (&optional re)
  "Add a regexp to select and highlight lines."
  (interactive)
  (when (called-interactively-p 'interactive)
    (setq re (completing-read "Regexp: " nil nil nil nil
                              'lv--re-history)))
  (add-to-list 'lv--re-history re t)
  (add-to-list 'lv--re re)
  (lv-add re))

;;;###autoload
(defun lv-del-re (&optional re)
  "Remove one of the current regexps to select and highlight lines."
  (interactive)
  (when (called-interactively-p 'interactive)
    (setq re (completing-read "Regexp: "
                              lv--re)))
  (setq lv--re (remove re lv--re))
  (lv-del re))


(defvar lv--limit-history nil
  "List of arguments to `lv-add-limit' so far.")

;;;###autoload
(defun lv-add-limit (&optional re)
  "Limit selected lines to those matching the given regexp."
  (interactive)
  (when (called-interactively-p 'interactive)
    (setq re (completing-read "Regexp: " nil nil nil nil
                              'lv--limit-history)))
  (add-to-list 'lv--limit-history re t)
  (add-to-list 'lv--limit re)
  (lv--prepare)
  (lv--update))

;;;###autoload
(defun lv-del-limit (&optional re)
  "Unlimit selected lines to those matching the given regexp."
  (interactive)
  (when (called-interactively-p 'interactive)
    (setq re (completing-read "Regexp: "
                              lv--limit)))
  (setq lv--limit (remove re lv--limit))
  (lv--prepare)
  (lv--update))

;;;###autoload
(defun lv-toggle ()
  "Temporarily (de)activate the effects of `lv-mode'."
  (interactive)
  (setq lv--active (not lv--active))
  (lv--prepare)
  (lv--update)
  (if lv--active
      (message "LV active")
    (message "LV inactive")))

;;;###autoload
(defun lv-reset ()
  "Remove all settings for `lv-mode' in current buffer."
  (interactive)
  (setq lv--active t)
  (lv--prepare)
  (setq lv--elems nil
        lv--count 0
        lv--limit nil)
  (lv--update))



;;;; Minor mode setup

(defvar lv-mode-map (make-sparse-keymap)
  "Keymap for `lv-mode'.")
(define-key lv-mode-map (kbd "C-c t") 'lv-toggle)
(define-key lv-mode-map (kbd "C-c r") 'lv-add-re)
(define-key lv-mode-map (kbd "C-c R") 'lv-del-re)
(define-key lv-mode-map (kbd "C-c l") 'lv-add-limit)
(define-key lv-mode-map (kbd "C-c L") 'lv-del-limit)
(define-key lv-mode-map (kbd "C-c Q") 'lv-reset)

;;;###autoload
(define-minor-mode lv-mode
  "Filter and highlight lines according to regular expressions.

\\{lv-mode-map}"
  :init-value nil
  :keymap lv-mode-map
  :group 'lv
  :lighter " LV"
  (when (not lv-mode)
    (lv-reset)))

(provide 'lv)
;;; lv.el ends here
