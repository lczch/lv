;;; lv-gem5-trace.el --- Log filter and highlighter for GEM5 traces

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
;; This extension specializes `lv-mode' to work with GEM5 traces.
;;
;; To activate it:
;;   (require 'lv-gem5-trace)
;;   (lv-mode t)
;;
;; To use it, see the help of `lv-mode'.

;;; Code:

(require 'lv)


(defvar lv--gem5-sn nil
  "List of GEM5 instruction sequence numbers to select.")
(make-variable-buffer-local 'lv--gem5-sn)

(defvar lv--gem5-sn-history nil
  "List of arguments to `lv-add-sn' so far.")

(defvar lv--gem5-sn-history-str nil
  "List of arguments to `lv-add-sn' so far (stringified version).")


(defun lv--gem5-sn-to-re (sn)
  "Return an appropriate regexp for the given sequence number."
  (format "\\[sn:%d\\]" sn))


;;;###autoload
(defun lv-add-gem5-sn (&optional sn)
  "Add a GEM5 instruction sequence number."
  (interactive)
  (when (called-interactively-p 'interactive)
    (setq sn (string-to-number
              (completing-read "Sequence number: " nil nil nil nil
                               'lv--gem5-sn-history-str))))
  (add-to-list 'lv--gem5-sn-history sn t)
  (setq lv--gem5-sn-history-str (mapcar 'int-to-string lv--gem5-sn-history))
  (add-to-list 'lv--gem5-sn sn)
  (lv-add (lv--gem5-sn-to-re sn)))

;;;###autoload
(defun lv-del-gem5-sn (&optional sn)
  "Remove a GEM5 instruction sequence number."
  (interactive)
  (when (called-interactively-p 'interactive)
    (setq sn (string-to-number
              (completing-read "Sequence number: "
                               (mapcar 'int-to-string lv--gem5-sn)))))
  (setq lv--gem5-sn (remove sn lv--gem5-sn))
  (lv-del (lv--gem5-sn-to-re sn)))


(define-key lv-mode-map (kbd "C-c s") 'lv-add-gem5-sn)
(define-key lv-mode-map (kbd "C-c S") 'lv-del-gem5-sn)


(provide 'lv-gem5-trace)
;;; lv-gem5-trace.el ends here
