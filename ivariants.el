;;; ivariants.el --- Ideographic Variants editor  -*- lexical-binding: t -*-

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Description: Tools for Editing Ideographic Variants
;; Created: 2014-01-01
;; Package-Requires: ((emacs "24.3"))
;; Keywords: text
;; Namespace: ivariants-
;; Human-Keywords: Ideographic Variations
;; Version: 1.140324
;; URL: http://github.com/kawabata/ivariants

;;; Commentary:

;; * ivariants.el … Ideographic Variants Integrated Editor
;;
;; ivariants.el provides various Ideographic Variants editing tools.
;; It provides various variation tables.
;;
;; ** Inserting Variants
;; `M-x ivariants-insert' inserts the variants of the cursor.
;;
;; ** ivariants-tree
;;
;; `M-x ivariants-tree' provides the tracing variants by tree widget.

;;; Code:

(require 'ivariants-table)
(require 'cl-lib)

(defvar ivariants-order
  '(proper traditional "," simplified ":" variant-simplified
           pseudo-simplified "|" variant)
  "Order to list in \\[ivariants].")

;; calculation

(defun ivariants-by-category (char category-str)
  "Get all CHAR variants of CATEGORY-STR."
  (let ((alist (aref ivariants-table char))
        result)
    (dolist (item alist)
      (let ((prop (symbol-name (car item)))
            (regexp (concat "/" category-str))
            (chars (cdr item)))
        (if (string-match regexp prop)
            (if (listp chars)
                (setq result (append result chars))
              (setq result (append result (list chars)))))))
    (cl-remove-duplicates result :test 'equal)))

(defun ivariants-by-category-string (char category)
  "Categorize CHAR variants by symbol CATEGORY.
Returned value is a list of string."
  (mapcar
   (lambda (char)
     (if (characterp char) (char-to-string char) char))
   (ivariants-by-category char (symbol-name category))))

;; e.g. (ivariants-by-category ?一 "variant")

(defun ivariants-char-string (char)
  "Collect all uniqe variants of CHAR.
Lists are ordered according to `ivariants-order'."
  (let ((variants-all)
        (variants-group))
    (mapconcat
     (lambda (category)
       (if (stringp category)
           (when variants-group
             (setq variants-group nil)
             category)
         (let* ((variants (ivariants-by-category-string char category)))
           (setq variants
                 (cl-set-difference variants variants-all :test 'equal))
           (when variants
             (setq variants-all (cl-union variants variants-all :test 'equal))
             (setq variants-group (cl-union variants variants-group :test 'equal)))
           (apply 'concat variants))))
     ivariants-order "")))

;;;###autoload
(defun ivariants-insert ()
  "Insert variants short string form after point."
  (interactive)
  (let ((string (ivariants-char-string (char-after (point)))))
    (if string (insert "《" string "》")
      (message "No varinats found!"))))

(provide 'ivariants)

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:

;;; ivariants.el ends here
