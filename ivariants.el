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

;;; Code:

(require 'ivariants-table)
(require 'cl-lib)

(defvar ivariants-order
  '(proper traditional simplified duplicate variant non-cognate japanese tiwanese compat ivs borrowed)
  "Order to list in \\[ivariants].")

;; calculation

(defun ivariants-by-category (char regexp)
  "Get all CHAR variants of category REGEXP."
  (let ((alist (aref ivariants-table char))
        result)
    (dolist (item alist)
      (let ((prop (symbol-name (car item)))
            (chars (cdr item)))
        (if (string-match regexp prop)
            (if (listp chars)
                (setq result (append result chars))
              (setq result (append result (list chars)))))))
    (cl-remove-duplicates result :test 'equal)))

;; e.g. (ivariants-by-category ?一 "variant")

(defun ivariants-char (char)
  "Collect all uniqe variants of CHAR.
Lists are ordered according to `ivariants-order'."
  (let ((chars)
        (result))
    (dolist (category (append ivariants-order '(.)))
      (let* ((variants (ivariants-by-category char (symbol-name category))))
        (setq variants (cl-set-difference variants chars))
        (when variants
          (setq chars (cl-union variants chars))
          (message "chars=%s" chars)
          (setq result (append result (list variants))))))
    result))

(defun ivariants-short-string (char)
  "Short string expression of variants of CHAR."
  (let ((ivariants (ivariants-char char)))
    (when ivariants
      (concat "《"
              (mapconcat (lambda (x)
                           (apply 'concat
                                  (mapcar (lambda (y) (if (characterp y) (list y) y)) x)))
                         ivariants "/")
              "》"))))

;;;###autoload
(defun ivariants-insert ()
  "Insert variants short string form after point."
  (interactive)
  (insert (ivariants-short-string (char-after (point)))))

(provide 'ivariants)

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:

;;; ivariants.el ends here
