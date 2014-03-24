;;; ivariants-table.el --- Math Symbol Input methods and conversion tools -*- lexical-binding: t -*-

;; Filename: ivariants-table.el
;; Description: Math Symbol Input methods and conversion tools
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2013-03-25
;; Package-Requires: ((emacs "24.3"))
;; Version: 1.131017
;; Keywords: i18n languages
;; Human-Keywords: math symbols

;; variants.el (Emacs 23 only)
;;
;; Note: functions defined only at byte-compile-time may not have
;; proper namespace.

(declare-function ivariants-add-charstr "ivariants-table" (charstr prop valstr))
(declare-function ivariants-parse-files "ivariants-table" ())

(eval-when-compile
  (defvar ivariants-files nil)
  (setq ivariants-files
    '(;; chinese
      "cjkvi-simplified.txt"
      "hydzd-borrowed.txt"
      "hydzd-variants.txt"
      ;; japanese
      "hyogai-variants.txt"
      "jinmei-variants.txt"
      "jisx0212-variants.txt"
      "jisx0213-variants.txt"
      "joyo-variants.txt"
      "jp-borrowed.txt"
      "koseki-variants.txt"
      "x0212-x0213-variants.txt"
      ;; misc
      "cjkvi-variants.txt"
      "duplicate-chars.txt"
      "non-cjk.txt"
      "non-cognates.txt"
      "numeric-variants.txt"
      "radical-variants.txt"
      "ucs-scs.txt"
      ))

  (defvar ivariants-char-table nil)
  (setq ivariants-char-table (make-char-table 'char-code-property-table))
  (defvar ivariants-attr-name-table nil)
  (setq ivariants-attr-name-table (make-hash-table :test 'equal))

  ;; utilities
  (defun ivariants-add-charstr (charstr prop valstr)
    (when (= 1 (length charstr))
      (let* ((char (string-to-char charstr))
             (val (if (= 1 (length valstr)) (string-to-char valstr) valstr))
             (alist (aref ivariants-char-table char))
             (item (assq prop alist)))
        (if item
            (unless (memq val item) (nconc item (list val)))
          (setq alist (cons (list prop val) alist))
          (aset ivariants-char-table char alist)))))

  (defun ivariants-parse-files ()
    "Parse variant data files."
    (interactive)
    (let* ((directory
            (expand-file-name
             "tables"
             (file-name-directory (or byte-compile-current-file
                                      load-file-name
                                      buffer-file-name))))
           (reverse-table (make-hash-table :test 'equal :size 50000)))
      (dolist (i ivariants-files)
        (with-temp-buffer
          (message "Now loading [%s]..." i)
          (insert-file-contents (expand-file-name i directory))
          (goto-char (point-min))
          (while (re-search-forward "^\\(.+?\\),\\(.+?\\),\\([^,\n]+\\)" nil t)
            (let ((a (match-string 1))
                  (b (intern (match-string 2)))
                  (c (match-string 3)))
              (if (equal b '<rev>) (puthash (intern a) (intern c) reverse-table)
                (if (equal b '<name>) (puthash (intern a) c ivariants-attr-name-table)
                  (let ((rev (gethash b reverse-table)))
                    (ivariants-add-charstr a b c)
                    (if rev
                        (ivariants-add-charstr c rev a)))))))))))
  ;; execute
  (ivariants-parse-files))

(defvar ivariants-name-table
  (eval-when-compile ivariants-attr-name-table))

(defvar ivariants-table
  (eval-when-compile ivariants-char-table))

(set-char-table-extra-slot ivariants-table 0 'ivariants)
(define-char-code-property 'ivariants ivariants-table)

(provide 'ivariants-table)

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+2.%02y%02m%02d\\\\?\n"
;; End:
