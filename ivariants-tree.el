;;; ivariants-tree.el --- Ideographic Variants Tree viewer  -*- lexical-binding: t -*-

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Description: Ideographic Variants Tree viewer
;; Created: 2014-01-01
;; Package-Requires: ((emacs "24.3") (ivs-edit "1.0"))
;; Keywords: text
;; Namespace: ivariants-tree-
;; Human-Keywords: Ideographic Variations
;; Version: 1.140316
;; URL: http://github.com/kawabata/ivariants

(require 'ivariants-table)
(require 'ivs-edit)
(require 'cl-lib)

(defun ivariants-tree-expander (widget)
  "WIDGETの子となる木のリストを返す."
  (let* ((char      (widget-get widget :char))
         (ancestors (cons char (widget-get widget :ancestors))) ;; ancestors
         (node-aj1  (ivariants-tree-ivs
                     char 'Adobe-Japan1))
         (node-hd   (ivariants-tree-ivs
                     char 'Hanyo-Denshi))
         (alist     (get-char-code-property char 'ivariants))
         (ivariants)
         (nodes))
    (dolist (item alist)
      (setq ivariants (append (cdr item) ivariants)))
    (if node-aj1 (push node-aj1 nodes))
    (if node-hd  (push node-hd nodes))
    (dolist (item alist)
      (let ((attribute (car item))
            (variants (cdr item)))
        (dolist (variant variants)
          (when (not (memq variant ancestors))
            (push
             (list 'tree-widget
                   :tag (format "%s %s"
                                (if (characterp variant) (char-to-string variant)
                                  variant)
                                (or (gethash attribute ivariants-name-table)
                                    (symbol-name attribute)))
                   :char variant
                   :open nil :ancestors (cl-union ancestors ivariants)
                   :expander 'ivariants-tree-expander)
             nodes)))))
    (nreverse nodes)))

(defun ivariants-tree-ivs (char ivd)
  "Return the widget node of CHAR with IVD."
  (let* ((entries (gethash char ivs-edit-table))
         (entries (cl-remove-if-not (lambda (x) (equal (elt x 1) ivd)) entries))
         children)
    (when entries
      (dolist (entry entries)
        (push
         (list 'tree-widget
               :tag (format "%c%c (%s)" char (car entry) (elt entry 2))
               :open t :has-children nil)
         children))
      `(tree-widget :tag ,(format "%c【%s】" char ivd)
                    :open nil
                    ,@(nreverse children)))))

(defun ivariants-to-names (list)
  (mapconcat
   (lambda (x)
     (or (assoc-default x ivariants-name)
         (symbol-name x)))
   list "・"))

(defun ivariants-tree (&optional char)
  (interactive
   (let* ((char (char-after (point)))
          (mnemonics (and char (category-set-mnemonics (char-category-set char)))))
     (list (string-to-char
            (read-string "Char? : " (if (and mnemonics (string-match "C" mnemonics))
                                        (char-to-string char)))))))
  (switch-to-buffer "*異体字検索*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    (mapcar #'delete-overlay (car all)))
  (widget-insert "【異体字検索】\n\n")
  (widget-create 'tree-widget
                 :tag (format "%c (%04X)" char char)
                 :char char
                 :ancestors nil
                 :open nil ; t
                 :expander 'ivariants-tree-expander
                 :has-children t)
  (widget-insert "\n")
  ;; Insert the Close button
  (widget-create 'push-button
                 :keymap tree-widget-button-keymap ; Emacs
                 :notify 'ivariants-tree-close
                 "Close")
  (use-local-map widget-keymap)
  (widget-setup)
  (help-mode)
  (goto-char (point-min))
  (widget-forward 1))

(defun ivariants-tree-close (&rest ignore)
  "Close the current dialog.
IGNORE arguments."
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'ivariants-tree)
