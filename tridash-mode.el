;; -*- lexical-binding: t -*-
;;; tridash.el --- Tridash language major mode

;; Copyright (C) 2019-2020  Alexander Gutev
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'regexp-opt)

(defvar tridash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)

  "Keymap for `tridash-mode'.")

(defvar tridash-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Symbol Constituent Characters
    (modify-syntax-entry ?\! "_" st)
    (modify-syntax-entry ?\@ "_" st)
    (modify-syntax-entry ?\$ "_" st)
    (modify-syntax-entry ?\% "_" st)
    (modify-syntax-entry ?^ "_" st)
    (modify-syntax-entry ?\& "_" st)
    (modify-syntax-entry ?\* "_" st)
    (modify-syntax-entry ?\- "_" st)
    (modify-syntax-entry ?\_ "_" st)
    (modify-syntax-entry ?\= "_" st)
    (modify-syntax-entry ?\+ "_" st)
    (modify-syntax-entry ?\: "_" st)
    (modify-syntax-entry ?\' "_" st)
    (modify-syntax-entry ?\\ "_" st)
    (modify-syntax-entry ?\| "_" st)
    (modify-syntax-entry ?\` "_" st)
    (modify-syntax-entry ?\~ "_" st)
    (modify-syntax-entry ?\< "_" st)
    (modify-syntax-entry ?\> "_" st)
    (modify-syntax-entry ?\? "_" st)
    (modify-syntax-entry ?\/ "_" st)

    ;; Punctuation Characters
    (modify-syntax-entry ?\. "." st)
    (modify-syntax-entry ?\, "." st)
    (modify-syntax-entry ?\; "." st)

    ;; Parenthesis Characters
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)

    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)

    ;; String Quotes and Escape Characters
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "\\" st)

    ;; Comment Delimiters
    (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)

  "Syntax table for `tridash-mode'.")

(defvar tridash-font-lock-keywords
  (list
   (cons
    (regexp-opt
     '("/operator" "/quote"
       "/context" "/state"
       "/external" "self"
       "/attribute" "/attribute-processor"
       "/module" "/import" "/use-as" "/use" "/export" "/in")
     t)
    'font-lock-builtin-face)))


;;; Indentation

(defvar tridash-indent-basic 4)


(defun tridash-indent-line ()
  "Indent current line of Tridash code."
  (interactive)

  (let ((savep (> (current-column) (current-indentation)))
	(indent (condition-case nil (max (tridash-calculate-indentation) 0)
		  (error 0))))

    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun tridash-calculate-indentation ()
  "Calculate the indentation level for the line at point."

  (save-excursion
    (beginning-of-line)

    (if (looking-at "[[:blank:]]*[})]")
	(progn
	  (- (tridash-calculate-indentation-after-point) tridash-indent-basic))

      (tridash-calculate-indentation-after-point))))

(defun tridash-calculate-indentation-after-point ()
  "Calculate the indentation of the line following point."

  ;; Move to previous token, skipping whitespace and comments
  (tridash-backward-comment)

  (if (looking-back "[({]" 1)
      ;; If closing parenthesis at end of line, indent by one level
      ;; otherwise return current column.

      (if (looking-at "[[:blank:]]*$")
	  (+ (current-indentation) tridash-indent-basic)
	(current-column))

    ;; If at closing parenthesis, return to beginning of infix
    ;; expression and indent according to its indentation level.

    (if (looking-back "[})]" 1)
	(progn
	  (tridash-expression-beginning)
	  (current-indentation))

      ;; If after infix operator, indent by 1 level. Otherwise, return
      ;; to beginning of infix expression and calculate indentation
      ;; from that point.

      (let ((num-nodes (tridash-count-nodes-till-point)))
	(if (and (> num-nodes 0)
		 (= (mod num-nodes 2) 0))

	    (+ (current-indentation) tridash-indent-basic)

	  (tridash-expression-beginning)
	  (tridash-calculate-indentation-after-point))))))

(defun tridash-expression-beginning ()
  "Moves backwards to the beginning of the infix expression at point."

  (condition-case nil
      (while
	  (and (not (bobp))
	       (or (not (bolp))
		   (save-excursion
		     (tridash-backward-comment)
		     (tridash-after-operator-p))))

	(backward-sexp))
    (error nil)))

(defun tridash-after-operator-p ()
  "Check whether point is after an operator."

  (let ((num-nodes (tridash-count-nodes-till-point)))
    (and (> num-nodes 0)
    	 (= (mod num-nodes 2) 0))))


;;;; Comments

(defun tridash-forward-comment ()
  "Moves point forward to the next character which is not
   whitespace and not part of a comment. Returns true if point
   was moved past the end of the current line, false otherwise."

  (let ((start-point (point)))
    (forward-comment (buffer-size))

    (let ((end-point (point)))
      (save-excursion
	(goto-char start-point)
	(re-search-forward "[\n|\r]" end-point t)))))

(defun tridash-backward-comment ()
  "Moves point backward to the first character which is not
   whitespace and not part of a comment. Returns true if move
   point was moved past the beginning of the current line, false
   otherwise."

  (let ((start-point (point)))
    (forward-comment (- (point)))

    (let ((end-point (point)))
      (save-excursion
	(goto-char start-point)
	(re-search-backward "[\n|\r]" end-point t)))))


;;;; Counting Nodes on Line

(defun tridash-count-nodes-till-point ()
  "Returns the number of nodes (tokens separated by whitespace)
   between the start of the current line and point. The number
   might not accurately reflect the actual number of node tokens
   on the line, that is the number of nodes between the last ';',
   ')', '}' or ';' and point token may be returned instead."

  (save-excursion
    (let ((nodes 0)
	  (end? nil))
      (while (not (or (bolp) end?))
	(skip-chars-backward " \t")
	(cond
	 ((not (zerop (skip-syntax-backward "w_")))
	  (setq nodes (1+ nodes)))

	 ;; Parenthesis and braces may only form part of operand nodes
	 ((not (zerop (skip-syntax-backward ")")))
	  (setq nodes (1+ nodes))
	  (setq end? t))

	 ;; Strings may only form part of operand nodes
	 ((not (zerop (skip-syntax-backward "\"")))
	  (setq nodes (1+ nodes))
	  (setq end? t))

	 (t ; ';', ')', '}' and ',' tokens
	  (setq end? t))))

      nodes)))



;;; Mode Definition

;;;###autoload
(define-derived-mode tridash-mode prog-mode "Tridash"
  "Major mode for editing Tridash source files"
  :syntax-table tridash-mode-syntax-table

  (setq-local multibyte-syntax-as-symbol t)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults '(tridash-font-lock-keywords))

  (setq-local indent-line-function 'tridash-indent-line))

;;; Module

(provide 'tridash-mode)
