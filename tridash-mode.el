;; -*- lexical-binding: t -*-
;;; tridash.el --- Tridash language major mode

;; Copyright (C) 2019  Alexander Gutev
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

(require 'smie)

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
     '(":module" ":use" ":alias" ":import" ":in" ":export"
       ":op" ":extern"
       ":attribute" ":quote"
       "self")
     t)
    'font-lock-builtin-face)))

(defvar tridash-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((atom)
      (node (node "operator" node)
	    ("(" node-list ")")
	    ("{" nodes "}")
	    (atom))
      (nodes (node ";")
	     (node "\n"))
      (node-list (node-list "," node-list)
		 (node)))
    '((assoc ","))
    '((assoc "operator")))))

;;;###autoload
(define-derived-mode tridash-mode prog-mode "Tridash"
  "Major mode for editing Tridash source files"
  :syntax-table tridash-mode-syntax-table

  (setq-local multibyte-syntax-as-symbol t)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults '(tridash-font-lock-keywords))

  (smie-setup tridash-smie-grammar 'tridash-smie-rules
	      :forward-token #'tridash-smie-forward-token
	      :backward-token #'tridash-smie-backward-token))


;;; Indentation

;; If NIL SMIE will use the value of `smie-indent-basic'
(defvar tridash-indent-basic 4)


(defun tridash-smie-rules (kind token)
  "SMIE Tridash indentation rules function."

  (pcase (cons kind token)
    (`(:elem . basic) tridash-indent-basic)
    (`(,_ . ",") (smie-rule-separator kind))
    (`(,:after . ";")
     0)

    (`(:after . "operator")
     (if (and (smie-rule-hanging-p)
    	      (not (smie-rule-parent-p "operator")))
    	 tridash-indent-basic))))

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

(defun tridash-smie-forward-token ()
  "SMIE Tridash forward token function. Returns \"node\" for node
   tokens, returns \"operator\" for infix operators."

  (let ((num-nodes (tridash-count-nodes-till-point)))

    (cond
     ((and (tridash-forward-comment)
	   (/= (mod num-nodes 2) 0))
      ";")

     ((looking-at ";")
      (forward-char)
      ";")

     ((looking-at ",")
      (forward-char)
      ",")

     (t
      (if (/= (skip-syntax-forward "w_") 0)
  	  (if (= (mod (tridash-count-nodes-till-point) 2) 0)
  	      "operator"
  	    "node")
	"")))))

(defun tridash-smie-backward-token ()
  "SMIE Tridash backward token function. Returns \"node\" for
   node tokens, returns \"operator\" for infix operators."

  (cond
   ((and (tridash-backward-comment)
	 (/= (mod (tridash-count-nodes-till-point) 2) 0))
    ";")

   ((looking-back ";" 1)
    (backward-char)
    ";")

   ((looking-back "," 1)
    (backward-char)
    ",")

   (t
    (if (/= (skip-syntax-backward "w_") 0)
  	(if (= (mod (tridash-count-nodes-till-point) 2) 0)
  	    "node"
  	  "operator")
      ""))))


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

	 ((not (zerop (skip-syntax-backward ")")))
	  (setq nodes (1+ nodes))
	  (setq end? t))

	 (t ; ';', ')', '}' and ',' tokens
	  (setq end? t))))

      nodes)))


;;; Module

(provide 'tridash-mode)
