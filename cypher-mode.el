;;; cypher-mode.el --- major mode for editing cypher scripts

;; Copyright 2013 François-Xavier Bois
;; Copyright 2019 Bjarte Johansen

;; Version: 1.0.0
;; Author: François-Xavier Bois <fxbois AT Google Mail Service>
;; Maintainer: Bjarte Johansen
;; Created: Sept 2013
;; Keywords: cypher graph
;; URL: http://github.com/ljos/cypher-mode
;; Repository: http://github.com/ljos/cypher-mode

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Code goes here

(require 'custom)
(require 'comint)

(defgroup cypher nil
  "Major mode for editing cypher scripts."
  :version "1.0.0"
  :group 'languages
  :prefix "cypher-")

(defgroup cypher-faces nil
  "Faces for syntax highlighting."
  :group 'cypher-mode
  :group 'faces)

(defcustom cypher-indent-offset 2
  "Indentation level."
  :type 'integer
  :group 'cypher-mode)

(defface cypher-keyword-face
  `((t :inherit font-lock-keyword-face))
  "Face for language keywords."
  :group 'cypher-faces)

(defface cypher-constant-face
  `((t :inherit font-lock-constant-face))
  "Face for language keywords."
  :group 'cypher-faces)

(defface cypher-builtin-face
  `((t :inherit font-lock-builtin-face))
  "Face for builtin function."
  :group 'cypher-faces)

(defface cypher-variable-face
  `((t :inherit font-lock-variable-name-face))
  "Face for vars."
  :group 'cypher-faces)

(defvar cypher-keywords
  '("all" "allshortestpaths" "and" "any" "as" "asc" "ascending" "assert"
    "by"
    "call" "case" "constraint on" "count" "create" "create constraint on" "create index on"
    "create unique" "create"
    "delete" "desc" "descending" "distinct" "drop constraint on" "drop index on" "drop"
    "else" "end" "extract"
    "fieldterminator" "filter" "foreach" "from"
    "has"
    "in" "is not" "is" "is unique" "is"
    "limit" "load csv"
    "match" "merge"
    "node" "none" "not" "on create" "on match" "on" "optional match" "or" "order by"
    "reduce" "rel" "relationship" "remove" "return distinct" "return"
    "scan" "set" "shortestpath" "single" "skip" "start" "then"
    "union" "union all" "unique" "unwind" "using" "using index" "using periodic commit" "using scan"
    "when" "where" "with" "with distinct" "with headers"
    "yield"
    "xor")
  "Cypher keywords.")

(defvar cypher-constants
  '("true" "false" "null"))

(defvar cypher-functions
  '("abs" "acos" "asin" "atan" "atan2" "avg"
    "ceil" "coalesce" "collect" "cos" "cot" "count"
    "degrees"
    "e" "endnode" "exists" "exp" "floor"
    "has" "haversin" "head"
    "id"
    "labels" "last" "left" "length" "log" "log10" "lower" "ltrim"
    "max" "min"
    "nodes"
    "percentilecont" "percentiledisc" "pi"
    "radians" "rand" "range" "reduce" "relationships" "rels" "replace" "right" "round" "rtrim"
    "sign" "sin" "size" "split" "sqrt" "startnode" "stdev" "stdevp" "str" "substring" "sum"
    "tail" "tan" "timestamp" "tofloat" "toint" "tolower" "tostring" "toupper" "trim" "type"
    "upper")
  "Cypher functions")


(defun cypher-indent-line ()
  "Indent current line as a cypher query."
  (interactive)
  (let (indent-column
        (keywords (concat "\\b" (regexp-opt cypher-keywords) "\\b")))
    (save-mark-and-excursion
      (beginning-of-line)
      (skip-syntax-forward "-" (line-end-position))
      (unless (looking-at keywords)
        (forward-line -1)
        (beginning-of-line)
        (if (not (looking-at keywords))
            (setq indent-column (current-indentation))
          (goto-char (match-end 0))
          (setq indent-column (1+ (current-column))))))
    (save-mark-and-excursion
      (indent-line-to (or indent-column 0))))
  (when (let ((search-spaces-regexp t))
          (string-match-p "^ *$"
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (point))))
    (skip-chars-forward "[:space:]" (line-end-position))))

(defvar cypher-font-lock-keywords
  `(("[$]\\w+" 0 'cypher-variable-face)
    (,(concat "\\b\\(" (regexp-opt cypher-functions) "\\)\\b(" ) 1 'cypher-builtin-face)
    (,(concat "\\b" (regexp-opt cypher-constants) "\\b" ) . 'cypher-constant-face)
    (,(concat "\\b" (regexp-opt cypher-keywords) "\\b") . 'cypher-keyword-face)))

(defvar cypher-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; _   : word
    (modify-syntax-entry ?_ "w" table)
    ;; //  : comment
    (modify-syntax-entry ?\/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; ' " : strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\` "\"" table)
    table)
  "Syntax table for `cypher-mode'.")

(defvar cypher-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-p" 'run-cypher-shell)
    (define-key map "\C-c\C-c" 'cypher-shell-send-region)
    map)
  "Keymap for `cypher-mode'.")

;;;###autoload
(define-derived-mode cypher-mode prog-mode "Cypher"
  "Major mode for editing web templates.
\\{cypher-mode-map}
"
  (setq-local indent-line-function #'cypher-indent-line)
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "/+\s-*")
  (setq-local font-lock-defaults
              '(cypher-font-lock-keywords
                nil ;; font-lock-keywords-only
                t   ;; font-lock-keywords-case-fold-search
                )))

(defgroup cypher-shell nil
  "Inferior mode for interactive cypher shell."
  :version "0.0.1"
  :group 'cypher
  :prefix "cypher-shell-")

(defvar cypher-shell-program "cypher-shell")

(defvar cypher-shell-user "neo4j")

(defvar cypher-shell-password nil)

(defvar cypher-shell-args '())

(defvar cypher-shell-prompt-regexp "^\\w*>")

(defvar cypher-shell-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `run-cypher-shell'")

(defun cypher-shell-set-user (user)
  "Set the user to a new value."
  (interactive
   (list (read-string "Username: ")))
  (setq cypher-shell-user password))

(defun cypher-shell--get-user ()
  (or cypher-shell-user
      (command-execute #'cypher-shell-set-user)))

(defun cypher-shell-set-password (password)
  "Set the password to a new value."
  (interactive
   (list (read-passwd "Password: ")))
  (setq cypher-shell-password password))


(defun cypher-shell--get-password ()
  (or cypher-shell-password
      (command-execute #'cypher-shell-set-password)))

(defun run-cypher-shell ()
  "Run an inferior instance of `cyper-shell' inside Emacs."
  (interactive)
  (let ((cypher-shell-program cypher-shell-program)
        (buffer (get-buffer-create "*Cypher*")))
    (when (not (comint-check-proc "*Cypher*"))
      (apply 'make-comint-in-buffer "Cypher" buffer
             cypher-shell-program nil
             (append (list "-u" (cypher-shell--get-user))
                     (list "-p" (cypher-shell--get-password))
                     cypher-shell-args)))
    (pop-to-buffer-same-window "*Cypher*")
    (inferior-cypher-mode)))

(defun cypher-shell-send-region ()
  "Send the active region (or whole buffer) to the running cypher
shell."
  (interactive)
  (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         (region (buffer-substring-no-properties beg end))
         (process (get-buffer-process (get-buffer "*Cypher*"))))
    (if (not process)
        (error
         "Start cypher process first with `M-x run-cypher-shell' or `%s'."
         (key-description
          (where-is-internal #'run-cypher-shell overriding-local-map t)))
      (comint-send-string process
                          (if (string-match ";\n?\s-*\n?\\'" region)
                              region
                            (concat region ";\n")))
      (comint-send-string process "\n"))))

(defun cypher-shell--initialize ()
  (setq comint-use-prompt-regexp t))

(defvar inferior-cypher-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `inferior-cypher-mode'.")

(defvar inferior-cypher-mode-syntax-table
  (let ((table (copy-syntax-table cypher-mode-syntax-table)))
    table)
  "Syntax table for `inferior-cypher-mode'")

(define-derived-mode inferior-cypher-mode comint-mode "Cypher-Shell"
  "Major mode for `run-cypher-shell'
\\{inferior-cypher-mode-map}"
  :group 'cypher-shell
  (setq-local comint-prompt-regexp cypher-shell-prompt-regexp)
  (setq-local comint-prompt-read-only t)
  (setq-local paragraph-start cypher-shell-prompt-regexp)
  (setq-local font-lock-defaults
              '(cypher-font-lock-keywords
                nil ;; font-lock-keywords-only
                t   ;; font-lock-keywords-case-fold-search
                )))

(add-hook 'inferior-cypher-mode #'cypher-shell--initialize)

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.cypher\\'" . cypher-mode))
  (add-to-list 'auto-mode-alist '("\\.cyp\\'" . cypher-mode)))

(provide 'cypher-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; cypher-mode.el ends here
