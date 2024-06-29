;;; js-log.el --- Insert js logs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/js-log
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Insert js logs

;;; Code:

(defvar shr-color-html-colors-alist)

(require 'treesit)

(defvar js-log-node-types '(">>" "<<" "~" "&=" "|=" ">>>=" "do"
                            "do_statement" "-=" "continue"
                            "continue_statement"
                            "statement_identifier" "labeled_statement"
                            "*=" "--" ">>>" "enum_assignment"
                            "enum_body" "enum" "enum_declaration"
                            "internal_module" "hash_bang_line"
                            "string_content" "document" "%"
                            "jsx_fragment" "==" "nested_identifier"
                            "flow_maybe_type" "empty_statement"
                            "super" "throw" "throw_statement"
                            "sequence_expression" "type_assertion"
                            "this_type" "extends_clause"
                            "class_heritage"
                            "generator_function_declaration" "await"
                            "await_expression" "void" "delete" "async"
                            "non_null_expression" "ERROR"
                            "object_assignment_pattern" "set" "get"
                            "public_field_definition" "static" "this"
                            "class_body" "class" "class_declaration"
                            "method_definition" "jsx_closing_element"
                            "jsx_self_closing_element" "jsx_text"
                            "jsx_expression" "jsx_attribute"
                            "jsx_opening_element" "jsx_element"
                            "template_type" "template_literal_type"
                            "symbol" "method_signature" "namespace"
                            "nested_type_identifier"
                            "function_signature" "optional_parameter"
                            "extends_type_clause" "module" "declare"
                            "ambient_declaration" "\"" "default"
                            "switch_default" "case" "switch_case"
                            "switch_body" "switch" "switch_statement"
                            "export_specifier" "export_clause"
                            "pair_pattern" "array_pattern" "!==" "?."
                            "optional_chain" "is" "type_predicate"
                            "type_predicate_annotation" "-"
                            "parenthesized_type" "comment"
                            "shorthand_property_identifier_pattern"
                            "object_pattern" "finally"
                            "finally_clause" "yield"
                            "yield_expression" "generator_function"
                            "shorthand_property_identifier" "false"
                            "boolean" "literal_type" "call_signature"
                            "interface" "interface_declaration"
                            "undefined" "typeof" "type_query"
                            "property_signature" "&"
                            "intersection_type" "lookup_type" "keyof"
                            "index_type_query" "infer" "infer_type"
                            "computed_property_name" "spread_element"
                            "as_expression" "in" "mapped_type_clause"
                            "index_signature" "object_type"
                            "type_arguments" "generic_type" "..."
                            "rest_pattern" "type_annotation"
                            "function_type" "tuple_type"
                            "conditional_type" "array_type" "readonly"
                            "readonly_type" "|" "never" "union_type"
                            "extends" "constraint" "unknown" "any"
                            "predefined_type" "default_type"
                            "type_parameter" "type_parameters"
                            "type_identifier" "type"
                            "type_alias_declaration" "catch"
                            "catch_clause" "try" "try_statement" "<"
                            "for_statement" "import_specifier"
                            "named_imports" "new" "new_expression"
                            "regex_flags" "regex_pattern" "/" "regex"
                            "true" "export" "export_statement" "from"
                            "as" "*" "namespace_import"
                            "import_clause" "import"
                            "import_statement" "+" "!=" "++"
                            "update_expression"
                            "assignment_expression" "var"
                            "variable_declaration" "else"
                            "else_clause" "break" "break_statement"
                            "===" "while" "while_statement"
                            "escape_sequence" "instanceof" "null" ">"
                            "?" "ternary_expression" "of" "for"
                            "for_in_statement" "array" "let" "!"
                            "unary_expression" "||" "<=" "&&" "number"
                            ">=" "binary_expression" "function"
                            "function_declaration" "return"
                            "return_statement" "+="
                            "augmented_assignment_expression" "]" "["
                            "subscript_expression" "if" "if_statement"
                            ";" "string_fragment" "'" "string"
                            "arguments" "." "member_expression" ","
                            "}" "${" "template_substitution" "`"
                            "template_string" "required_parameter" ":"
                            "property_identifier" "pair" "object" "="
                            "identifier" "variable_declarator" "const"
                            "lexical_declaration" "{"
                            "statement_block" "=>" ")"
                            "formal_parameters" "arrow_function" "("
                            "parenthesized_expression"
                            "call_expression" "expression_statement"
                            "program")
  "List of JavaScript syntax node types for logging within a tree-sitter parser.")

(defun js-log-check-node-type (type node)
  "Check if NODE's TYPE matches type or list TYPE.

Argument TYPE is either a string specifying a single node type or a list of
strings specifying multiple NODE types.

Argument NODE is the Treesit node whose TYPE is being checked."
  (if (listp type)
      (member (treesit-node-type node)
              type)
    (equal (treesit-node-type node) type)))

(defvar-local js-log-current-node nil)

(defun js-log-get-current-node ()
  "Retrieve the last node from an ascending list."
  (js-log-last-item
   (js-log-get-node-list-ascending)))

(defvar js-log-nodes-alist nil
  "Alist mapping JavaScript AST node types to logging functions.")

(defun js-log-get-metadata ()
  "Return completion metadata for the current minibuffer contents."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun js-log-minibuffer-auto-default-candidates ()
  "Display completion candidates for minibuffer input."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (js-log-get-metadata) 'category)
       all))))

(defun js-log-ivy-selected-cand ()
  "Log selected candidate's metadata and path."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors (js-log-get-metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun js-log-default-top-minibuffer-completion ()
  "Display top completion or current content in minibuffer."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (js-log-minibuffer-auto-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defvar js-log-minibuffer-candidate-finders
  '(js-log-ivy-selected-cand
    js-log-default-top-minibuffer-completion)
  "List of functions to find current minibuffer completion candidates.")

(defun js-log-get-current-candidate ()
  "Return selected candidate information as a cons cell."
  (let (target)
    (run-hook-wrapped
     'js-log-minibuffer-candidate-finders
     (lambda (fun)
       (when-let ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    target))

(defun js-log-node-cons (node)
  "Return cons of NODE text without properties and node.

Argument NODE is the tree-sitter node to extract text from and create a cons
cell."
  (when node
    (cons (substring-no-properties (treesit-node-text node))
          node)))

(defun js-log-last-item (items)
  "Return the last element of the list ITEMS.

Argument ITEMS is a list from which the last item will be retrieved."
  (car (last items)))

;;;###autoload
(defun js-log-node-at-point ()
  "Log node details at cursor position."
  (interactive)
  (let* ((node (js-log-last-item (js-log-get-node-list-ascending)))
         (type (treesit-node-type node))
         (field (treesit-node-field-name node))
         (txt (substring-no-properties (treesit-node-text node)))
         (text (truncate-string-to-width
                (substring-no-properties
                 txt
                 0
                 (string-match-p "\n" txt))
                80)))
    (message (string-join
              (remove nil
                      (append
                       (list
                        (when type
                          (propertize type 'face
                                      'font-lock-keyword-face))
                        (when field
                          (propertize field 'face
                                      'font-lock-builtin-face)))
                       (list text)))
              " - "))))

(defun js-log-annotate-parent-node (node)
  "Annotate parent NODE with capitalized, space-separated type.

Argument NODE is the node whose parent will be annotated."
  (when-let* ((parent (treesit-node-parent node))
              (str (treesit-node-type parent)))
    (capitalize (replace-regexp-in-string "_" "\s" str))))

(defun js-log-get-object-pattern-ids (node)
  "Return filtered NODE IDs matching specific patterns.

Argument NODE is a Treesit node to extract pattern IDs from."
  (seq-filter
   (apply-partially
    #'js-log-check-node-type
    '("identifier"
      "shorthand_property_identifier_pattern"))
   (remove nil
           (flatten-list (treesit-induce-sparse-tree
                          node
                          (lambda (item) item))))))

(defun js-log-current--node-child-p (node parent)
  "Return sparse tree if NODE is a child of PARENT.

Argument NODE is the node to check if it is a child.

Argument PARENT is the node to check against for being a parent of NODE."
  (remove nil
          (flatten-list
           (treesit-induce-sparse-tree
            parent
            (lambda (it)
              (treesit-node-eq
               it
               node))))))

(defun js-log-current-node-child-p (node)
  "Check if NODE is a child of the current logging node.

Argument NODE is the node to check if it is a child of the current node."
  (or (not js-log-current-node)
      (js-log-current--node-child-p node js-log-current-node)))

(defun js-log-get-node-id (node)
  "Extract NODE IDs from a JavaScript syntax tree.

Argument NODE is a Treesit node to be processed by the function."
  (pcase (treesit-node-type node)
    ("import_statement"
     (let ((clauses (flatten-list (cdr (treesit-induce-sparse-tree
                                        node
                                        "identifier\\|as")))))
       (seq-reduce (lambda (acc c)
                     (pcase (treesit-node-type c)
                       ("as" (setq acc (nbutlast acc)))
                       (_ (setq acc (nconc acc (list c))))))
                   clauses
                   '())))
    ("variable_declaration"
     (seq-filter (apply-partially
                  #'js-log-check-node-type
                  '("identifier"))
                 (remove nil
                         (flatten-list (mapcar
                                        (lambda (n)
                                          (treesit-search-subtree
                                           n
                                           (apply-partially
                                            #'js-log-check-node-type
                                            '("identifier"))
                                           nil t 2))
                                        (treesit-node-children node))))))
    ("lexical_declaration"
     (if-let ((obj (treesit-search-subtree node
                                           "object_pattern"
                                           nil
                                           nil
                                           2)))
         (js-log-get-object-pattern-ids obj)
       (treesit-search-subtree node
                               (apply-partially
                                #'js-log-check-node-type
                                '("identifier"))
                               nil t 2)))
    ("object"
     (if-let ((obj (treesit-search-subtree node
                                           "object_pattern"
                                           nil
                                           nil
                                           2)))
         (js-log-get-object-pattern-ids obj)
       (treesit-search-subtree node
                               (apply-partially
                                #'js-log-check-node-type
                                '("identifier"))
                               nil t 2)))
    ("catch_clause"
     (treesit-search-subtree
      node
      (apply-partially
       #'js-log-check-node-type
       '("identifier"))
      nil t 2))
    ("formal_parameters"
     (when (ignore-errors (js-log-current-node-child-p
                           (treesit-node-parent node)))
       (let* ((args
               (remove nil
                       (flatten-list (treesit-induce-sparse-tree
                                      node
                                      (lambda (item) item))))))
         (seq-filter
          (apply-partially
           #'js-log-check-node-type
           '("identifier"
             "shorthand_property_identifier_pattern"))
          args))))
    ((or "arrow_function"
         "function")
     (when (js-log-current-node-child-p node)
       (when-let ((children (treesit-node-children
                             node)))
         (cond ((js-log-check-node-type
                 '("identifier"
                   "shorthand_property_identifier_pattern")
                 (car children))
                (list (car children)))
               ((js-log-check-node-type '("formal_parameters")
                                        (car children))
                (let* ((args
                        (remove nil
                                (flatten-list (treesit-induce-sparse-tree
                                               (seq-find
                                                (apply-partially
                                                 #'js-log-check-node-type
                                                 '("formal_parameters"))
                                                children)
                                               (lambda (item) item)))))
                       (result (seq-filter
                                (apply-partially
                                 #'js-log-check-node-type
                                 '("identifier"
                                   "shorthand_property_identifier_pattern"))
                                args)))
                  result))))))
    ((or "function_declaration"
         "statement_block"
         "expression_statement" "if_statement"
         "formal_parameters"
         "parenthesized_expression"
         "declarator"
         "enum_declaration"
         "generator_function_declaration"
         "class_declaration"
         "declare"
         "ambient_declaration"
         "interface_declaration"
         "type_alias_declaration"
         "export_statement"
         "variable_declaration")
     (let ((found (treesit-search-subtree
                   node
                   (apply-partially
                    #'js-log-check-node-type
                    '("{"
                      "("
                      "identifier"
                      "property_identifier"))
                   nil t 2)))
       (unless (member (treesit-node-type found)
                       '("{"
                         "("))
         found)))))

(defun js-log-get-parents ()
  "Return list of parent nodes for a given node."
  (let* ((parent-node (js-log-get-top-parent-node))
         (node (save-excursion
                 (js-log-backward-whitespace-ignore-comments)
                 (treesit-node-at (point) nil t)))
         (path (list parent-node)))
    (while (setq parent-node
                 (seq-find (apply-partially
                            #'js-log-current--node-child-p node)
                           (treesit-node-children
                            parent-node)))
      (push parent-node path))
    (reverse path)))

(defun js-log--node-list-ascending ()
  "Return ascending list of nodes from current point."
  (cl-loop for node = (treesit-node-at (point))
           then (treesit-node-parent node) while node
           if (eq (treesit-node-start node)
                  (point))
           collect node))

(defun js-log-get-node-list-ascending ()
  "Return ascending list of nodes from point."
  (save-excursion
    (skip-chars-backward "\s\t\n\r\f")
    (while (and (equal "comment" (treesit-node-type (treesit-node-at (point))))
                (not (bobp)))
      (goto-char (treesit-node-start (treesit-node-at (point))))
      (skip-chars-backward "\s\t\n\r\f"))
    (let* ((node-list
            (js-log--node-list-ascending))
           (largest-node (js-log-last-item node-list))
           (parent (treesit-node-parent largest-node)))
      (if (null largest-node)
          (list (treesit-node-at (point))
                (treesit-node-parent
                 (treesit-node-at (point))))
        (if parent
            (append node-list (list parent))
          node-list)))))

(defun js-log--get-node-or-children (node)
  "Return NODE or its children as a cons cell.

Argument NODE is a Treesitter node object."
  (let* ((children (treesit-node-children node t)))
    (if (not children)
        node
      (cons node (mapcar #'js-log--get-node-or-children children)))))

(defun js-log-get-top-parent-node ()
  "Return top-level parent node of current point."
  (let ((node (treesit-node-at (point)))
        (top-level-children (treesit-node-children
                             (treesit-buffer-root-node))))
    (seq-find  (lambda (top-child)
                 (delq nil
                       (flatten-list
                        (treesit-induce-sparse-tree
                         top-child
                         (lambda (it)
                           (treesit-node-eq
                            it
                            node))))))
               top-level-children)))

;;;###autoload
(defun js-log-export-it ()
  "Insert \"export \" before first child of top parent node."
  (interactive)
  (let* ((parent-node (js-log-get-top-parent-node))
         (child (treesit-node-child parent-node 0)))
    (unless (member (treesit-node-type child) '("export" "import"))
      (save-excursion
        (goto-char (treesit-node-start child))
        (insert "export ")))))

(defun js-log-mark-node (node)
  "Mark NODE start and end, display its type and field name.

Argument NODE is the tree-sitter node to be marked."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node))
        (annotation (string-join
                     (delq nil (list (treesit-node-type node)
                                     (treesit-node-field-name node)))
                     " ")))
    (push-mark-command t)
    (goto-char start)
    (push-mark start nil t)
    (push-mark end)
    (message annotation)))

;;;###autoload
(defun js-log-mark-top-parent ()
  "Mark the top-level parent node of the current point in a JavaScript file."
  (interactive)
  (when-let ((parent-node (js-log-get-top-parent-node)))
    (js-log-mark-node parent-node)))

;;;###autoload
(defun js-log-mark-narrow-to-top ()
  "Narrow buffer to the top-level parent node of the current point."
  (interactive)
  (when-let* ((parent-node (js-log-get-top-parent-node))
              (start (treesit-node-start parent-node))
              (end (treesit-node-end parent-node)))
    (narrow-to-region start end)))

;;;###autoload
(defun js-log-narrow-dwim ()
  "Narrow buffer to selected region or top-level JavaScript node."
  (interactive)
  (pcase-let ((`(,start . ,end)
               (if (and (region-active-p)
                        (use-region-p))
                   (cons (region-beginning)
                         (region-end))
                 (let ((parent-node (js-log-get-top-parent-node)))
                   (cons (treesit-node-start parent-node)
                         (treesit-node-end parent-node))))))
    (narrow-to-region start end)))

(defvar-keymap js-log-expand-repeat-map
  :doc
  "Keymap to repeat `next-buffer' and `previous-buffer'.  Used in `repeat-mode'."
  :repeat t
  "u" #'js-log-expand-parents-up
  "m" #'js-log-expand-parents-up
  "d" #'js-log-expand-parents-down)

(defvar-local js-log-current-parents nil)

;;;###autoload
(defun js-log-expand-parents-up ()
  "Highlight parent nodes in log buffer."
  (interactive)
  (js-log-mark-parent-or-child 1))

;;;###autoload
(defun js-log-expand-parents-down ()
  "Highlight parent nodes in log display."
  (interactive)
  (js-log-mark-parent-or-child -1))

(defun js-log-mark-parent-or-child (&optional direction)
  "Highlight parent or child node in log.

Optional argument DIRECTION is an integer that determines the direction to
navigate through parent or child nodes. It defaults to 1."
  (unless direction (setq direction 1))
  (when-let ((node (treesit-node-at (point))))
    (unless (and js-log-current-parents
                 (memq last-command '(js-log-expand-parents-up
                                      js-log-expand-parents-down)))
      (setq js-log-current-parents (reverse (js-log-get-parents)))
      (when node
        (push node js-log-current-parents)))
    (when-let* ((node-start (and node
                                 (treesit-node-start node)))
                (pred (if (> direction 0)
                          (apply-partially #'> node-start)
                        (apply-partially #'< node-start)))
                (items (seq-sort-by #'treesit-node-start pred
                                    (seq-filter
                                     (lambda (parent)
                                       (let* ((parent-start (treesit-node-start
                                                             parent))
                                              (res (funcall pred
                                                            parent-start)))
                                         res))
                                     js-log-current-parents)))
                (next (car items)))
      (js-log-mark-node next))))

(defun js-log-which-func ()
  "Display JavaScript function or variable name at point."
  (let ((parents (js-log-get-parents))
        (result))
    (dolist (node parents)
      (when-let* ((child (treesit-node-child node 0 t))
                  (text (treesit-node-text child))
                  (type (treesit-node-type node))
                  (name
                   (pcase type
                     ("pair"
                      (pcase (treesit-node-type child)
                        ("property_identifier" text)
                        ("computed_property_name" text)
                        ("string" (concat "[" text "]"))))
                     ("variable_declarator"
                      (pcase (treesit-node-type child)
                        ("identifier" text)))
                     ("method_definition" text)
                     ("interface_declaration"
                      (pcase (treesit-node-type child)
                        ("type_identifier" text)))
                     ("property_signature"
                      (pcase (treesit-node-type child)
                        ("property_identifier" text)))
                     (_
                      nil))))
        (push name result)))
    (and result
         (replace-regexp-in-string "\\.\\[" "[" (string-join (reverse result)
                                                             ".")))))

;;;###autoload
(define-minor-mode js-log-which-func-mode
  "Display current JavaScript function or variable name path on save.

Automatically log the JavaScript function or variable name path when saving
files, and ensure `which-function-mode' is active for this feature to work."
  :lighter " js-log-which-func"
  :global nil
  (if js-log-which-func-mode
      (add-hook 'which-func-functions #'js-log-which-func nil t)
    (remove-hook 'which-func-functions #'js-log-which-func t))
  (unless which-function-mode
    (which-function-mode 1)))

(defun js-log-backward-whitespace-ignore-comments ()
  "Skip whitespace and comments, move point backward."
  (skip-chars-backward "\s\t\r\f\n")
  (let ((node (treesit-node-at (point) nil t))
        (prev-node))
    (while
        (and node
             (or (not prev-node)
                 (not (treesit-node-eq prev-node node)))
             (equal (treesit-node-type node) "comment"))
      (goto-char (treesit-node-start node))
      (skip-chars-backward "\s\t\r\f\n")
      (setq prev-node node)
      (setq node (treesit-node-at (point) nil t)))))

(defun js-log-position-within-node (position node)
  "Check if POSITION is within NODE's start and end.

Argument POSITION is the character position within the buffer.

Argument NODE is the Treesitter node to check the POSITION against."
  (< (treesit-node-start node)
     position
     (treesit-node-end node)))

(defun js-log-parse-node-ids (node)
  "Extract NODE IDs from a JavaScript AST node.

Argument NODE is a Treesitter node from which to extract identifiers."
  (let ((ids))
    (pcase (treesit-node-type node)
      ((or "arrow_function"
           "function_declaration"
           "variable_declarator"
           "lexical_declaration"
           "statement_block"
           "variable_declaration"
           "expression_statement")
       (when-let ((children (treesit-node-children node t)))
         (let ((child))
           (while (setq child (pop children))
             (pcase (treesit-node-type child)
               ((or "identifier"
                    "shorthand_property_identifier_pattern")
                (push child ids))
               ((or "formal_parameters"
                    "lexical_declaration"
                    "variable_declarator"
                    "variable_declaration"
                    "statement_block")
                (let ((subchildren (treesit-node-children child t)))
                  (setq children (append children subchildren))))
               ((or "required_parameter"
                    "object_pattern"
                    "pair_pattern"
                    "array_pattern"
                    "rest_pattern")
                (setq children (append children
                                       (treesit-node-children
                                        child t)))))))))
      ((and
        "for_statement"
        (guard (js-log-position-within-node (point) node)))
       (when-let ((children (treesit-node-children node t)))
         (let ((child))
           (while (setq child (pop children))
             (pcase (treesit-node-type child)
               ((or "identifier"
                    "shorthand_property_identifier_pattern")
                (push child ids))
               ("variable_declarator"
                (setq children (append children
                                       (treesit-node-children child t))))
               ((or "lexical_declaration"
                    "variable_declaration")
                (let ((subchildren (treesit-node-children child t)))
                  (setq children subchildren))))))))
      ((and
        "for_in_statement"
        (guard (js-log-position-within-node (point) node)))
       (when-let ((children (treesit-node-children node t)))
         (let ((child))
           (while (setq child (pop children))
             (pcase (treesit-node-type child)
               ((or "identifier"
                    "shorthand_property_identifier_pattern")
                (push child ids))
               ("variable_declarator"
                (setq children (append children
                                       (treesit-node-children child t))))
               ((or "lexical_declaration"
                    "variable_declaration")
                (let ((subchildren (treesit-node-children child t)))
                  (setq children subchildren))))))))
      ("function"
       (when-let* ((children (treesit-node-children node t))
                   (statement-block (seq-find (apply-partially
                                               #'js-log-check-node-type
                                               "statement_block")
                                              children)))
         (when (js-log-position-within-node (point)
                                            statement-block)
           (let ((child))
             (while (setq child (pop children))
               (pcase (treesit-node-type child)
                 ((or "identifier"
                      "shorthand_property_identifier_pattern")
                  (push child ids))
                 ("formal_parameters"
                  (setq children (append children
                                         (treesit-node-children child t))))
                 ("required_parameter"
                  (let ((subchildren (treesit-node-children child t)))
                    (setq children (append children subchildren)))))))))))
    ids))

(defun js-log-get-ids-from-parents ()
  "Extract IDs from parent nodes in a JavaScript AST."
  (let ((parents (js-log-get-parents))
        (result)
        (pos (point)))
    (dolist (node parents)
      (when-let ((type (and (js-log-position-within-node pos node)
                            (treesit-node-type node))))
        (let ((args (js-log-parse-node-ids node)))
          (when args
            (push args result)))))
    (flatten-list result)))

(defun js-log-get-visible-nodes ()
  "Collect unique visible JavaScript AST nodes."
  (delete-dups
   (seq-uniq
    (append
     (js-log-get-ids-from-parents)
     (js-log-visible-ids))
    #'treesit-node-eq)))

(defun js-log-treesit-sparse ()
  "Display sparse tree representation with node details."
  (mapcar #'js-log--get-node-or-children
          (treesit-node-children
           (treesit-buffer-root-node))))

(defun js-log-get-parents-with-children ()
  "Return list of parent nodes with their children."
  (let* ((parent-node (js-log-get-top-parent-node))
         (node (save-excursion
                 (js-log-backward-whitespace-ignore-comments)
                 (treesit-node-at (point) nil t)))
         (path (list parent-node)))
    (while (setq parent-node
                 (seq-find (apply-partially
                            #'js-log-current--node-child-p node)
                           (treesit-node-children
                            parent-node)))
      (push parent-node path))
    (reverse path)))

(defun js-log-node-backward-all ()
  "Navigate all JavaScript log nodes in reverse."
  (let ((nodes)
        (node)
        (prev-start))
    (while (setq node (when-let ((n (and (not (equal (point) prev-start))
                                         (js-log-get-node-list-ascending))))
                        (pcase (treesit-node-type (car n))
                          ("{" nil)
                          (_ (js-log-last-item n)))))
      (setq prev-start (point))
      (goto-char (treesit-node-start node))
      (push node nodes))
    nodes))

(defun js-log-node-ids-in-scope ()
  "Log node IDs from JavaScript scope."
  (let ((nodes (reverse (js-log-node-backward-all)))
        (node)
        (res))
    (while (setq node (pop nodes))
      (when-let ((item (js-log-parse-node-ids node)))
        (push item res)))
    (flatten-list res)))

(defun js-log-visible-ids ()
  "Get visible JavaScript node IDs."
  (setq js-log-current-node (or (js-log-last-item
                                 (js-log-get-node-list-ascending))))
  (save-excursion
    (let ((ids (or
                (js-log-node-ids-in-scope)))
          (prev-scope))
      (while
          (setq prev-scope
                (when-let ((next (car
                                  (js-log-get-node-list-ascending))))
                  (goto-char (treesit-node-start next))
                  (unless (treesit-node-eq next prev-scope)
                    next)))
        (setq ids (append ids (js-log-node-ids-in-scope))))
      ids)))

(defun js-log-random-hex-color ()
  "Log a random hexadecimal color code."
  (require 'shr-color)
  (cdr (nth (random (1- (length shr-color-html-colors-alist)))
            shr-color-html-colors-alist)))

(defun js-log-get-theme ()
  "Return CSS string with random background and contrasting text color."
  (let ((color (downcase (js-log-random-hex-color))))
    (if (or (= (length color) 4)
            (= (length color) 7))
        (format "'background-color: %s; color: %s'"
                color
                (if (>= (apply #'+ (funcall
                                    (with-no-warnings
                                      (if
                                          (fboundp
                                           'color-values)
                                          #'color-values
                                        #'x-color-values))
                                    color))
                        (* (apply #'+ (funcall
                                       (with-no-warnings
                                         (if
                                             (fboundp
                                              'color-values)
                                             #'color-values
                                           #'x-color-values))
                                       "white"))
                           .6))
                    "black"
                  "white")))))

;;;###autoload
(defun js-log-minibuffer-preview ()
  "Display preview of JavaScript log in minibuffer."
  (interactive)
  (let ((cand (js-log-get-current-candidate)))
    (let ((window (minibuffer-selected-window)))
      (when window
        (with-selected-window window
          (when-let* ((node (cdr (assoc (cdr cand)
                                        js-log-nodes-alist)))
                      (start (treesit-node-start node))
                      (end (treesit-node-end node)))
            (goto-char start)
            (pulse-momentary-highlight-region
             start end)))))))

(defvar js-log-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j")
                #'js-log-minibuffer-preview)
    map)
  "Keymap for minibuffer input with preview functionality on `C-j'.")

(defun js-log-read-visible-ids (prompt &optional predicate require-match
                                       initial-input hist def
                                       inherit-input-method)
  "Return list of IDs visible in current JavaScript code scope.

Argument PROMPT is a string to display as the prompt in the minibuffer.

Optional argument PREDICATE is a function to filter the completion candidates.

Optional argument REQUIRE-MATCH is a boolean; if non-nil, the user is not
allowed to exit unless the input matches one of the completions.

Optional argument INITIAL-INPUT is a string to insert before reading input.

Optional argument HIST is a symbol representing a history list and/or a cons
cell (HISTVAR . HISTPOS).

Optional argument DEF is the default value or a list of default values.

Optional argument INHERIT-INPUT-METHOD, if non-nil, means the minibuffer
inherits the current input method and the setting of
`enable-multibyte-characters'."
  (setq js-log-nodes-alist
        (mapcar #'js-log-node-cons
                (seq-sort-by #'treesit-node-end #'<
                             (js-log-get-visible-nodes))))
  (let* ((annot-fn (lambda (it)
                     (format
                      (propertize
                       (concat (propertize " " 'display '(space :align-to
                                                          40))
                               " %s")
                       'face 'completions-annotations)
                      (if-let ((node
                                (cdr (assoc (substring-no-properties
                                             it)
                                            js-log-nodes-alist))))
                          (js-log-annotate-parent-node node)
                        ""))))
         (strs (mapcar #'car
                       js-log-nodes-alist)))
    (minibuffer-with-setup-hook
        (lambda ()
          (use-local-map
           (make-composed-keymap js-log-minibuffer-map
                                 (current-local-map)))
          (add-hook 'after-change-functions
                    (lambda (&rest _)
                      (js-log-minibuffer-preview)) nil
                    t))
      (save-excursion
        (completing-read prompt
                         (lambda (str pred action)
                           (if (eq action 'metadata)
                               `(metadata
                                 (annotation-function . ,annot-fn))
                             (complete-with-action action strs
                                                   str pred)))
                         predicate require-match
                         initial-input hist def
                         inherit-input-method)))))

;;;###autoload
(defun js-log-insert-complete ()
  "Insert JavaScript identifier completion at point."
  (interactive)
  (let ((word
         (when-let ((sym (symbol-at-point)))
           (symbol-name
            sym))))
    (when-let
        ((item
          (js-log-read-visible-ids "Symbol: "
                                   (when word
                                     (lambda (it)
                                       (and (not (equal word it))
                                            (string-prefix-p word it)))))))
      (let ((parts))
        (setq parts
              (if-let* ((sym (symbol-at-point))
                        (word (symbol-name
                               sym)))
                  (progn
                    (if (string-prefix-p word item)
                        (list (substring-no-properties item (length word)))
                      (list " " item)))
                (list item)))
        (apply #'insert parts)))))

;;;###autoload
(defun js-log-jump-to-symbol ()
  "Navigate to chosen JavaScript symbol location."
  (interactive)
  (when-let* ((item (js-log-read-visible-ids "Symbol: "))
              (node (cdr (assoc item js-log-nodes-alist))))
    (goto-char (treesit-node-start node))))

;;;###autoload
(defun js-log ()
  "Insert formatted console.log with metadata and theme."
  (interactive)
  (let* ((pos (point))
         (pred (lambda (str)
                 (let* ((node
                         (cdr
                          (assoc-string str
                                        js-log-nodes-alist)))
                        (start (treesit-node-start node)))
                   (> pos start))))
         (theme (js-log-get-theme))
         (meta)
         (formatted))
    (setq meta (mapconcat (apply-partially #'format "%s")
                          (delq nil
                                (list (ignore-errors (or (js-log-which-func)
                                                        (treesit-add-log-current-defun)))
                                      (buffer-name (current-buffer))
                                      (line-number-at-pos (point))))
                          " "))
    (pcase (treesit-node-type
            (js-log-last-item (js-log-get-node-list-ascending)))
      ((and (pred (string= "arguments"))
            (guard (ignore-errors
                     (string= "console" (save-excursion
                                          (goto-char
                                           (treesit-node-start
                                            (js-log-last-item
                                             (js-log-get-node-list-ascending))))
                                          (when (equal
                                                 (treesit-node-type
                                                  (js-log-last-item
                                                   (js-log-get-node-list-ascending)))
                                                 "call_expression")
                                            (goto-char
                                             (treesit-node-start
                                              (js-log-last-item
                                               (js-log-get-node-list-ascending))))
                                            (substring-no-properties
                                             (treesit-node-text
                                              (treesit-node-at
                                               (point))))))))))
       (let ((item
              (js-log-read-visible-ids "Symbol: "
                                       pred))
             (word (thing-at-point 'symbol t)))
         (cond ((and word (string-prefix-p word item))
                (insert (substring-no-properties item (length word))))
               ((and word)
                (insert ",")
                (newline-and-indent)
                (insert
                 item
                 ","))
               ((save-excursion
                  (looking-back "," 0))
                (newline-and-indent)
                (insert
                 item
                 ","))
               ((save-excursion
                  (skip-chars-backward "\s\t\n\r\f")
                  (looking-back "," 0))
                (indent-according-to-mode)
                (insert
                 item
                 ","))
               (t (insert ",")
                  (newline-and-indent)
                  (insert
                   item ",")))))
      (_
       (let ((marked (list (js-log-read-visible-ids "Symbol: "
                                                    pred)))
             (indent-level)
             (indent-str)
             (result))
         (indent-according-to-mode)
         (setq indent-level (+ 2 (current-column)))
         (setq indent-str (make-string indent-level ?\ ))
         (setq formatted (concat (string-join
                                  marked
                                  ",\n")
                                 ","))
         (setq result (concat "console.log(\n"
                              indent-str "'%c" "<" meta
                              "> "
                              indent-str
                              (mapconcat (lambda (it)
                                           (concat
                                            it
                                            ": %o "))
                                         marked " ")
                              ":\\n' ," "\n" indent-str theme ", \n"
                              indent-str formatted
                              "\n" (make-string (current-column) ?\ ) ")"))
         (insert result)
         (forward-char -1)
         (skip-chars-backward "\s\t\n\r\f"))))))

(provide 'js-log)
;;; js-log.el ends here
