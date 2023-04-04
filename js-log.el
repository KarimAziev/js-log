;;; js-log.el --- Insert js logs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/js-log
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "28.2"))

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

(defun js-log-get-metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun js-log-minibuffer-auto-default-candidates ()
  "Return all current completion candidates from the minibuffer."
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
  "Return the currently selected item in Ivy."
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
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target."
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
    js-log-default-top-minibuffer-completion))

(defun js-log-get-current-candidate ()
  "Return cons filename for current completion candidate."
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
                            "program"))

(defun js-log-check-node-type (type node)
  "Check whether NODE's type is TYPE.
TYPE can be a either string, or list of strings."
  (if (listp type)
      (member (treesit-node-type node)
              type)
    (equal (treesit-node-type node) type)))

(defun js-log-node-cons (node)
  "Return a cons with NODE's text and NODE."
  (when node
    (cons (substring-no-properties (treesit-node-text node))
          node)))

;;;###autoload
(defun js-log-node-at-point ()
  "Describe NODE at point."
  (interactive)
  (let* ((node (treesit-node-at (point)))
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

(defun js-log-annotate-node (node)
  "Annotate NODE."
  (let* ((type (treesit-node-type node))
         (field (treesit-node-field-name node)))
    (string-join
     (remove nil
             (append
              (list
               (when type
                 (propertize type 'face
                             'font-lock-keyword-face))
               (when field
                 (propertize field 'face
                             'font-lock-builtin-face)))))
     " - ")))

(defun js-log-node-backward-all ()
  "Return nodes before current position and in current scope."
  (let ((nodes)
        (node)
        (prev-start))
    (while (setq node (when-let ((n (and (not (equal (point) prev-start))
                                         (js-log-get-node-list-ascending))))
                        (pcase (treesit-node-type (car n))
                          ("{" nil)
                          (_ (car (last
                                   n))))))
      (setq prev-start (point))
      (goto-char (treesit-node-start node))
      (push node nodes))
    nodes))

(defun js-log-get-object-pattern-ids (node)
  "Return identifiers from NODE.
NODE's type should be object_pattern."
  (seq-filter
   (apply-partially
    #'js-log-check-node-type
    '("identifier"
      "shorthand_property_identifier_pattern"))
   (remove nil
           (flatten-list (treesit-induce-sparse-tree
                          node
                          (lambda (item) item))))))

(defun js-log-get-node-id (node)
  "Return identifier or list of identifiers from NODE on top level."
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

(defun js-log-node-ids-in-scope ()
  "Return list of visible in scope identifiers."
  (let ((nodes (reverse (js-log-node-backward-all)))
        (node)
        (res))
    (while (setq node (pop nodes))
      (when-let ((item (js-log-get-node-id node)))
        (push item res)))
    (flatten-list res)))

(defun js-log-extract-args ()
  "Extract arguments if point located before body of function or arrow function."
  (when-let* ((prev-nodes (js-log-get-node-list-ascending))
              (node (car (last prev-nodes))))
    (let* ((params (pcase (treesit-node-type node)
                     ("arrow_function"
                      (car (treesit-node-children
                            node)))
                     ("formal_parameters"
                      node)))
           (args (remove nil
                         (flatten-list (treesit-induce-sparse-tree
                                        params
                                        (lambda (item) item))))))
      (setq args (seq-filter
                  (apply-partially
                   #'js-log-check-node-type
                   '("identifier"
                     "shorthand_property_identifier_pattern"))
                  args)))))

(defun js-log-visible-ids ()
  "Return list of visible identifiers from all scopes."
  (let ((ids (js-log-node-ids-in-scope))
        (prev-scope))
    (while
        (setq prev-scope
              (when-let ((next (car
                                (js-log-get-node-list-ascending))))
                (goto-char (treesit-node-start next))
                (when-let* ((prev-nodes (js-log-get-node-list-ascending))
                            (node (car (last prev-nodes))))
                  (let ((args (pcase (treesit-node-type node)
                                ("arrow_function"
                                 (js-log-extract-args))
                                ("formal_parameters"
                                 (js-log-extract-args)))))
                    (setq ids (append ids args))))
                (unless (treesit-node-eq next prev-scope)
                  next)))
      (setq ids (append ids (js-log-node-ids-in-scope))))
    (seq-sort-by #'treesit-node-end '< ids)))

(defun js-log-get-node-list-ascending ()
  "Return ascending node list."
  (save-excursion
    (skip-chars-backward "\s\t\n")
    (while (and (equal "comment" (treesit-node-type (treesit-node-at (point))))
                (not (bobp)))
      (goto-char (treesit-node-start (treesit-node-at (point))))
      (skip-chars-backward "\s\t\n"))
    (let* ((node-list
            (cl-loop for node = (treesit-node-at (point))
                     then
                     (treesit-node-parent
                      node)
                     while
                     node
                     if
                     (eq
                      (treesit-node-start
                       node)
                      (point))
                     collect
                     node))
           (largest-node (car (last node-list)))
           (parent (treesit-node-parent largest-node)))
      (if (null largest-node)
          (list (treesit-node-at (point))
                (treesit-node-parent
                 (treesit-node-at (point))))
        (append node-list (list parent))))))

(defun js-log-random-hex-color ()
  "Return random hexadecimal-color."
  (require 'shr-color)
  (cdr (nth (random (1- (length shr-color-html-colors-alist)))
            shr-color-html-colors-alist)))

(defun js-log-get-theme ()
  "Return string with styles for console.log."
  (let ((color (downcase (js-log-random-hex-color))))
    (if (or (= (length color) 4)
            (= (length color) 7))
        (format "'background-color: %s; color: %s'"
                color
                (if (>= (apply #'+ (x-color-values
                                    color))
                        (* (apply #'+ (x-color-values "white")) .6))
                    "black"
                  "white")))))

(defvar-local js-log-visible-nodes-alist nil)


(defun js-log-minibuffer-preview ()
  "Jump to current minibuffer candidate."
  (interactive)
  (let ((cand (js-log-get-current-candidate)))
    (let ((window (minibuffer-selected-window)))
      (when window
        (with-selected-window window
          (let* ((node (cdr (assoc (cdr cand)
                                   js-log-visible-nodes-alist)))
                 (start (treesit-node-start node))
                 (end (treesit-node-end node)))
            (goto-char start)
            (pulse-momentary-highlight-region
             start end)))))))

(defvar js-log-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j")
                'js-log-minibuffer-preview)
    map))

(defun js-log-read-visible-ids (prompt &optional predicate require-match
                                       initial-input hist def
                                       inherit-input-method)
  "Read NODES in minibuffer.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
See `completing-read' for more details on completion,
PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD."
  (setq js-log-visible-nodes-alist
        (copy-tree
         (save-excursion
           (mapcar #'js-log-node-cons
                   (js-log-visible-ids)))))
  (let* ((annot-fn (lambda (it)
                     (concat " "
                             (if-let
                                 ((node
                                   (cdr (assoc (substring-no-properties
                                                it)
                                               js-log-visible-nodes-alist))))
                                 (js-log-annotate-node node)
                               " "))))
         (strs (mapcar #'car
                       js-log-visible-nodes-alist)))
    (minibuffer-with-setup-hook
        (lambda ()
          (use-local-map
           (make-composed-keymap js-log-minibuffer-map
                                 (current-local-map))))
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
  "Read js items and insert them with console.log."
  (interactive)
  (let ((word (when-let ((sym (symbol-at-point)))
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
  "Read js items and insert them with console.log."
  (interactive)
  (when-let* ((item (js-log-read-visible-ids "Symbol: "))
              (node (cdr (assoc item js-log-visible-nodes-alist))))
    (goto-char (treesit-node-start node))))

;;;###autoload
(defun js-log ()
  "Read js items and insert them with console.log."
  (interactive)
  (let ((buff (buffer-name (current-buffer)))
        (meta)
        (formatted)
        (theme (js-log-get-theme)))
    (setq meta (mapconcat (apply-partially #'format "%s")
                          (list buff (line-number-at-pos (point)))
                          " "))
    (pcase (treesit-node-type
            (car (last (js-log-get-node-list-ascending))))
      ("arguments"
       (let ((name
              (ignore-errors
                (save-excursion
                  (goto-char
                   (treesit-node-start
                    (car
                     (last
                      (js-log-get-node-list-ascending)))))
                  (when (equal
                         (treesit-node-type
                          (car
                           (last
                            (js-log-get-node-list-ascending))))
                         "call_expression")
                    (goto-char
                     (treesit-node-start
                      (car
                       (last
                        (js-log-get-node-list-ascending)))))
                    (substring-no-properties
                     (treesit-node-text
                      (treesit-node-at
                       (point)))))))))
         (pcase name
           ("console"
            (let ((item (js-log-read-visible-ids "Symbol: "))
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
                       (skip-chars-backward "\s\t\n")
                       (looking-back "," 0))
                     (indent-for-tab-command)
                     (insert
                      item
                      ","))
                    (t (insert ",")
                       (newline-and-indent)
                       (insert
                        item ","))))))))
      (_ (let ((marked (list (js-log-read-visible-ids "Symbol: ")))
               (indent-level)
               (indent-str)
               (result))
           (indent-for-tab-command)
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
           (skip-chars-backward "\s\t\n"))))))

(provide 'js-log)
;;; js-log.el ends here