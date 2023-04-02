;;; js-log.el --- Insert js logs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/js-log
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "27.1"))

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

(defun js-log-prettier-format (string &rest options)
  "Apply prettier on STRING with OPTIONS.
Return list of two elements: status (t or nil) and string with result."
  (when-let ((prettier-cmd (let ((dir default-directory)
                                 (node-modules)
                                 (found))
                             (while (setq node-modules
                                          (unless found
                                            (setq dir (locate-dominating-file
                                                       dir
                                                       "node_modules"))))
                               (setq dir (let
                                             ((parent
                                               (file-name-directory
                                                (directory-file-name
                                                 (expand-file-name dir
                                                                   default-directory)))))
                                           (when (and
                                                  (file-exists-p dir)
                                                  (file-exists-p parent)
                                                  (not
                                                   (equal
                                                    (file-truename (directory-file-name
                                                                    (expand-file-name
                                                                     dir)))
                                                    (file-truename
                                                     (directory-file-name
                                                      (expand-file-name
                                                       parent))))))
                                             (if (file-name-absolute-p dir)
                                                 (directory-file-name parent)
                                               (file-relative-name parent)))))
                               (let ((file (expand-file-name
                                            "node_modules/.bin/prettier"
                                            node-modules)))
                                 (setq found
                                       (when (and (file-exists-p file)
                                                  (file-executable-p file))
                                         file))))
                             (or found (executable-find "prettier")))))
    (with-temp-buffer
      (insert string)
      (let ((status (apply #'call-process-region
                           (append
                            (list (point-min)
                                  (point-max)
                                  prettier-cmd
                                  t
                                  t
                                  nil)
                            (or (flatten-list options)
                                (list "--parser"
                                      "typescript"))))))
        (if (zerop status)
            (buffer-string)
          (message "%s" (buffer-string))
          nil)))))

(defun js-log-get-metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun js-log-get-current-candidate ()
  "Return cons filename for current completion candidate."
  (let (target)
    (run-hook-wrapped
     '((lambda ()
         (when (and (memq 'ivy--queue-exhibit post-command-hook)
                    (boundp 'ivy-text)
                    (boundp 'ivy--length)
                    (boundp 'ivy-last)
                    (fboundp 'ivy--expand-file-name)
                    (fboundp 'ivy-state-current))
           (cons
            (completion-metadata-get (ignore-errors
                                       (js-log-get-metadata))
                                     'category)
            (ivy--expand-file-name
             (if (and (> ivy--length 0)
                      (stringp (ivy-state-current ivy-last)))
                 (ivy-state-current ivy-last)
               ivy-text)))))
       (when (and (minibufferp) minibuffer-completion-table)
         (pcase-let* ((`(,category . ,candidates)
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
                            (completion-metadata-get
                             (js-log-get-metadata) 'category)
                            all))))
                      (contents (minibuffer-contents))
                      (top (if (test-completion contents
                                                minibuffer-completion-table
                                                minibuffer-completion-predicate)
                               contents
                             (let ((completions
                                    (completion-all-sorted-completions)))
                               (if (null completions)
                                   contents
                                 (concat
                                  (substring contents
                                             0 (or (cdr (last completions)) 0))
                                  (car completions)))))))
           (cons category (or (car (member top candidates)) top)))))
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

(defun js-log-check-node-type-by-re (type node)
  "Check whether NODE's type is TYPE.
TYPE can be a either string, or list of strings."
  (string-match-p type
                  (treesit-node-type node)))

(defun js-log-node-type-identifier-p (node)
  "Check whether NODE's type is TYPE.
TYPE can be a either string, or list of strings."
  (js-log-check-node-type '("identifier" ;; "member_expression"
                            "property_identifier")
                          node))

(defun js-log-node-cons (node)
  "Return a cons with NODE's text and NODE."
  (when node
    (cons (substring-no-properties (treesit-node-text node))
          node)))

(defun js-log-ids-top-level ()
  "Search for identifiers at top level.
Return alist of texts and nodes."
  (mapcar #'js-log-node-cons
          (remove nil
                  (mapcar
                   (lambda (it)
                     (treesit-search-subtree
                      it
                      (apply-partially
                       #'js-log-check-node-type '("identifier"
                                                  "property_identifier"))))
                   (treesit-node-children (treesit-buffer-root-node))))))

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

(defun js-log-get-node-while (fn node &rest args)
  "Apply FN with NODE and ARGS and it's parents."
  (when (treesit-node-p node)
    (let ((parents)
          (root (treesit-buffer-root-node)))
      (while (when-let ((parent (apply fn (append (list node) args))))
               (unless (treesit-node-eq root parent)
                 (push parent parents)
                 (setq node parent)
                 parent)))
      parents)))

(defun js-log-get-node-parent-ids ()
  "Return identifiers from all parents nodes."
  (let ((nodes (js-log-get-node-while 'treesit-node-parent
                                      (treesit-node-at (point)))))
    (mapcar #'js-log-node-cons (mapcar
                                (lambda (it)
                                  (treesit-search-subtree
                                   it
                                   (lambda (it)
                                     (equal "identifier"
                                            (treesit-node-type it)))
                                   nil t))
                                nodes))))

(defun js-log-node-get-siblings (step &optional named)
  "Return siblings for node at point.
Negative STEP means prev sibling, positive means next.
If NAMED is non-nil, look for named
siblings only."
  (let ((node (treesit-node-at (point)))
        (fn (if (> step 0)
                'treesit-node-next-sibling
              'treesit-node-prev-sibling))
        (siblings))
    (while (setq node (funcall fn node named))
      (push node siblings))
    (if (> step 0)
        (nreverse siblings)
      siblings)))

(defun js-log-current-node-parents ()
  "Return parents for current node."
  (cl-loop for node = (treesit-node-at (point))
           then (treesit-node-parent node)
           while node
           if (eq (treesit-node-start node)
                  (point))
           collect node))

(defun js-log-get-node-siblings-all ()
  "Return siblings for node at point.
Negative STEP means prev sibling, positive means next.
If NAMED is non-nil, look for named
siblings only."
  (let ((right-siblings (js-log-node-get-siblings 1))
        (left-siblings (js-log-node-get-siblings -1))
        (node (treesit-node-at (point))))
    (cond ((and right-siblings
                (not left-siblings))
           (save-excursion
             (goto-char (treesit-node-end (car (last right-siblings))))
             (setq left-siblings (js-log-node-get-siblings -1))))
          ((and left-siblings
                (not right-siblings))
           (save-excursion
             (goto-char (treesit-node-start (car left-siblings)))
             (setq right-siblings
                   (js-log-node-get-siblings 1)))))
    (seq-sort-by
     (lambda (n)
       (+ (treesit-node-start n)
          (treesit-node-end n)))
     '<
     (append (list node) left-siblings right-siblings))))

(defun js-log-node-backward-all ()
  "Return nodes before current position and in current scope."
  (let ((nodes)
        (node))
    (while (setq node (when-let ((n (js-log-get-node-list-ascending)))
                        (pcase (treesit-node-type (car n))
                          ("{" nil)
                          (_ (car (last
                                   n))))))
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
    ((or "lexical_declaration"
         "variable_declaration")
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
    ids))

(defun js-log-get-node-list-ascending ()
  "Return ascending node list."
  (save-excursion
    (skip-chars-backward "\n\s\t")
    (let* ((node-list
            (cl-loop for node = (treesit-node-at (point))
                     then (treesit-node-parent node)
                     while node
                     if (eq (treesit-node-start node)
                            (point))
                     collect node))
           (largest-node (car (last node-list)))
           (parent (treesit-node-parent largest-node)))
      (if (null largest-node)
          (list (treesit-node-at (point))
                (treesit-node-parent
                 (treesit-node-at (point))))
        (append node-list (list parent))))))

(defun js-log-jump-to-node-end (node)
  "Jump to end of NODE and highlight start and end."
  (when-let ((start (treesit-node-start node))
             (end (treesit-node-end node)))
    (goto-char end)
    (pulse-momentary-highlight-region start end)
    node))

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
  (with-minibuffer-selected-window
    (let* ((node (cdr (assoc (cdr (js-log-get-current-candidate))
                             js-log-visible-nodes-alist)))
           (start (treesit-node-start node))
           (end (treesit-node-end node)))
      (save-excursion
        (goto-char start)
        (pulse-momentary-highlight-region
         start end)))))

(defvar js-log-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j")
                'js-log-minibuffer-preview)
    map))

(defun js-log-read-visible-ids ()
  "Read NODES in minibuffer."
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
      (completing-read "Symbol: "
                       (lambda (str pred action)
                         (if (eq action 'metadata)
                             `(metadata
                               (annotation-function . ,annot-fn))
                           (complete-with-action action strs
                                                 str pred)))))))

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
            (when (looking-back "," 0)
              (newline-and-indent))
            (insert
             (concat (js-log-read-visible-ids)
                     ","))))))
      (_ (let ((marked (list (js-log-read-visible-ids)))
               (result))
           (setq formatted (concat (string-join
                                    marked
                                    ",\n")
                                   ",\n"))
           (setq result (concat "console.log('%c" "<" meta
                                "> "
                                (mapconcat (lambda (it)
                                             (concat
                                              it
                                              ": %o "))
                                           marked " ")
                                ":\\n' ," theme ", "
                                formatted
                                ")"))
           (insert result)
           (when (re-search-backward "," nil t 1)
             (forward-char 1)))))))

(provide 'js-log)
;;; js-log.el ends here