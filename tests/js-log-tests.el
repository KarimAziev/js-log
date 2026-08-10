;;; js-log-tests.el --- Tests for js-log -*- lexical-binding: t; -*-

(require 'ert)
(require 'js-log)

(defconst js-log-tests--terminated-source
  (concat "const myVal = 3;\n"
          "    \n"
          ";(function () {\n"
          "  console.log(\"IIFE runs immediately\", myVal);\n"
          "})()\n\n"
          "const myVal2 = 5;"))

(defconst js-log-tests--asi-source
  (concat "const myVal = 3\n"
          "    \n"
          ";(function () {\n"
          "  console.log(\"IIFE runs immediately\", myVal);\n"
          "})()\n\n"
          "const myVal2 = 5;"))

(defmacro js-log-tests--with-js-buffer (source &rest body)
  "Evaluate BODY in a `js-ts-mode' buffer containing SOURCE."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (treesit-ready-p 'javascript t))
     (with-temp-buffer
       (insert ,source)
       (js-ts-mode)
       ,@body)))

(defun js-log-tests--logical-texts ()
  "Return the current buffer's top-level logical statement texts."
  (mapcar (lambda (record)
            (buffer-substring-no-properties (aref record 1)
                                            (aref record 2)))
          (js-log--logical-statements
           (treesit-buffer-root-node 'javascript))))

(defun js-log-tests--visible-texts ()
  "Return visible identifier texts at point, sorted alphabetically."
  (sort (mapcar (lambda (node)
                  (treesit-node-text node t))
                (js-log-get-visible-nodes))
        #'string-lessp))

(ert-deftest js-log-logical-statements-fold-empty-semicolon-prefix ()
  (js-log-tests--with-js-buffer js-log-tests--terminated-source
    (should
     (equal (js-log-tests--logical-texts)
            '("const myVal = 3;"
              ";(function () {\n  console.log(\"IIFE runs immediately\", myVal);\n})()"
              "const myVal2 = 5;")))))

(ert-deftest js-log-logical-statements-reassign-asi-guard-semicolon ()
  (js-log-tests--with-js-buffer js-log-tests--asi-source
    (should
     (equal (js-log-tests--logical-texts)
            '("const myVal = 3"
              ";(function () {\n  console.log(\"IIFE runs immediately\", myVal);\n})()"
              "const myVal2 = 5;")))))

(ert-deftest js-log-logical-statements-support-typescript-parser ()
  (skip-unless (treesit-ready-p 'typescript t))
  (with-temp-buffer
    (insert js-log-tests--asi-source)
    (typescript-ts-mode)
    (should
     (equal
      (mapcar (lambda (record)
                (buffer-substring-no-properties (aref record 1)
                                                (aref record 2)))
              (js-log--logical-statements
               (treesit-buffer-root-node 'typescript)))
      '("const myVal = 3"
        ";(function () {\n  console.log(\"IIFE runs immediately\", myVal);\n})()"
        "const myVal2 = 5;")))))

(ert-deftest js-log-end-of-statement-observes-semicolon-ownership ()
  (dolist (case `((,js-log-tests--terminated-source . "const myVal = 3;")
                  (,js-log-tests--asi-source . "const myVal = 3")))
    (js-log-tests--with-js-buffer (car case)
      (goto-char (point-min))
      (js-log-end-of-statement)
      (should
       (equal (buffer-substring-no-properties (point-min) (point))
              (cdr case))))))

(ert-deftest js-log-statement-navigation-uses-three-logical-siblings ()
  (dolist (source (list js-log-tests--terminated-source
                        js-log-tests--asi-source))
    (js-log-tests--with-js-buffer source
      (goto-char (point-min))
      (let ((starts (list (point))))
        (js-log-next-statement)
        (push (point) starts)
        (should (looking-at-p ";(function"))
        (js-log-next-statement)
        (push (point) starts)
        (should (looking-at-p "const myVal2"))
        (js-log-previous-statement)
        (should (looking-at-p ";(function"))
        (should (= (length (delete-dups starts)) 3))))))

(ert-deftest js-log-logical-node-resolves-semicolon-to-guarded-expression ()
  (dolist (source (list js-log-tests--terminated-source
                        js-log-tests--asi-source))
    (js-log-tests--with-js-buffer source
      (goto-char (point-min))
      (search-forward ";(function")
      (goto-char (match-beginning 0))
      (let ((node (js-log--node-at-point)))
        (should (equal (treesit-node-type node) "expression_statement"))
        (should (string-prefix-p "(function" (treesit-node-text node t)))))))

(ert-deftest js-log-parent-resolution-does-not-enter-ended-function ()
  (js-log-tests--with-js-buffer
      "function outer() { const hidden = 1; }\n\nconst visible = 2;"
    (goto-char (point-min))
    (search-forward "}")
    (let ((types (mapcar #'treesit-node-type
                         (js-log-get-parents (point)))))
      (should (member "program" types))
      (should-not (member "function_declaration" types))
      (should-not (member "statement_block" types)))
    (should (equal (js-log-tests--visible-texts) '("outer")))))

(ert-deftest js-log-visible-nodes-do-not-leak-from-ended-scopes ()
  (dolist (case
           '(("function foo(param) {\n  const hidden = 1;\n}\nPOINT\nconst visible = 2;"
              "foo")
             ("const outer = 0;\nif (outer) {\n  const hidden = 1;\n}\nPOINT\nconst after = 2;"
              "outer")))
    (js-log-tests--with-js-buffer (car case)
      (goto-char (point-min))
      (search-forward "POINT")
      (replace-match "")
      (should (equal (js-log-tests--visible-texts) (cdr case))))))

(ert-deftest js-log-visible-nodes-keep-function-locals-inside-function ()
  (js-log-tests--with-js-buffer
      "function foo(param) {\n  const local = 1;\n  POINT\n}\nconst after = 2;"
    (goto-char (point-min))
    (search-forward "POINT")
    (replace-match "")
    (should (equal (js-log-tests--visible-texts)
                   '("foo" "local" "param")))))

(ert-deftest js-log-visible-nodes-keep-program-bindings-at-eob ()
  (js-log-tests--with-js-buffer "const atEob = 1;"
    (goto-char (point-max))
    (should (equal (js-log-tests--visible-texts) '("atEob")))))

(provide 'js-log-tests)
;;; js-log-tests.el ends here
