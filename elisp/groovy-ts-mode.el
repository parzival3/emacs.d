;;; groovy-ts-mode.el --- tree-sitter support for Groovy  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author     : Enrico Tolotto <etolotto@gmail.com>
;; Maintainer : Theodor Thornhill <etolotto@gmail.com>
;; Created    : Decembeer 2023
;; Keywords   : groovy languages tree-sitter

;;; Commentary:
;;

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))
(require 'c-ts-common) ; For comment indent and filling.

(defcustom groovy-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `groovy-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp)

(defvar groovy-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Taken from the groovy-ts-langs version
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?%  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?\' "\""    table)
    (modify-syntax-entry ?\240 "."   table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?\^m "> b" table)
    (modify-syntax-entry ?@ "'" table)
    table)
  "Syntax table for `groovy-ts-mode'.")

(defvar groovy-ts-mode--indent-rules
  `((groovy
     ((parent-is "program") column-0 0)
     ((match "}" "element_value_array_initializer")
      parent-bol 0)
     ((node-is "}") column-0 c-ts-common-statement-offset)
     ((node-is ")") parent-bol 0)
     ((node-is "else") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((and (parent-is "comment") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "text_block") no-indent)
     ((parent-is "class_body") column-0 c-ts-common-statement-offset)
     ((parent-is "array_initializer") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "annotation_type_body") column-0 c-ts-common-statement-offset)
     ((parent-is "interface_body") column-0 c-ts-common-statement-offset)
     ((parent-is "constructor_body") column-0 c-ts-common-statement-offset)
     ((parent-is "enum_body_declarations") parent-bol 0)
     ((parent-is "enum_body") column-0 c-ts-common-statement-offset)
     ((parent-is "switch_block") column-0 c-ts-common-statement-offset)
     ((parent-is "record_declaration_body") column-0 c-ts-common-statement-offset)
     ((query "(method_declaration (block _ @indent))") parent-bol groovy-ts-mode-indent-offset)
     ((query "(method_declaration (block (_) @indent))") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "local_variable_declaration") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "expression_statement") parent-bol groovy-ts-mode-indent-offset)
     ((match "type_identifier" "field_declaration") parent-bol 0)
     ((parent-is "field_declaration") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "return_statement") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "variable_declarator") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "method_invocation") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "switch_rule") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "switch_label") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "ternary_expression") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "lambda_expression") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "element_value_array_initializer") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "function_definition") parent-bol 0)
     ((parent-is "conditional_expression") first-sibling 0)
     ((parent-is "assignment_expression") parent-bol 2)
     ((parent-is "binary_expression") parent 0)
     ((parent-is "parenthesized_expression") first-sibling 1)
     ((parent-is "argument_list") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "annotation_argument_list") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "modifiers") parent-bol 0)
     ((parent-is "formal_parameters") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "formal_parameter") parent-bol 0)
     ((parent-is "init_declarator") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "if_statement") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "for_statement") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "while_statement") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "switch_statement") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "case_statement") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "labeled_statement") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "do_statement") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "block") column-0 c-ts-common-statement-offset)))
  "Tree-sitter indent rules.")

(defvar groovy-ts-mode--keywords
  '("abstract" "assert" "break" "case" "catch"
    "class" "continue" "default" "do" "else"
    "enum" "exports" "extends" "final" "finally"
    "for" "if" "implements" "import" "instanceof"
    "interface" "module" "native" "new" "non-sealed"
    "open" "opens" "package" "private" "protected"
    "provides" "public" "requires" "return" "sealed"
    "static" "strictfp" "switch" "synchronized"
    "throw" "throws" "to" "transient" "transitive"
    "try" "uses" "volatile" "while" "with" "record"
    "@interface")
  "Groovy keywords for tree-sitter font-locking.")


(defvar groovy-ts-mode--operators
  '("+" ":" "++" "-" "--" "&" "&&" "|" "||" "="
    "!=" "==" "*" "/" "%" "<" "<=" ">" ">="
    "-=" "+=" "*=" "/=" "%=" "->" "^" "^="
    "|=" "~" ">>" ">>>" "<<" "::" "?" "&=")
  "Groovy operators for tree-sitter font-locking.")

(defvar groovy-ts-mode--font-lock-settings
  `(:language groovy
   :override t
   :feature comment
   ((line_comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face)
   :language groovy
   :override t
   :feature constant
   (((identifier) @font-lock-constant-face
      (:match "\\`[A-Z_][A-Z_\\d]*\\'" @font-lock-constant-face))
     [(true) (false)] @font-lock-constant-face)
   :language groovy
   :override t
   :feature keyword
   ([,@groovy-ts-mode--keywords
      (this)
      (super)] @font-lock-keyword-face
      (labeled_statement
       (identifier) @font-lock-keyword-face))
   :language groovy
   :override t
   :feature operator
   ([,@groovy-ts-mode--operators] @font-lock-operator-face
     "@" @font-lock-constant-face)
   :language groovy
   :override t
   :feature annotation
   ((annotation
      name: (identifier) @font-lock-constant-face)

     (marker_annotation
      name: (identifier) @font-lock-constant-face))
   :language groovy
   :override t
   :feature literal
   ((null_literal) @font-lock-constant-face
     (binary_integer_literal)  @font-lock-number-face
     (decimal_integer_literal) @font-lock-number-face
     (hex_integer_literal) @font-lock-number-face
     (octal_integer_literal) @font-lock-number-face
     (decimal_floating_point_literal) @font-lock-number-face
     (hex_floating_point_literal) @font-lock-number-face)
   :language groovy
   :override t
   :feature type
   ((annotation_type_declaration
      name: (identifier) @font-lock-type-face)

     (interface_declaration
      name: (identifier) @font-lock-type-face)

     (class_declaration
      name: (identifier) @font-lock-type-face)

     (record_declaration
      name: (identifier) @font-lock-type-face)

     (enum_declaration
      name: (identifier) @font-lock-type-face)

     (constructor_declaration
      name: (identifier) @font-lock-type-face)

     (compact_constructor_declaration
      name: (identifier) @font-lock-type-face)

     (field_access
      object: (identifier) @font-lock-type-face)

     (method_reference (identifier) @font-lock-type-face)

     (scoped_identifier (identifier) @font-lock-constant-face)

     ((scoped_identifier name: (identifier) @font-lock-type-face)
      (:match "\\`[A-Z]" @font-lock-type-face))

     (type_identifier) @font-lock-type-face

     [(boolean_type)
      (integral_type)
      (floating_point_type)
      (void_type)] @font-lock-type-face)
   :language groovy
   :override t
   :feature definition
   ((annotation_type_element_declaration
      name: (identifier) @font-lock-function-name-face)

     (method_declaration
      name: (identifier) @font-lock-function-name-face)

     (variable_declarator
      name: (identifier) @font-lock-variable-name-face)

     (element_value_pair
      key: (identifier) @font-lock-property-use-face)

     (formal_parameter
      name: (identifier) @font-lock-variable-name-face)

     (catch_formal_parameter
      name: (identifier) @font-lock-variable-name-face))
   :language groovy
   :override t
   :feature expression
   ((method_invocation
      object: (identifier) @font-lock-variable-use-face)

     (method_invocation
      name: (identifier) @font-lock-function-call-face)

     (argument_list (identifier) @font-lock-variable-name-face)

     (expression_statement (identifier) @font-lock-variable-use-face))

   :language groovy
   :feature bracket
   ((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language groovy
   :feature delimiter
   ((["," ":" ";"]) @font-lock-delimiter-face))
  "Tree-sitter font-lock settings for `groovy-ts-mode'.")

;;;###autoload
(define-derived-mode groovy-ts-mode prog-mode "groovy[ts]"
  "Major mode for editing groovy with tree-sitter."
  :syntax-table groovy-ts-mode--syntax-table
  :group 'groovy

  (setq-local font-lock-defaults nil)
  (unless (treesit-ready-p 'groovy)
    (error "Tree-sitter for Groovy isn't available"))

  (treesit-parser-create 'groovy)

  ;; Comments.
  (c-ts-common-comment-setup)

  (groovy-ts-setup))


(defun groovy-ts-setup ()
  "Setup treesit for groovy-ts-mode."
  ;; Our tree-sitter setup goes here.

  ;; This handles font locking -- more on that below.
  (setq-local treesit-font-lock-settings
               (apply #'treesit-font-lock-rules
                    groovy-ts-mode--font-lock-settings))

  ;; This handles indentation -- again, more on that below.
  (setq-local treesit-simple-indent-rules groovy-ts-mode--indent-rules)

  ;; ... everything else we talk about go here also ...

  ;; End with this
  (treesit-major-mode-setup))

(provide 'groovy-ts-mode)

;;; groovy-ts-mode.el ends here
