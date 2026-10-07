;;;; Load axllm and src/template.lisp, then call RUN-TEMPLATE-TESTS.
;;;; Kept independent of the parent-owned ASDF test-system wiring.

(defpackage #:axllm/template-tests
  (:use #:cl)
  (:export #:run-template-tests))

(in-package #:axllm/template-tests)

(defvar *checks* 0)

(defun check (expected actual)
  (incf *checks*)
  (assert (axllm/core::core-value-equal expected actual) ()
          "Expected ~S, got ~S" expected actual))

(defun render-error (source vars &optional (context "test"))
  (handler-case
      (progn (axllm::render-template-content source vars context)
             (error "Expected a template error for ~S" source))
    (ax:ax-error (condition) (ax:ax-error-message condition))))

(defun run-fixtures ()
  (let ((count 0)
        (root (let ((override (uiop:getenv "AXIR_CONFORMANCE_DIR")))
                (if override (uiop:ensure-directory-pathname override)
                    (asdf:system-relative-pathname "axllm" "../../ir/conformance/")))))
    (dolist (file (directory (merge-pathnames "prompt/*.json" root)))
      (let* ((fixture (ax:parse-json (uiop:read-file-string file)))
             (kind (ax:jget fixture "kind"))
             (source (ax:jget fixture "template"))
             (vars (ax:jget fixture "vars" (ax:object)))
             (required (ax:jget fixture "required_variables" #())))
        (cond
          ((equal kind "template")
           (check (ax:jget fixture "expected_output")
                  (axllm::render-template-content source vars))
           (axllm/conformance:record-result "prompt" file :semantic)
           (incf count))
          ((member kind '("template_validate" "template_validation") :test #'equal)
           (check (ax:jget fixture "expected_result")
                  (axllm::validate-prompt-template-syntax source "fixture" required))
           (axllm/conformance:record-result "prompt" file :semantic)
           (incf count))
          ((equal kind "template_error")
           (let ((actual (if (equal (ax:jget fixture "operation") "validate")
                             (axllm::validate-prompt-template-syntax source "fixture" required)
                             (render-error source vars "fixture"))))
             (incf *checks*)
             (assert (and (stringp actual)
                          (search (ax:jget fixture "expected_error_contains") actual)) ()
                     "Fixture ~A: unexpected error ~S" file actual))
           (axllm/conformance:record-result "prompt" file :validation-error)
           (incf count))
          ((and (stringp kind) (search "template" kind))
           (error "Unhandled template fixture kind ~S in ~A" kind file)))))
    (assert (plusp count))
    (format t "Template conformance: ~D fixtures PASS~%" count)))

(defun run-template-tests ()
  (let ((*checks* 0))
    (run-fixtures)
    ;; JSON model, exact scalar spelling, and no recursive interpolation.
    (check "true false 0 2.5 {{ absent }}"
           (axllm::render-template-content "{{ yes }} {{ no }} {{ n }} {{ f }} {{ text }}"
                                           (ax:parse-json
                                            "{\"yes\":true,\"no\":false,\"n\":0,\"f\":2.5,\"text\":\"{{ absent }}\"}")))
    (check "" (axllm::render-template-content ""))
    (check "{{}} {{ no-close }" (axllm::render-template-content "{{}} {{ no-close }"))
    (check "if include" (axllm::render-template-content
                         "{{ if }} {{ include }}" (ax:object "if" "if" "include" "include")))
    (check "AokB" (axllm::render-template-content
                  "A{{ if outer }}{{ if inner }}bad{{ else }}{{ value }}{{ /if }}{{ else }}{{ missing }}{{ /if }}B"
                  (ax:object "outer" ax:true "inner" ax:false "value" "ok")))
    (check "" (axllm::render-template-content "{{ if x }}{{ absent }}{{ /if }}"
                                             (ax:object "x" ax:false)))
    (check "2" (axllm::render-template-content "{{ items.length }}"
                                              (ax:object "items" #(1 2))))
    ;; A null leaf is present but not interpolatable. A null intermediate,
    ;; or a scalar intermediate (even a string), is missing, not a type error.
    (check "test:1:1 Missing template variable 'x'" (render-error "{{ x }}" (ax:object)))
    (dolist (value (list :null #() (ax:object) nil))
      (check "test:1:1 Variable 'x' must be string, number, or boolean"
             (render-error "{{ x }}" (ax:object "x" value))))
    (dolist (value (list :null "abc" 0 ax:true ax:false #()))
      (check "test:1:1 Missing template variable 'x.y'"
             (render-error "{{ x.y }}" (ax:object "x" value))))
    (check "test:1:1 Missing template variable 'snake_case'"
           (render-error "{{ snake_case }}" (ax:object "snakeCase" "not an alias")))
    (dolist (value (list :null 0 1 "false" "" #() (ax:object) nil))
      (check "test:1:1 Condition 'x' must be boolean"
             (render-error "{{ if x }}yes{{ /if }}" (ax:object "x" value))))
    (check "test:1:1 Missing template variable 'x'"
           (render-error "{{ if x }}yes{{ /if }}" (ax:object)))
    ;; Equality is strict string equality, not truthiness or coercion. Empty
    ;; strings and literal backslashes are allowed; escapes are not decoded.
    (dolist (value (list :null 1 ax:true ax:false #() (ax:object) "TRUE"))
      (check "no" (axllm::render-template-content
                   "{{ if x === 'true' }}yes{{ else }}no{{ /if }}" (ax:object "x" value))))
    (dolist (case '(("" "''") ("a\\n" "'a\\n'") ("a'b" "\"a'b\"") ("😀" "'😀'")))
      (check "yes" (axllm::render-template-content
                    (format nil "{{ if x === ~A }}yes{{ else }}no{{ /if }}" (second case))
                    (ax:object "x" (first case)))))
    (check "test:1:1 Missing template variable 'x'"
           (render-error "{{ if x === '' }}yes{{ /if }}" (ax:object)))
    ;; Parse both branches, even when an invalid branch would be unselected.
    (dolist (case '(("{{ }}" "test:1:1 Invalid tag ''")
                    ("{{ a-b }}" "test:1:1 Invalid tag 'a-b'")
                    ("{{ items.0 }}" "test:1:1 Invalid tag 'items.0'")
                    ("{{ x..y }}" "test:1:1 Invalid tag 'x..y'")
                    ("{{ (print x) }}" "test:1:1 Invalid tag '(print x)'")
                    ("{{{x}}}" "test:1:1 Invalid tag '{x'")
                    ("{{ else }}" "test:1:1 Unexpected 'else'")
                    ("{{ /if }}" "test:1:1 Unexpected '/if'")
                    ("{{ if x }}" "test:1:1 Unclosed 'if' block")
                    ("{{ if x }}{{ else }}" "test:1:1 Unclosed 'if' block")
                    ("{{ if x }}{{ else }}{{ else }}{{ /if }}" "test:1:21 Unexpected 'else'")
                    ("{{ if x }}{{ if y }}" "test:1:11 Unclosed 'if' block")
                    ("{{ if x == 'a' }}{{ /if }}" "test:1:1 Invalid if condition 'x == 'a''")
                    ("{{ if x === true }}{{ /if }}" "test:1:1 Invalid if condition 'x === true'")
                    ("{{ include 'x' }}" "test:1:1 Unexpected 'include' directive at runtime (includes must be compiled)")
                    ("{{ if x }}{{ else }}{{ bad-tag }}{{ /if }}" "test:1:21 Invalid tag 'bad-tag'")))
      (check (second case) (render-error (first case) (ax:object "x" ax:true)))
      (check (second case) (axllm::validate-prompt-template-syntax (first case) "test")))
    ;; UTF-16 columns, LF-only line breaks, CRLF, combining and astral text.
    (check "test:1:4 Missing template variable 'x'" (render-error "😀é{{ x }}" (ax:object)))
    (check "test:2:5 Invalid tag 'x-y'"
           (render-error (format nil "head~C~Cé😀{{ x-y }}" #\Return #\Newline) (ax:object)))
    (check "test:1:3 Unexpected 'else'"
           (render-error (format nil "a~C{{ else }}" (code-char #x2028)) (ax:object)))
    (check "test:1:4 Unclosed 'if' block" (render-error "😀 {{ if x }}" (ax:object)))
    (check "ok" (axllm::render-template-content
                 (format nil "{{~Cx~C}}" (code-char #xfeff) (code-char #xa0)) (ax:object "x" "ok")))
    (check "ok" (axllm::render-template-content
                 (format nil "{{ if x~C===~C'ok' }}ok{{ /if }}" (code-char #xfeff) (code-char #x2028))
                 (ax:object "x" "ok")))
    (let ((source (format nil "{{ if~Cx }}" #\Tab)))
      (check (format nil "test:1:1 Invalid tag 'if~Cx'" #\Tab) (render-error source (ax:object))))
    ;; Exact AST shape, JSON serializability, vector children, and UTF-16 index.
    (check (ax:parse-json "[{\"type\":\"text\",\"value\":\"😀\"},{\"type\":\"var\",\"name\":\"x\",\"index\":2}]")
           (axllm/core::core-template-parse "😀{{ x }}" "test"))
    (let* ((source "{{ if x }}{{ y }}{{ else }}no{{ /if }}")
           (tree (axllm/core::core-template-parse source "test")))
      (check (ax:parse-json "[{\"type\":\"if\",\"condition\":\"x\",\"then\":[{\"type\":\"var\",\"name\":\"y\",\"index\":10}],\"else\":[{\"type\":\"text\",\"value\":\"no\"}],\"index\":0}]") tree)
      (check tree (ax:parse-json (ax:encode-json tree)))
      (check "no" (axllm/core::core-template-render-tree tree (ax:object "x" ax:false) source "test"))
      (check #("x" "y") (axllm/core::core-template-collect-vars tree)))
    (let ((source "{{ z }}{{ if user.mode === 'fast' }}{{ z }}{{ A }}{{ else }}{{ if flag }}{{ _b }}{{ /if }}{{ /if }}{{ ! ignore }}"))
      (check #("A" "_b" "flag" "user.mode" "z") (axllm::collect-template-variable-names source))
      (check ax:true (axllm::validate-prompt-template-syntax source "test" #("_b" "flag" "user.mode")))
      (check "must preserve template variable {{user}}"
             (axllm::validate-prompt-template-syntax source "test" #("user" "other"))))
    (check #() (axllm::collect-template-variable-names "{{ ! x }} plain"))
    (check ax:true (axllm::validate-prompt-template-syntax ""))
    (check ax:true (axllm::validate-prompt-template-syntax "{{ x }}" "test" :null))
    (check "must preserve template variable {{x}}"
           (axllm::validate-prompt-template-syntax "{{ ! x }}" "test" #("x")))
    (check "template-validate:1:1 Unexpected 'else'" (axllm::validate-prompt-template-syntax "{{ else }}"))
    (check "inline-template:1:1 Missing template variable 'x'" (render-error "{{x}}" (ax:object) "inline-template"))
    (format t "Template boundaries: ~D assertions PASS~%" *checks*)
    t))
