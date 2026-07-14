;;; vnode-element-tests.el --- 测试 vnode-element 树解析（对齐 Clojure）  -*- lexical-binding: t; -*-

(require 'ert)
(require 'vnode-element)

;; ── 辅助 ──────────────────────────────────────────────
(defmacro should-vnode (node type &rest body)
  "Assert that NODE is a vnode-element of TYPE, then evaluate BODY."
  (declare (indent 2))
  `(progn
     (should (vnode-element-p ,node))
     (should (eq (vnode-element-type ,node) ,type))
     ,@body))

(defun vnode-prop-value (node prop)
  (plist-get (vnode-element-props node) prop))

;; ── 字符串转换 ─────────────────────────────────────────
(ert-deftest test-string-returns-string ()
  "字符串应原样返回，不包装成节点。"
  (let ((result (vnode-element-desc-to-element "hello")))
    (should (stringp result))
    (should (string= result "hello"))))

(ert-deftest test-multiline-string-returns-string ()
  "多行字符串也直接返回字符串。"
  (let ((result (vnode-element-desc-to-element "line1\nline2")))
    (should (stringp result))
    (should (string= result "line1\nline2"))))

;; ── 基础标签 ──────────────────────────────────────────
(ert-deftest test-simple-row ()
  (let ((node (vnode-element-desc-to-element
               '(row (text "a") (text "b")))))
    (should-vnode node 'row
      (should (= (length (vnode-element-children node)) 2))
      (should-vnode (nth 0 (vnode-element-children node)) 'text)
      (should-vnode (nth 1 (vnode-element-children node)) 'text))))

(ert-deftest test-simple-col ()
  (let ((node (vnode-element-desc-to-element
               '(col (text :value "x")))))
    (should-vnode node 'col
      (should (= (length (vnode-element-children node)) 1)))))

(ert-deftest test-button-with-event ()
  "按钮使用 :label 和 :on-click 属性。"
  (let ((node (vnode-element-desc-to-element
               '(button :label "Click" :on-click :my-event))))
    (should-vnode node 'button
      (should (string= (vnode-prop-value node :label) "Click"))
      (should (eq (vnode-prop-value node :on-click) :my-event)))))

;; ── 属性解析细节 ──────────────────────────────────────
(ert-deftest test-key-removed-from-props ()
  (let ((node (vnode-element-desc-to-element
               '(row :key "my-row" (text "a")))))
    (should-vnode node 'row
      (should (equal (vnode-element-key node) "my-row"))
      (should-not (plist-member (vnode-element-props node) :key)))))

(ert-deftest test-id-automatic ()
  (let ((node (vnode-element-desc-to-element '(text "no-id"))))
    (should (vnode-element-id node))
    (should (symbolp (vnode-element-id node)))))

(ert-deftest test-id-explicit ()
  (let ((node (vnode-element-desc-to-element
               '(text :id :my-text "test"))))
    (should (eq (vnode-element-id node) :my-text))))

;; ── 匿名组件 / 片段 ───────────────────────────────────
(ert-deftest test-anonymous-fragment-returns-list ()
  "当标签为列表时，返回节点列表。"
  (let ((result (vnode-element-desc-to-element
                 '((row (text "a")) (text "b")))))
    (should (listp result))
    (should (= (length result) 2))
    (should-vnode (car result) 'row
      ;; row 的子节点应为一个 text 节点，children 内是 "a"
      (let ((row-children (vnode-element-children (car result))))
        (should (= (length row-children) 1))
        (should-vnode (car row-children) 'text
          (should (equal (vnode-element-children (car row-children))
                         '("a"))))))
    ;; 第二个元素是 text 节点，其 children 应是 ("b")
    (should-vnode (cadr result) 'text
      (should (equal (vnode-element-children (cadr result))
                     '("b"))))))

(ert-deftest test-anonymous-fragment-flatten-in-children ()
  "匿名片段作为子节点时，内部元素被展平到父节点的 children 中。"
  (let ((node (vnode-element-desc-to-element
               '(col ((row (text "a")) (text "b"))))))
    (should-vnode node 'col
      (let ((children (vnode-element-children node)))
        (should (= (length children) 2))
        (should-vnode (nth 0 children) 'row)
        (should-vnode (nth 1 children) 'text)))))

(ert-deftest test-nested-anonymous-fragments ()
  "嵌套匿名片段正确展平。"
  (let ((node (vnode-element-desc-to-element
               '(col ((row ((text "a") (text "b"))) (text "c"))))))
    (should-vnode node 'col
      (let ((children (vnode-element-children node)))
        ;; col 的 children 应为 row 和 text "c"，共 2 个
        (should (= (length children) 2))
        (should-vnode (car children) 'row
          ;; row 的子节点是匿名片段 ((text "a") (text "b")) 展开后的两个 text 节点
          (should (equal (length (vnode-element-children (car children))) 2)))
        (should-vnode (cadr children) 'text)))))

;; ── 数字 / 符号 ───────────────────────────────────────
(ert-deftest test-number-returns-string ()
  "数字被转换为字符串返回。"
  (let ((result (vnode-element-desc-to-element 42)))
    (should (stringp result))
    (should (string= result "42"))))

(ert-deftest test-symbol-returns-string ()
  "符号也被转换为字符串返回。"
  (let ((result (vnode-element-desc-to-element 'foo)))
    (should (stringp result))
    (should (string= result "foo"))))

;; ── vnode-element-create-element 基础测试 ─────────────
(ert-deftest test-create-element-with-strings ()
  "字符串子节点保持原样。"
  (let ((node (vnode-element-create-element 'row :children '("a" "b"))))
    (should-vnode node 'row
      (let ((children (vnode-element-children node)))
        (should (equal children '("a" "b")))))))

(ert-deftest test-create-element-explicit-props ()
  (let ((node (vnode-element-create-element 'button :id :btn
                                            :props '(:label "OK"))))
    (should-vnode node 'button
      (should (eq (vnode-element-id node) :btn))
      (should (string= (vnode-prop-value node :label) "OK")))))

(provide 'vnode-element-tests)
;;; vnode-element-tests.el ends here
