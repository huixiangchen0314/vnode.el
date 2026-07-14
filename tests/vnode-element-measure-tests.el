;;; vnode-element-measure-tests.el --- 测试尺寸测量模块  -*- lexical-binding: t; -*-

(require 'ert)
(require 'vnode-element)
(require 'vnode-element-style)
(require 'vnode-element-measure)

;; ═══════════════════════════════════════════════════════════════════
;; 辅助：创建测试节点
;; ═══════════════════════════════════════════════════════════════════

(defun mtest--text-node (text)
  "创建一个内容为 TEXT 的 text 节点。"
  (vnode-element-desc-to-element `(text ,text)))

(defun mtest--button-node (label)
  "创建一个 label 为 LABEL 的 button 节点。"
  (vnode-element-desc-to-element `(button :label ,label)))

(defun mtest--row-container (&optional children)
  "创建一个 row 容器（默认 display: block，不会自动变 flex）。"
  (apply #'vnode-element-create-element 'row :children children nil))

;; ═══════════════════════════════════════════════════════════════════
;; 基础判断测试
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-auto-p ()
  (should (vnode-element--auto-p nil))
  (should (vnode-element--auto-p 'auto))
  (should-not (vnode-element--auto-p 0))
  (should-not (vnode-element--auto-p 5)))

(ert-deftest test-resolved-p ()
  (should (vnode-element--resolved-p 0))
  (should (vnode-element--resolved-p 10))
  (should-not (vnode-element--resolved-p -1))
  (should-not (vnode-element--resolved-p nil))
  (should-not (vnode-element--resolved-p 'auto)))

;; ═══════════════════════════════════════════════════════════════════
;; 文本节点测量
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-text-node-width ()
  (let ((node (mtest--text-node "hello")))
    ;; 文本自然宽度为 5（"hello"）
    (should (= (vnode-element-get-pref-width node 10) 5))))

(ert-deftest test-text-node-multiline-width ()
  (let ((node (mtest--text-node "ab\ncde")))
    ;; 最宽一行是 "cde"，宽度 3
    (should (= (vnode-element-get-pref-width node 10) 3))))

(ert-deftest test-text-node-height ()
  (let ((node (mtest--text-node "hello")))
    ;; 单行高度为 1
    (should (= (vnode-element-get-pref-height node 10) 1))))

(ert-deftest test-text-node-multiline-height ()
  (let ((node (mtest--text-node "a\nb\nc")))
    ;; 三行高度为 3
    (should (= (vnode-element-get-pref-height node 10) 3))))

;; ═══════════════════════════════════════════════════════════════════
;; 按钮节点测量
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-button-width ()
  (let ((node (mtest--button-node "OK")))
    ;; "OK" 长度 2，两边空格共 +2，所以宽度 4
    (should (= (vnode-element-get-pref-width node 10) 4))))

(ert-deftest test-button-height ()
  (let ((node (mtest--button-node "X")))
    (should (= (vnode-element-get-pref-height node 10) 1))))

;; ═══════════════════════════════════════════════════════════════════
;; 固定尺寸优先
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-fixed-width ()
  (let ((node (vnode-element-create-element 'row :props '(:width 20))))
    (should (= (vnode-element-get-pref-width node 10) 20))))

(ert-deftest test-fixed-height ()
  (let ((node (vnode-element-create-element 'row :props '(:height 5))))
    (should (= (vnode-element-get-pref-height node 10) 5))))

;; ═══════════════════════════════════════════════════════════════════
;; 容器测量：非 Flex 垂直堆叠（默认 block）
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-block-container-width-max-of-children ()
  "非 Flex 容器（垂直堆叠）宽度取子元素最大值。"
  (let ((child1 (mtest--text-node "ab"))
        (child2 (mtest--text-node "cde")))
    (let ((container (vnode-element-create-element 'row :children (list child1 child2))))
      ;; 宽度 = max(2, 3) + padding/border/margin = 3（默认无 padding 等）
      (should (= (vnode-element-get-pref-width container 10) 3)))))

(ert-deftest test-block-container-height-sum-of-children ()
  "非 Flex 容器（垂直堆叠）高度累加子元素。"
  (let ((child1 (mtest--text-node "a"))   ; height=1
        (child2 (mtest--text-node "b\nc"))) ; height=2
    (let ((container (vnode-element-create-element 'row :children (list child1 child2))))
      ;; 高度 = 1 + 2 = 3
      (should (= (vnode-element-get-pref-height container 10) 3)))))

;; ═══════════════════════════════════════════════════════════════════
;; Flex 容器测量：row 方向
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-flex-row-container-width-sum ()
  "Flex row 方向宽度累加子元素。"
  (let ((child1 (mtest--text-node "ab"))   ; width=2
        (child2 (mtest--text-node "cde"))) ; width=3
    (let ((container (vnode-element-desc-to-element
                      `(row :display flex :flex-direction row
                            (text "ab") (text "cde")))))
      ;; 宽度 = 2 + 3 = 5 (无 gap)
      (should (= (vnode-element-get-pref-width container 10) 5)))))

(ert-deftest test-flex-row-container-height-max ()
  "Flex row 方向高度取子元素最大。"
  (let ((container (vnode-element-desc-to-element
                    `(row :display flex :flex-direction row
                          (text "a")        ; height=1
                          (text "b\nc"))))) ; height=2
    (should (= (vnode-element-get-pref-height container 10) 2))))

;; ═══════════════════════════════════════════════════════════════════
;; Flex 容器测量：col 方向
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-flex-col-container-height-sum ()
  "Flex col 方向高度累加子元素。"
  (let ((container (vnode-element-desc-to-element
                    `(col :display flex :flex-direction col
                          (text "a") (text "b")))))
    (should (= (vnode-element-get-pref-height container 10) 2))))

(ert-deftest test-flex-col-container-width-max ()
  "Flex col 方向宽度取子元素最大。"
  (let ((container (vnode-element-desc-to-element
                    `(col :display flex :flex-direction col
                          (text "ab") (text "cde")))))
    (should (= (vnode-element-get-pref-width container 10) 3))))

;; ═══════════════════════════════════════════════════════════════════
;; gap 的影响
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-flex-row-gap-increases-width ()
  (let ((container (vnode-element-desc-to-element
                    `(row :display flex :flex-direction row :gap 1
                          (text "a") (text "b")))))
    ;; 宽度 = 1 + gap(1) + 1 = 3
    (should (= (vnode-element-get-pref-width container 10) 3))))

(ert-deftest test-flex-col-gap-increases-height ()
  (let ((container (vnode-element-desc-to-element
                    `(col :display flex :flex-direction col :gap 2
                          (text "a") (text "b")))))
    ;; 高度 = 1 + gap(2) + 1 = 4
    (should (= (vnode-element-get-pref-height container 10) 4))))

;; ═══════════════════════════════════════════════════════════════════
;; margin 的影响
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-margin-increases-container-height ()
  (let ((child (vnode-element-desc-to-element
                `(text :margin 1 "a"))))  ; margin 四边为1
    (let ((container (vnode-element-create-element 'row :children (list child))))
      ;; 子元素高度 1 + margin-top(1) + margin-bottom(1) = 3
      (should (= (vnode-element-get-pref-height container 10) 3)))))

;; ═══════════════════════════════════════════════════════════════════
;; resolved-style 写入与隔离
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-resolved-style-stored ()
  "测量后 resolved-style 应包含计算出的尺寸。"
  (let ((node (mtest--text-node "hello")))
    (vnode-element-get-pref-width node 10)
    (should (vnode-element--resolved-p
             (plist-get (vnode-element-resolved-style node) :width)))
    (should (= (plist-get (vnode-element-resolved-style node) :width) 5))))

(ert-deftest test-resolved-style-isolation ()
  "子节点的 resolved-style 不会影响父节点。"
  (let* ((child (mtest--text-node "abc"))
         (container (vnode-element-create-element 'row :children (list child))))
    ;; 触发子节点测量
    (vnode-element-get-pref-width child 10)
    ;; 容器的 resolved-style 应还未设置（或为 nil）
    (should-not (vnode-element--resolved-p
                 (plist-get (vnode-element-resolved-style container) :width)))))

(ert-deftest test-resolved-style-does-not-overwrite-props ()
  "resolved-style 写入不应影响 props 中的原始值。"
  (let ((node (vnode-element-create-element 'row :props '(:width auto))))
    (vnode-element-get-pref-width node 10)
    (should (eq (plist-get (vnode-element-props node) :width) 'auto))))

;; ═══════════════════════════════════════════════════════════════════
;; GetChildWidth/Height 使用 resolved-style
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-get-child-width-uses-resolved ()
  "测量子元素后，再次获取应直接返回缓存值。"
  (let ((child (mtest--text-node "hi")))
    ;; 先测量一次
    (vnode-element-get-pref-width child 10)
    ;; 然后调用 get-child-width，应该直接返回缓存
    (should (= (vnode-element-get-child-width child 10) 2))))

;; ═══════════════════════════════════════════════════════════════════
;; 无子节点容器默认尺寸
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-empty-container-width-zero ()
  "无子节点的容器，宽度为 0（仅 padding/border 默认为 0）。"
  (let ((container (vnode-element-create-element 'row)))
    (should (= (vnode-element-get-pref-width container 10) 0))))

(ert-deftest test-empty-container-height-zero-or-one ()
  "无子节点的容器，高度可能为 0 或取决于 min-height，默认 0。"
  (let ((container (vnode-element-create-element 'row)))
    (should (= (vnode-element-get-pref-height container 10) 0))))

(provide 'vnode-element-measure-tests)
;;; vnode-element-measure-tests.el ends here
