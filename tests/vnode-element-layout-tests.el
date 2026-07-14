;;; vnode-element-layout-tests.el --- 测试布局引擎（Block 布局）  -*- lexical-binding: t; -*-

(require 'ert)
(require 'vnode-element)
(require 'vnode-element-style)
(require 'vnode-element-measure)
(require 'vnode-element-layout)

;; ═══════════════════════════════════════════════════════════════════
;; 辅助函数
;; ═══════════════════════════════════════════════════════════════════

(defun ltest--text-node (text)
  "创建一个内容为 TEXT 的 text 节点。"
  (vnode-element-desc-to-element `(text ,text)))

(defun ltest--layout-root (root &optional cols rows)
  "对 ROOT 进行布局，使用 COLS x ROWS 的可用空间（默认 80x24）。"
  (vnode-element-layout root (or cols 80) (or rows 24) 1 0))

(defun ltest--geom (node)
  "返回节点的几何信息 (start-row start-col end-row end-col)。"
  (list (vnode-element-start-row node)
        (vnode-element-start-col node)
        (vnode-element-end-row node)
        (vnode-element-end-col node)))

;; ═══════════════════════════════════════════════════════════════════
;; 基础文本节点布局（现在放在容器中）
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-layout-single-text-in-container ()
  "单个文本节点在容器中应占满可用宽度，高度为 1。"
  (let ((root (vnode-element-desc-to-element
               '(row (text "hello")))))
    (ltest--layout-root root 80 24)
    (let ((child (car (vnode-element-children root))))
      (should (equal (ltest--geom child) '(1 0 2 80))))))

(ert-deftest test-layout-fixed-width-text-in-container ()
  "指定宽度的文本节点，在容器中不应撑满可用空间。"
  (let ((root (vnode-element-desc-to-element
               '(row (text :width 10 "hello")))))
    (ltest--layout-root root 80 24)
    (let ((child (car (vnode-element-children root))))
      (should (equal (ltest--geom child) '(1 0 2 10))))))

;; ═══════════════════════════════════════════════════════════════════
;; Block 容器垂直堆叠
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-layout-block-two-texts ()
  "两个文本节点垂直排列，第二个在第一个下方。"
  (let ((root (vnode-element-desc-to-element
               '(row (text "a") (text "b")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom (car (vnode-element-children root))) '(1 0 2 80)))
    (should (equal (ltest--geom (cadr (vnode-element-children root))) '(2 0 3 80)))))

;; ═══════════════════════════════════════════════════════════════════
;; margin 偏移
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-layout-margin-offset ()
  "带有 margin 的子元素，位置应偏移。"
  (let ((root (vnode-element-desc-to-element
               '(row (text :margin 2 "x")))))
    (ltest--layout-root root 80 24)
    (let ((child (car (vnode-element-children root))))
      (should (equal (ltest--geom child) '(3 2 4 78)))))) ; margin-top=2, margin-left=2, 可用宽度80-4=76, 但文本宽度实际为1，所以右边界 2+1=3? 实际上 child-width 为 auto，会占满可用宽度 76，所以 end-col = 2+76=78

;; ═══════════════════════════════════════════════════════════════════
;; padding / border 缩小内容区域
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-layout-padding-shrinks-content ()
  "容器的 padding 会使子元素偏移并缩小可用宽度。"
  (let ((root (vnode-element-desc-to-element
               '(row :padding 1 (text "x")))))
    (ltest--layout-root root 80 24)
    (let ((child (car (vnode-element-children root))))
      ;; 容器从 (1,0) 到 (2,80)，内容区域从 (2,1) 到 (3,79)
      (should (equal (ltest--geom child) '(2 1 3 79))))))

;; ═══════════════════════════════════════════════════════════════════
;; 容器 auto 高度由内容撑开
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-layout-auto-height-by-content ()
  "未指定高度的容器，高度等于子元素总高度（含 margin、padding）。"
  (let ((root (vnode-element-desc-to-element
               '(row :padding 0 :margin 0
                     (text :margin 0 :height 3 "a")))))
    (ltest--layout-root root 80 24)
    ;; 容器高度 = 子元素高度(3) + margin(0) + padding(0) = 3
    (should (= (vnode-element-end-row root) 4)))) ; start-row=1, height=3 → end-row=4

;; ═══════════════════════════════════════════════════════════════════
;; display: none 尺寸归零
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-layout-none-display ()
  "display: none 的节点尺寸为 0，且不影响布局。"
  (let ((root (vnode-element-desc-to-element
               '(row :display none (text "x")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom root) '(1 0 1 0)))
    ;; resolved-style 宽高应为 0
    (should (= (plist-get (vnode-element-resolved-style root) :width) 0))
    (should (= (plist-get (vnode-element-resolved-style root) :height) 0))))

;; ═══════════════════════════════════════════════════════════════════
;; 相对定位偏移
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-layout-relative-offset ()
  "relative 定位的元素在正常流位置基础上偏移。"
  (let ((root (vnode-element-desc-to-element
               '(row (text :position relative :left 3 :top 1 "x")))))
    (ltest--layout-root root 80 24)
    (let ((child (car (vnode-element-children root))))
      ;; 正常流位置为 (1,0)→(2,80)，偏移后为 (2,3)→(3,83)
      (should (equal (ltest--geom child) '(2 3 3 83))))))

;; ═══════════════════════════════════════════════════════════════════
;; 嵌套容器
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-layout-nested-block ()
  "嵌套容器应递归布局。"
  (let ((root (vnode-element-desc-to-element
               '(row (row :padding 1 (text "a"))))))
    (ltest--layout-root root 80 24)
    (let ((outer (car (vnode-element-children root))))
      ;; 内层容器有 padding 1，高度 = 1(文本) + 1(pad-top) + 1(pad-bottom) = 3，外层高度 auto 所以也是 3
      (should (equal (ltest--geom outer) '(1 0 4 80)))
      (let ((inner (car (vnode-element-children outer))))
        (should (equal (ltest--geom inner) '(2 1 3 79))))))) ; 内层文本受 padding 影响

;; ═══════════════════════════════════════════════════════════════════
;; resolved-style 被正确设置
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-layout-resolved-style-set ()
  "布局后 resolved-style 应包含计算出的宽高。"
  (let ((root (vnode-element-desc-to-element
               '(row :width 30 :height 10))))
    (ltest--layout-root root 80 24)
    (should (= (plist-get (vnode-element-resolved-style root) :width) 30))
    (should (= (plist-get (vnode-element-resolved-style root) :height) 10))))

(provide 'vnode-element-layout-tests)
;;; vnode-element-layout-tests.el ends here
