;;; vnode-element-style-tests.el --- 测试样式读取接口  -*- lexical-binding: t; -*-

(require 'ert)
(require 'vnode-element)
(require 'vnode-element-style)

;; ── 辅助函数 ──────────────────────────────────────────
(defun make-test-node (&rest props)
  "创建一个用于测试的 vnode-element，type 为 'row，props 由 PROPS 指定。"
  (vnode-element-create-element 'row :props props))

;; ═══════════════════════════════════════════════════════
;; 显示与定位
;; ═══════════════════════════════════════════════════════
(ert-deftest test-style-display-default ()
  (let ((node (make-test-node)))
    (should (eq (vnode-element-style-display node) 'block))))

(ert-deftest test-style-display-explicit ()
  (let ((node (make-test-node :display 'flex)))
    (should (eq (vnode-element-style-display node) 'flex))))

(ert-deftest test-style-position-default ()
  (let ((node (make-test-node)))
    (should (eq (vnode-element-style-position node) 'static))))

(ert-deftest test-style-position-absolute ()
  (let ((node (make-test-node :position 'absolute)))
    (should (eq (vnode-element-style-position node) 'absolute))))

(ert-deftest test-style-left-right-top-bottom ()
  (let ((node (make-test-node :left 1 :right 2 :top 3 :bottom 4)))
    (should (= (vnode-element-style-left node) 1))
    (should (= (vnode-element-style-right node) 2))
    (should (= (vnode-element-style-top node) 3))
    (should (= (vnode-element-style-bottom node) 4))))

(ert-deftest test-style-offset-defaults ()
  (let ((node (make-test-node)))
    (should-not (vnode-element-style-left node))
    (should-not (vnode-element-style-right node))
    (should-not (vnode-element-style-top node))
    (should-not (vnode-element-style-bottom node))))

;; ═══════════════════════════════════════════════════════
;; 盒模型尺寸
;; ═══════════════════════════════════════════════════════
(ert-deftest test-style-width-auto-default ()
  (let ((node (make-test-node)))
    (should (eq (vnode-element-style-width node) 'auto))))

(ert-deftest test-style-width-explicit ()
  (let ((node (make-test-node :width 30)))
    (should (= (vnode-element-style-width node) 30))))

(ert-deftest test-style-height-auto-default ()
  (let ((node (make-test-node)))
    (should (eq (vnode-element-style-height node) 'auto))))

(ert-deftest test-style-min-max-width ()
  (let ((node (make-test-node :min-width 5 :max-width 100)))
    (should (= (vnode-element-style-min-width node) 5))
    (should (= (vnode-element-style-max-width node) 100))))

(ert-deftest test-style-min-max-height ()
  (let ((node (make-test-node :min-height 2 :max-height 50)))
    (should (= (vnode-element-style-min-height node) 2))
    (should (= (vnode-element-style-max-height node) 50))))

(ert-deftest test-style-min-max-defaults ()
  (let ((node (make-test-node)))
    (should-not (vnode-element-style-min-width node))
    (should-not (vnode-element-style-max-width node))
    (should-not (vnode-element-style-min-height node))
    (should-not (vnode-element-style-max-height node))))

;; ═══════════════════════════════════════════════════════
;; margin / padding / border（简写和四边独立）
;; ═══════════════════════════════════════════════════════
(ert-deftest test-style-margin-uniform ()
  "margin 设为整数时四边相同。"
  (let ((node (make-test-node :margin 2)))
    (should (= (vnode-element-style-margin node :top) 2))
    (should (= (vnode-element-style-margin node :right) 2))
    (should (= (vnode-element-style-margin node :bottom) 2))
    (should (= (vnode-element-style-margin node :left) 2))))

(ert-deftest test-style-margin-plist ()
  "margin 设为 plist 时各边独立。"
  (let ((node (make-test-node :margin '(:top 1 :right 2 :bottom 3 :left 4))))
    (should (= (vnode-element-style-margin node :top) 1))
    (should (= (vnode-element-style-margin node :right) 2))
    (should (= (vnode-element-style-margin node :bottom) 3))
    (should (= (vnode-element-style-margin node :left) 4))))

(ert-deftest test-style-margin-default ()
  "不设 margin 时默认 0。"
  (let ((node (make-test-node)))
    (should (= (vnode-element-style-margin node :top) 0))
    (should (= (vnode-element-style-margin node :left) 0))))

(ert-deftest test-style-padding-uniform ()
  (let ((node (make-test-node :padding 1)))
    (should (= (vnode-element-style-padding node :top) 1))
    (should (= (vnode-element-style-padding node :bottom) 1))))

(ert-deftest test-style-padding-plist ()
  (let ((node (make-test-node :padding '(:top 0 :right 2 :bottom 0 :left 2))))
    (should (= (vnode-element-style-padding node :right) 2))
    (should (= (vnode-element-style-padding node :left) 2))))

(ert-deftest test-style-border-uniform ()
  (let ((node (make-test-node :border-width 1)))
    (should (= (vnode-element-style-border-width node :top) 1))
    (should (= (vnode-element-style-border-width node :bottom) 1))))

(ert-deftest test-style-border-plist ()
  (let ((node (make-test-node :border-width '(:top 2 :right 0 :bottom 2 :left 0))))
    (should (= (vnode-element-style-border-width node :top) 2))
    (should (= (vnode-element-style-border-width node :right) 0))))

(ert-deftest test-style-border-default ()
  (let ((node (make-test-node)))
    (should (= (vnode-element-style-border-width node :left) 0))))

;; ═══════════════════════════════════════════════════════
;; Flex 容器属性
;; ═══════════════════════════════════════════════════════
(ert-deftest test-style-flex-direction-explicit ()
  (let ((node (make-test-node :flex-direction 'col)))
    (should (eq (vnode-element-style-flex-direction node) 'col))))

(ert-deftest test-style-flex-direction-auto-from-row ()
  "row 类型默认主轴为 row。"
  (let ((node (vnode-element-create-element 'row)))
    (should (eq (vnode-element-style-flex-direction node) 'row))))

(ert-deftest test-style-flex-direction-auto-from-col ()
  "col 类型默认主轴为 col。"
  (let ((node (vnode-element-create-element 'col)))
    (should (eq (vnode-element-style-flex-direction node) 'col))))

(ert-deftest test-style-flex-direction-auto-other ()
  "其他类型默认 row。"
  (let ((node (vnode-element-create-element 'text)))
    (should (eq (vnode-element-style-flex-direction node) 'row))))

(ert-deftest test-style-justify-content-default ()
  (let ((node (make-test-node)))
    (should (eq (vnode-element-style-justify-content node) 'start))))

(ert-deftest test-style-justify-content-center ()
  (let ((node (make-test-node :justify-content 'center)))
    (should (eq (vnode-element-style-justify-content node) 'center))))

(ert-deftest test-style-align-items-default ()
  (let ((node (make-test-node)))
    (should (eq (vnode-element-style-align-items node) 'stretch))))

(ert-deftest test-style-align-items-end ()
  (let ((node (make-test-node :align-items 'end)))
    (should (eq (vnode-element-style-align-items node) 'end))))

(ert-deftest test-style-align-content-default ()
  (let ((node (make-test-node)))
    (should (eq (vnode-element-style-align-content node) 'stretch))))

(ert-deftest test-style-flex-wrap-default ()
  (let ((node (make-test-node)))
    (should (eq (vnode-element-style-flex-wrap node) 'nowrap))))

(ert-deftest test-style-flex-wrap-wrap ()
  (let ((node (make-test-node :flex-wrap 'wrap)))
    (should (eq (vnode-element-style-flex-wrap node) 'wrap))))

(ert-deftest test-style-gap-default ()
  (let ((node (make-test-node)))
    (should (= (vnode-element-style-gap node) 0))))

(ert-deftest test-style-gap-3 ()
  (let ((node (make-test-node :gap 3)))
    (should (= (vnode-element-style-gap node) 3))))

;; ═══════════════════════════════════════════════════════
;; Flex 子项属性
;; ═══════════════════════════════════════════════════════
(ert-deftest test-style-flex-grow-default ()
  (let ((node (make-test-node)))
    (should (= (vnode-element-style-flex-grow node) 0))))

(ert-deftest test-style-flex-grow-2 ()
  (let ((node (make-test-node :flex-grow 2)))
    (should (= (vnode-element-style-flex-grow node) 2))))

(ert-deftest test-style-flex-shrink-default ()
  (let ((node (make-test-node)))
    (should (= (vnode-element-style-flex-shrink node) 1))))

(ert-deftest test-style-flex-basis-auto-default ()
  (let ((node (make-test-node)))
    (should (eq (vnode-element-style-flex-basis node) 'auto))))

(ert-deftest test-style-flex-basis-numeric ()
  (let ((node (make-test-node :flex-basis 10)))
    (should (= (vnode-element-style-flex-basis node) 10))))

(ert-deftest test-style-align-self-default ()
  (let ((node (make-test-node)))
    (should (eq (vnode-element-style-align-self node) 'auto))))

(ert-deftest test-style-align-self-center ()
  (let ((node (make-test-node :align-self 'center)))
    (should (eq (vnode-element-style-align-self node) 'center))))

;; ═══════════════════════════════════════════════════════
;; 视觉属性
;; ═══════════════════════════════════════════════════════
(ert-deftest test-style-face ()
  (let ((node (make-test-node :face 'bold)))
    (should (eq (vnode-element-style-face node) 'bold))))

(ert-deftest test-style-face-default ()
  (let ((node (make-test-node)))
    (should-not (vnode-element-style-face node))))

(ert-deftest test-style-background ()
  (let ((node (make-test-node :background "#333")))
    (should (string= (vnode-element-style-background node) "#333"))))

(ert-deftest test-style-color ()
  (let ((node (make-test-node :color "white")))
    (should (string= (vnode-element-style-color node) "white"))))

(ert-deftest test-style-visible-default ()
  (let ((node (make-test-node)))
    (should (vnode-element-style-visible node))))

(ert-deftest test-style-visible-false ()
  (let ((node (make-test-node :visible nil)))
    (should-not (vnode-element-style-visible node))))

;; ═══════════════════════════════════════════════════════
;; 便捷查询
;; ═══════════════════════════════════════════════════════
(ert-deftest test-style-is-auto-width ()
  (let ((auto-node (make-test-node))
        (fixed-node (make-test-node :width 40)))
    (should (vnode-element-style-is-auto-width-p auto-node))
    (should-not (vnode-element-style-is-auto-width-p fixed-node))))

(ert-deftest test-style-is-auto-height ()
  (let ((auto-node (make-test-node))
        (fixed-node (make-test-node :height 10)))
    (should (vnode-element-style-is-auto-height-p auto-node))
    (should-not (vnode-element-style-is-auto-height-p fixed-node))))

(ert-deftest test-style-is-flex-container ()
  (let ((flex-node (make-test-node :display 'flex))
        (block-node (make-test-node)))
    (should (vnode-element-style-is-flex-container-p flex-node))
    (should-not (vnode-element-style-is-flex-container-p block-node))))

(provide 'vnode-element-style-tests)
;;; vnode-element-style-tests.el ends here
