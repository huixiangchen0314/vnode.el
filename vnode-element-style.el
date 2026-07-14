;;; vnode-element-style.el — vnode 样式属性读取接口  -*- lexical-binding: t; -*-

(require 'vnode-element)

;; ═══════════════════════════════════════════════════════════════
;; 内部辅助：从 props 安全取值
;; ═══════════════════════════════════════════════════════════════

(defun vnode-element-style--get (node prop &optional default)
  "从 NODE 的 props plist 中读取 PROP，若无则返回 DEFAULT。"
  (let ((p (vnode-element-props node)))
    (if (plist-member p prop)
        (plist-get p prop)
      default)))

(defun vnode-element-style--side-value (node property side &optional default)
  "获取 NODE 的复合属性 PROPERTY 在 SIDE 方向的值。
PROPERTY 可以是整数（四边相同）或 plist `(:top 1 :right 2 ...)`。
SIDE 为 :top, :right, :bottom, :left 之一。"
  (let ((val (vnode-element-style--get node property 0)))
    (if (plistp val)
        (or (plist-get val side) 0)
      val)))

;; ═══════════════════════════════════════════════════════════════
;; 显示与定位
;; ═══════════════════════════════════════════════════════════════

(defun vnode-element-style-display (node)
  "返回 NODE 的 :display 属性，默认 'block。"
  (vnode-element-style--get node :display 'block))

(defun vnode-element-style-position (node)
  "返回 NODE 的 :position 属性，默认 'static。"
  (vnode-element-style--get node :position 'static))

(defun vnode-element-style-left (node)
  "返回 NODE 的 :left 属性，默认 nil。"
  (vnode-element-style--get node :left nil))

(defun vnode-element-style-right (node)
  (vnode-element-style--get node :right nil))

(defun vnode-element-style-top (node)
  (vnode-element-style--get node :top nil))

(defun vnode-element-style-bottom (node)
  (vnode-element-style--get node :bottom nil))

;; ═══════════════════════════════════════════════════════════════
;; 盒模型尺寸（单位：字符列 / 行）
;; ═══════════════════════════════════════════════════════════════

(defun vnode-element-style-width (node)
  "返回 :width，可为整数或 'auto。"
  (vnode-element-style--get node :width 'auto))

(defun vnode-element-style-height (node)
  (vnode-element-style--get node :height 'auto))

(defun vnode-element-style-min-width (node)
  (vnode-element-style--get node :min-width nil))

(defun vnode-element-style-max-width (node)
  (vnode-element-style--get node :max-width nil))

(defun vnode-element-style-min-height (node)
  (vnode-element-style--get node :min-height nil))

(defun vnode-element-style-max-height (node)
  (vnode-element-style--get node :max-height nil))

;; 简写 margin / padding（支持整数或 plist）

(defun vnode-element-style-margin (node side)
  "返回 NODE 的 margin-SIDE 值（整数）。"
  (vnode-element-style--side-value node :margin side))

(defun vnode-element-style-padding (node side)
  (vnode-element-style--side-value node :padding side))

(defun vnode-element-style-border (node side)
  "返回 NODE 的 border-SIDE 宽度（整数），默认 0。"
  (vnode-element-style--side-value node :border side))

;; ═══════════════════════════════════════════════════════════════
;; Flex 容器属性
;; ═══════════════════════════════════════════════════════════════

(defun vnode-element-style-flex-direction (node)
  "返回 :flex-direction，对于 'row 标签默认为 'row，'col 标签默认为 'col。"
  (or (vnode-element-style--get node :flex-direction nil)
      (let ((type (vnode-element-type node)))
        (cond ((eq type 'row) 'row)
              ((eq type 'col) 'col)
              (t 'row)))))

(defun vnode-element-style-justify-content (node)
  (vnode-element-style--get node :justify-content 'start))

(defun vnode-element-style-align-items (node)
  (vnode-element-style--get node :align-items 'stretch))

(defun vnode-element-style-align-content (node)
  (vnode-element-style--get node :align-content 'stretch))

(defun vnode-element-style-flex-wrap (node)
  (vnode-element-style--get node :flex-wrap 'nowrap))

(defun vnode-element-style-gap (node)
  "返回 :gap 值（整数），默认 0。"
  (vnode-element-style--get node :gap 0))

;; ═══════════════════════════════════════════════════════════════
;; Flex 子项属性
;; ═══════════════════════════════════════════════════════════════

(defun vnode-element-style-flex-grow (node)
  (vnode-element-style--get node :flex-grow 0))

(defun vnode-element-style-flex-shrink (node)
  (vnode-element-style--get node :flex-shrink 1))

(defun vnode-element-style-flex-basis (node)
  "返回 :flex-basis，整数或 'auto，默认 'auto。"
  (vnode-element-style--get node :flex-basis 'auto))

(defun vnode-element-style-align-self (node)
  (vnode-element-style--get node :align-self 'auto))

;; ═══════════════════════════════════════════════════════════════
;; 视觉属性（不影响布局，供渲染器使用）
;; ═══════════════════════════════════════════════════════════════

(defun vnode-element-style-face (node)
  "返回 :face 属性。"
  (vnode-element-style--get node :face nil))

(defun vnode-element-style-background (node)
  (vnode-element-style--get node :background nil))

(defun vnode-element-style-color (node)
  (vnode-element-style--get node :color nil))

(defun vnode-element-style-visible (node)
  (vnode-element-style--get node :visible t))

;; ═══════════════════════════════════════════════════════════════
;; 便捷查询
;; ═══════════════════════════════════════════════════════════════

(defun vnode-element-style-is-auto-width-p (node)
  (eq (vnode-element-style-width node) 'auto))

(defun vnode-element-style-is-auto-height-p (node)
  (eq (vnode-element-style-height node) 'auto))

(defun vnode-element-style-is-flex-container-p (node)
  (eq (vnode-element-style-display node) 'flex))

(provide 'vnode-element-style)
;;; vnode-element-style.el ends here
