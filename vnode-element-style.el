;;; vnode-element-style.el --- vnode 样式属性读取接口（精简视觉样式）  -*- lexical-binding: t; -*-

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

(defun vnode-element-style-margin (node side)
  "返回 NODE 的 margin-SIDE 值（整数）。"
  (vnode-element-style--side-value node :margin side))

(defun vnode-element-style-padding (node side)
  (vnode-element-style--side-value node :padding side))

(defun vnode-element-style-border-width (node side)
  "返回 NODE 的 border-SIDE 宽度（整数），默认 0。"
  (vnode-element-style--side-value node :border-width side))

;; ═══════════════════════════════════════════════════════════════
;; Flex 容器属性
;; ═══════════════════════════════════════════════════════════════

(defun vnode-element-style-flex-direction (node)
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
  (vnode-element-style--get node :flex-basis 'auto))

(defun vnode-element-style-align-self (node)
  (vnode-element-style--get node :align-self 'auto))

;; ═══════════════════════════════════════════════════════════════
;; 视觉属性：文本与字体
;; ═══════════════════════════════════════════════════════════════

(defun vnode-element-style-font-weight (node)
  "返回 :font-weight，默认 'normal。"
  (vnode-element-style--get node :font-weight 'normal))

(defun vnode-element-style-font-style (node)
  "返回 :font-style，默认 'normal。"
  (vnode-element-style--get node :font-style 'normal))

(defun vnode-element-style-font-size (node)
  "返回 :font-size，整数（1/10 pt）或 nil。"
  (vnode-element-style--get node :font-size nil))

(defun vnode-element-style-font-family (node)
  "返回 :font-family，字符串或 nil。"
  (vnode-element-style--get node :font-family nil))

(defun vnode-element-style-text-align (node)
  "返回 :text-align，默认 'left。"
  (vnode-element-style--get node :text-align 'left))

(defun vnode-element-style-text-decoration (node)
  "返回 :text-decoration，符号或列表，默认 nil。"
  (vnode-element-style--get node :text-decoration nil))

(defun vnode-element-style-line-height (node)
  "返回 :line-height，整数或 nil。"
  (vnode-element-style--get node :line-height nil))

(defun vnode-element-style-color (node)
  "返回 :color（前景色），字符串或 nil。"
  (vnode-element-style--get node :color nil))

(defun vnode-element-style-background (node)
  "返回 :background（背景色），字符串或 nil。"
  (vnode-element-style--get node :background nil))

;; ═══════════════════════════════════════════════════════════════
;; 视觉属性：边框
;; ═══════════════════════════════════════════════════════════════

(defun vnode-element-style-border-color (node)
  "返回 :border-color，字符串或 nil。"
  (vnode-element-style--get node :border-color nil))

(defun vnode-element-style-border-style (node)
  "返回 :border-style，符号（如 'solid）或 nil。"
  (vnode-element-style--get node :border-style nil))

(defun vnode-element-style-border-radius (node)
  "返回 :border-radius，整数或 nil。Emacs 通常不支持，仅保留接口。"
  (vnode-element-style--get node :border-radius nil))

;; ═══════════════════════════════════════════════════════════════
;; 通用视觉
;; ═══════════════════════════════════════════════════════════════

(defun vnode-element-style-visible (node)
  "返回 :visible，默认 t。"
  (vnode-element-style--get node :visible t))

(defun vnode-element-style-overflow (node)
  "返回 :overflow，默认 'visible。"
  (vnode-element-style--get node :overflow 'visible))

(defun vnode-element-style-cursor (node)
  "返回 :cursor，符号或 nil。Emacs 中作用有限。"
  (vnode-element-style--get node :cursor nil))

;; ═══════════════════════════════════════════════════════════════
;; 已有的 face 属性
;; ═══════════════════════════════════════════════════════════════

(defun vnode-element-style-face (node)
  "返回 :face 属性，可直接指定 Emacs face 符号或 plist。"
  (vnode-element-style--get node :face nil))

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
