;;; vnode-element-measure.el --- 尺寸测量，对齐 C# LayoutUtils / Element 行为  -*- lexical-binding: t; -*-

(require 'vnode-element)
(require 'vnode-element-style)
(require 'cl-lib)

;; ═══════════════════════════════════════════════════════════════════
;; 内部：读取“有效值”（resolved-style 优先，否则 props）
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element--effective-style (node prop)
  "返回 NODE 的样式属性 PROP 的有效值。
先查 resolved-style，若不存在则查 props。"
  (let ((resolved (vnode-element-resolved-style node)))
    (if (and resolved (plist-member resolved prop))
        (plist-get resolved prop)
      ;; 回退到原始 props
      (plist-get (vnode-element-props node) prop))))

(defun vnode-element--set-resolved! (node prop value)
  "将 PROP 和 VALUE 写入 NODE 的 resolved-style 中（副作用）。"
  (let ((current (vnode-element-resolved-style node)))
    (setf (vnode-element-resolved-style node)
          (plist-put (or current '()) prop value))))

;; ═══════════════════════════════════════════════════════════════════
;; 基础判断：Auto / Resolved
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element--auto-p (value)
  "判断 value 是否为 auto（即 nil 或符号 \\='auto）。"
  (or (null value) (eq value 'auto)))

(defun vnode-element--resolved-p (value)
  "判断 value 是否已解析为确定数值（非 nil、非 \\='auto、整数且 >= 0）。"
  (and (integerp value) (>= value 0)))

;; ═══════════════════════════════════════════════════════════════════
;; 常用 IsAuto / IsResolved 检查（参照 LayoutUtils）
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-is-auto-width-p (node)
  "检查宽度是否为 auto。优先查 resolved-style。"
  (let ((v (vnode-element--effective-style node :width)))
    (vnode-element--auto-p v)))

(defun vnode-element-is-auto-height-p (node)
  (let ((v (vnode-element--effective-style node :height)))
    (vnode-element--auto-p v)))

(defun vnode-element-is-auto-min-width-p (node)
  (vnode-element--auto-p (vnode-element-style-min-width node)))

(defun vnode-element-is-auto-max-width-p (node)
  (vnode-element--auto-p (vnode-element-style-max-width node)))

(defun vnode-element-is-auto-min-height-p (node)
  (vnode-element--auto-p (vnode-element-style-min-height node)))

(defun vnode-element-is-auto-max-height-p (node)
  (vnode-element--auto-p (vnode-element-style-max-height node)))

(defun vnode-element-is-auto-margin-top-p (node)
  (vnode-element--auto-p (vnode-element-style-margin node :top)))
(defun vnode-element-is-auto-margin-right-p (node)
  (vnode-element--auto-p (vnode-element-style-margin node :right)))
(defun vnode-element-is-auto-margin-bottom-p (node)
  (vnode-element--auto-p (vnode-element-style-margin node :bottom)))
(defun vnode-element-is-auto-margin-left-p (node)
  (vnode-element--auto-p (vnode-element-style-margin node :left)))

(defun vnode-element-is-auto-padding-top-p (node)
  (vnode-element--auto-p (vnode-element-style-padding node :top)))
(defun vnode-element-is-auto-padding-right-p (node)
  (vnode-element--auto-p (vnode-element-style-padding node :right)))
(defun vnode-element-is-auto-padding-bottom-p (node)
  (vnode-element--auto-p (vnode-element-style-padding node :bottom)))
(defun vnode-element-is-auto-padding-left-p (node)
  (vnode-element--auto-p (vnode-element-style-padding node :left)))

(defun vnode-element-is-auto-border-top-p (node)
  (vnode-element--auto-p (vnode-element-style-border node :top)))
(defun vnode-element-is-auto-border-right-p (node)
  (vnode-element--auto-p (vnode-element-style-border node :right)))
(defun vnode-element-is-auto-border-bottom-p (node)
  (vnode-element--auto-p (vnode-element-style-border node :bottom)))
(defun vnode-element-is-auto-border-left-p (node)
  (vnode-element--auto-p (vnode-element-style-border node :left)))

(defun vnode-element-is-auto-top-p (node)
  (vnode-element--auto-p (vnode-element-style-top node)))
(defun vnode-element-is-auto-right-p (node)
  (vnode-element--auto-p (vnode-element-style-right node)))
(defun vnode-element-is-auto-bottom-p (node)
  (vnode-element--auto-p (vnode-element-style-bottom node)))
(defun vnode-element-is-auto-left-p (node)
  (vnode-element--auto-p (vnode-element-style-left node)))

(defun vnode-element-is-auto-flex-basis-p (node)
  (vnode-element--auto-p (vnode-element-style-flex-basis node)))

;; ═══════════════════════════════════════════════════════════════════
;; 获取子元素尺寸（GetChildWidth / GetChildHeight）
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-get-child-width (child available-height)
  "返回 CHILD 的宽度。优先从 resolved-style 读取，否则调用首选宽度测量。"
  (let ((resolved (vnode-element--effective-style child :width)))
    (if (and resolved (vnode-element--resolved-p resolved))
        resolved
      (vnode-element-get-pref-width child available-height))))

(defun vnode-element-get-child-height (child available-width)
  "返回 CHILD 的高度。优先从 resolved-style 读取，否则调用首选高度测量。"
  (let ((resolved (vnode-element--effective-style child :height)))
    (if (and resolved (vnode-element--resolved-p resolved))
        resolved
      (vnode-element-get-pref-height child available-width))))

;; ═══════════════════════════════════════════════════════════════════
;; 尺寸钳位（ClampMainSize / ClampCrossSize）
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-clamp-main-size (child size is-row)
  "将主轴尺寸 SIZE 限制在 CHILD 的 Min/Max 范围内。"
  (if is-row
      (let ((min (or (vnode-element-style-min-width child) 0))
            (max (or (vnode-element-style-max-width child) most-positive-fixnum)))
        (max min (min size max)))
    (let ((min (or (vnode-element-style-min-height child) 0))
          (max (or (vnode-element-style-max-height child) most-positive-fixnum)))
      (max min (min size max)))))

(defun vnode-element-clamp-cross-size (child size is-row)
  "将交叉轴尺寸 SIZE 限制在 CHILD 的 Min/Max 范围内。"
  (if is-row
      (let ((min (or (vnode-element-style-min-height child) 0))
            (max (or (vnode-element-style-max-height child) most-positive-fixnum)))
        (max min (min size max)))
    (let ((min (or (vnode-element-style-min-width child) 0))
          (max (or (vnode-element-style-max-width child) most-positive-fixnum)))
      (max min (min size max)))))

;; ═══════════════════════════════════════════════════════════════════
;; 获取 Margin 组合（参考 LayoutUtils.GetMargin）
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-get-margin (child is-row is-cross &optional start-only)
  "获取 CHILD 在给定轴上的 margin 总和（或起始一侧）。
IS-ROW 为主轴水平方向，IS-CROSS 为交叉轴。
START-ONLY 非 nil 则只返回起始一侧的 margin。"
  (if is-row
      (if is-cross                  ; 垂直 margin
          (let ((mt (vnode-element-style-margin child :top))
                (mb (vnode-element-style-margin child :bottom)))
            (if start-only mt (+ mt mb)))
        ;; 水平 margin
        (let ((ml (vnode-element-style-margin child :left))
              (mr (vnode-element-style-margin child :right)))
          (if start-only ml (+ ml mr))))
    ;; 主轴为垂直方向
    (if is-cross                  ; 水平 margin（交叉轴）
        (let ((ml (vnode-element-style-margin child :left))
              (mr (vnode-element-style-margin child :right)))
          (if start-only ml (+ ml mr)))
      ;; 垂直 margin（主轴）
      (let ((mt (vnode-element-style-margin child :top))
            (mb (vnode-element-style-margin child :bottom)))
        (if start-only mt (+ mt mb))))))

;; ═══════════════════════════════════════════════════════════════════
;; 内容测量辅助：沿轴测量容器内容尺寸
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-measure-content-along-axis
    (container available-content-size get-child-size get-margin-start get-margin-end accumulate
               padding-start padding-end border-start border-end)
  "沿给定轴测量容器内容尺寸的公共辅助函数。"
  (let ((result 0)
        (count 0))
    (dolist (child (vnode-element-children container))
      (when (vnode-element-p child)
        (let* ((size (funcall get-child-size child available-content-size))
               (occupied (+ (funcall get-margin-start child)
                            size
                            (funcall get-margin-end child))))
          (if accumulate
              (progn
                (setq result (+ result occupied))
                (when (> count 0)
                  (setq result (+ result (vnode-element-style-gap container)))))
            (when (> occupied result)
              (setq result occupied)))
          (setq count (1+ count)))))
    (+ result padding-start padding-end border-start border-end)))

;; ═══════════════════════════════════════════════════════════════════
;; 首选宽度测量（MeasurePreferredWidth）
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-measure-pref-width (container height)
  "根据子元素计算容器的首选宽度（用于自动宽度布局）。
结果会钳制在 MinWidth 与 MaxWidth 之间，并缓存到 resolved-style。"
  (let ((cached (vnode-element--effective-style container :width)))
    (if (vnode-element--resolved-p cached)
        cached
      (let* ((display (vnode-element-style-display container))
             (accumulate
              (if (eq display 'flex)
                  (let ((dir (vnode-element-style-flex-direction container)))
                    (or (eq dir 'row) (eq dir 'row-reverse)))
                nil))
             (content-height
              (- height
                 (vnode-element-style-padding container :top)
                 (vnode-element-style-padding container :bottom)
                 (vnode-element-style-border container :top)
                 (vnode-element-style-border container :bottom)))
             (measured
              (vnode-element-measure-content-along-axis
               container content-height
               (lambda (child ch) (vnode-element-get-child-width child ch))
               (lambda (child) (vnode-element-style-margin child :left))
               (lambda (child) (vnode-element-style-margin child :right))
               accumulate
               (vnode-element-style-padding container :left)
               (vnode-element-style-padding container :right)
               (vnode-element-style-border container :left)
               (vnode-element-style-border container :right)))
             (clamped
              (let ((min (or (vnode-element-style-min-width container) 0))
                    (max (or (vnode-element-style-max-width container) most-positive-fixnum)))
                (max min (min measured max)))))
        (vnode-element--set-resolved! container :width clamped)
        clamped))))

;; ═══════════════════════════════════════════════════════════════════
;; 首选高度测量（MeasurePreferredHeight）
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-measure-pref-height (container width)
  "根据子元素计算容器的首选高度（用于自动高度布局）。
结果会钳制在 MinHeight 与 MaxHeight 之间，并缓存到 resolved-style。"
  (let ((cached (vnode-element--effective-style container :height)))
    (if (vnode-element--resolved-p cached)
        cached
      (let* ((display (vnode-element-style-display container))
             (accumulate
              (if (eq display 'flex)
                  (let ((dir (vnode-element-style-flex-direction container)))
                    (not (or (eq dir 'row) (eq dir 'row-reverse))))
                t))
             (content-width
              (- width
                 (vnode-element-style-padding container :left)
                 (vnode-element-style-padding container :right)
                 (vnode-element-style-border container :left)
                 (vnode-element-style-border container :right)))
             (measured
              (vnode-element-measure-content-along-axis
               container content-width
               (lambda (child cw) (vnode-element-get-child-height child cw))
               (lambda (child) (vnode-element-style-margin child :top))
               (lambda (child) (vnode-element-style-margin child :bottom))
               accumulate
               (vnode-element-style-padding container :top)
               (vnode-element-style-padding container :bottom)
               (vnode-element-style-border container :top)
               (vnode-element-style-border container :bottom)))
             (clamped
              (let ((min (or (vnode-element-style-min-height container) 0))
                    (max (or (vnode-element-style-max-height container) most-positive-fixnum)))
                (max min (min measured max)))))
        (vnode-element--set-resolved! container :height clamped)
        clamped))))

;; ═══════════════════════════════════════════════════════════════════
;; GetPreferredWidth / GetPreferredHeight 叶子节点实现
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-get-pref-width (node available-height)
  "返回 NODE 的自然宽度（列数），优先从 resolved-style 获取。"
  (let ((cached (vnode-element--effective-style node :width)))
    (if (vnode-element--resolved-p cached)
        cached
      (let* ((explicit (vnode-element-style-width node))
             (measured
              (if (not (vnode-element--auto-p explicit))
                  explicit
                (pcase (vnode-element-type node)
                  (`text
                   (if (vnode-element-children node)
                       (let ((str (car (vnode-element-children node))))
                         (if (stringp str)
                             (let ((lines (split-string str "\n")))
                               (apply #'max (mapcar #'string-width lines)))
                           0))
                     0))
                  (`button
                   (let ((label (vnode-element-style--get node :label "")))
                     (+ (string-width label) 2)))
                  (_
                   (if (vnode-element-children node)
                       (vnode-element-measure-pref-width node available-height)
                     0)))))            ;; 无子元素返回 0
             (clamped (vnode-element-clamp-main-size node measured t)))
        (vnode-element--set-resolved! node :width clamped)
        clamped))))

(defun vnode-element-get-pref-height (node available-width)
  "返回 NODE 的自然高度（行数），优先从 resolved-style 获取。"
  (let ((cached (vnode-element--effective-style node :height)))
    (if (vnode-element--resolved-p cached)
        cached
      (let* ((explicit (vnode-element-style-height node))
             (measured
              (if (not (vnode-element--auto-p explicit))
                  explicit
                (pcase (vnode-element-type node)
                  (`text
                   (if (vnode-element-children node)
                       (let ((str (car (vnode-element-children node))))
                         (if (stringp str)
                             (length (split-string str "\n"))
                           0))
                     0))
                  (`button 1)
                  (_
                   (if (vnode-element-children node)
                       (vnode-element-measure-pref-height node available-width)
                     0)))))            ;; 无子元素返回 0，原来是 1
             (clamped (vnode-element-clamp-cross-size node measured t)))
        (vnode-element--set-resolved! node :height clamped)
        clamped))))

(provide 'vnode-element-measure)
;;; vnode-element-measure.el ends here
