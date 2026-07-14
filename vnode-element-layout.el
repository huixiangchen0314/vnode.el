;;; vnode-element-layout.el --- 布局引擎入口及 Block 布局器  -*- lexical-binding: t; -*-

(require 'vnode-element)
(require 'vnode-element-style)
(require 'vnode-element-measure)
(require 'vnode-element-registry)
(require 'cl-lib)

;; ═══════════════════════════════════════════════════════════════════
;; 辅助函数：应用容器尺寸、计算内容区域
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-layout--apply-container-size (node available-cols available-rows start-row start-col)
  "根据用户设置和可用空间，设置 NODE 的几何槽和 resolved-style 的 :width/:height。
AVAILABLE-COLS 和 AVAILABLE-ROWS 是容器可用列数和行数。
返回 (end-row end-col)，表示容器占用的外框结束坐标。"
  (let* ((w (vnode-element-style-width node))
         (h (vnode-element-style-height node))
         (min-w (or (vnode-element-style-min-width node) 0))
         (max-w (or (vnode-element-style-max-width node) most-positive-fixnum))
         (min-h (or (vnode-element-style-min-height node) 0))
         (max-h (or (vnode-element-style-max-height node) most-positive-fixnum))
         ;; 宽度：如果用户指定了非 auto 值则使用，否则用可用宽度
         (final-w (if (vnode-element--auto-p w) available-cols w))
         (final-h (if (vnode-element--auto-p h) available-rows h))
         ;; 钳位
         (final-w (max min-w (min final-w max-w)))
         (final-h (max min-h (min final-h max-h))))
    ;; 写入 resolved-style
    (setf (vnode-element-resolved-style node)
          (plist-put (vnode-element-resolved-style node) :width final-w))
    (setf (vnode-element-resolved-style node)
          (plist-put (vnode-element-resolved-style node) :height final-h))
    ;; 写入几何坐标
    (setf (vnode-element-start-row node) start-row)
    (setf (vnode-element-start-col node) start-col)
    (setf (vnode-element-end-row node) (+ start-row final-h))
    (setf (vnode-element-end-col node) (+ start-col final-w))
    ;; 返回结束坐标
    (list (+ start-row final-h) (+ start-col final-w))))

(defun vnode-element-layout--content-rect (node)
  "返回 NODE 的内容区域 (x y width height)，即去掉 padding 和 border 后的矩形。
坐标是相对于缓冲区起始的绝对位置。"
  (let* ((pad-top (vnode-element-style-padding node :top))
         (pad-right (vnode-element-style-padding node :right))
         (pad-bottom (vnode-element-style-padding node :bottom))
         (pad-left (vnode-element-style-padding node :left))
         (bor-top (vnode-element-style-border node :top))
         (bor-right (vnode-element-style-border node :right))
         (bor-bottom (vnode-element-style-border node :bottom))
         (bor-left (vnode-element-style-border node :left))
         (sx (vnode-element-start-col node))
         (sy (vnode-element-start-row node))
         (ex (vnode-element-end-col node))
         (ey (vnode-element-end-row node)))
    (list (+ sx pad-left bor-left)
          (+ sy pad-top bor-top)
          (- ex sx pad-left pad-right bor-left bor-right)
          (- ey sy pad-top pad-bottom bor-top bor-bottom))))

(defun vnode-element-layout--apply-relative-offset (node)
  "若 NODE 为 relative 定位且设置了 :left/:top，则调整其几何坐标。"
  (when (eq (vnode-element-style-position node) 'relative)
    (let ((left (vnode-element-style-left node))
          (top (vnode-element-style-top node)))
      (when (and left (not (vnode-element--auto-p left)))
        (cl-incf (vnode-element-start-col node) left)
        (cl-incf (vnode-element-end-col node) left))
      (when (and top (not (vnode-element--auto-p top)))
        (cl-incf (vnode-element-start-row node) top)
        (cl-incf (vnode-element-end-row node) top)))))

(defun vnode-element-layout--find-containing-block (node)
  "向上查找第一个 position 不为 static 的祖先节点，若无则返回 nil。"
  (let ((parent (vnode-element-parent node)))
    (while (and parent (eq (vnode-element-style-position parent) 'static))
      (setq parent (vnode-element-parent parent)))
    parent))

(defun vnode-element-layout--absolute-children (container available-cols available-rows)
  "对 CONTAINER 中 position 为 absolute 的子元素进行布局。
使用 AVAILABLE-COLS / AVAILABLE-ROWS 作为无包含块时的视口大小。"
  (dolist (child (vnode-element-children container))
    (when (and (vnode-element-p child)
               (eq (vnode-element-style-position child) 'absolute))
      ;; 建立父子关联，以便查找包含块
      (setf (vnode-element-parent child) container)
      (let* ((cb (vnode-element-layout--find-containing-block child))
             (cb-rect (if cb
                          (vnode-element-layout--content-rect cb)
                        ;; 无包含块则使用传入的视口尺寸
                        (list 0 0 available-cols available-rows)))
             (cb-x (nth 0 cb-rect))
             (cb-y (nth 1 cb-rect))
             (cb-w (nth 2 cb-rect))
             (cb-h (nth 3 cb-rect))
             (left (vnode-element-style-left child))
             (right (vnode-element-style-right child))
             (top (vnode-element-style-top child))
             (bottom (vnode-element-style-bottom child))
             (width (vnode-element-style-width child))
             (height (vnode-element-style-height child))
             ;; 计算最终宽度
             (final-w
              (cond
               ((and (not (vnode-element--auto-p width)) width)
                width)
               ((and (not (vnode-element--auto-p left))
                     (not (vnode-element--auto-p right)))
                (- cb-w left right))
               (t
                (vnode-element-get-pref-width child cb-h))))
             ;; 计算最终高度
             (final-h
              (cond
               ((and (not (vnode-element--auto-p height)) height)
                height)
               ((and (not (vnode-element--auto-p top))
                     (not (vnode-element--auto-p bottom)))
                (- cb-h top bottom))
               (t
                (vnode-element-get-pref-height child final-w))))
             ;; 钳位（假设主轴为水平方向，使用 is-row = t）
             (final-w (vnode-element-clamp-main-size child final-w t))
             (final-h (vnode-element-clamp-cross-size child final-h t))
             ;; 计算水平位置 (x 为列坐标)
             (x (cond
                 ((not (vnode-element--auto-p left))
                  (+ cb-x left))
                 ((not (vnode-element--auto-p right))
                  (+ cb-x cb-w (- right) (- final-w)))
                 (t cb-x)))          ; 若未指定，默认放到包含块左上角
             ;; 计算垂直位置 (y 为行坐标)
             (y (cond
                 ((not (vnode-element--auto-p top))
                  (+ cb-y top))
                 ((not (vnode-element--auto-p bottom))
                  (+ cb-y cb-h (- bottom) (- final-h)))
                 (t cb-y))))
        ;; 递归布局子元素（绝对定位元素内部仍可包含子元素）
        (vnode-element-layout child final-w final-h y x)))))

;; ═══════════════════════════════════════════════════════════════════
;; None 布局
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-layout-none (node _available-cols _available-rows start-row start-col)
  "对 display: none 的节点，尺寸设置为 0。"
  (setf (vnode-element-start-row node) start-row
        (vnode-element-end-row node) start-row
        (vnode-element-start-col node) start-col
        (vnode-element-end-col node) start-col)
  (vnode-element--set-resolved! node :width 0)
  (vnode-element--set-resolved! node :height 0)
  (list start-row start-col))

;; ═══════════════════════════════════════════════════════════════════
;; Block 布局器
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-layout-block (node available-cols available-rows start-row start-col)
  "Block 格式化上下文：子元素垂直堆叠。"
  ;; 1. 应用容器自身尺寸
  (vnode-element-layout--apply-container-size node available-cols available-rows start-row start-col)
  ;; 2. 计算内容区域
  (cl-destructuring-bind (content-x content-y content-width content-height)
      (vnode-element-layout--content-rect node)
    ;; 3. 遍历正常流子元素
    (let ((current-y content-y)
          (max-content-width 0)
          (auto-height (vnode-element--auto-p (vnode-element-style-height node))))
      (dolist (child (vnode-element-children node))
        (when (vnode-element-p child)
          (let* ((display (vnode-element-style-display child))
                 (pos    (vnode-element-style-position child)))
            (cond
             ;; display: none → 尺寸归零，不占空间
             ((eq display 'none)
              (vnode-element-layout-none child content-width content-height content-x current-y))
             ;; 绝对定位 → 稍后统一处理
             ((eq pos 'absolute)
              nil)
             ;; 正常流子元素
             (t
              ;; 建立父子关系
              (setf (vnode-element-parent child) node)
              (let* ((margin-top (vnode-element-style-margin child :top))
                     (margin-bottom (vnode-element-style-margin child :bottom))
                     (margin-left (vnode-element-style-margin child :left))
                     (margin-right (vnode-element-style-margin child :right))
                     ;; 子元素可用宽度（内容宽度减去左右 margin）
                     (child-available-width (- content-width margin-left margin-right))
                     ;; 获取子元素高度
                     (child-height (vnode-element-get-child-height child child-available-width))
                     ;; 子元素宽度：若为 auto，则占满内容宽度，否则用指定值
                     (child-width (if (vnode-element-is-auto-width-p child)
                                      child-available-width
                                    (vnode-element-get-child-width child content-height)))
                     ;; 子元素起点
                     (child-start-row (+ current-y margin-top))
                     (child-start-col (+ content-x margin-left)))
                ;; 递归布局子元素
                (cl-destructuring-bind (_child-end-row _child-end-col)
                    (vnode-element-layout child
                                          child-width
                                          child-height
                                          child-start-row
                                          child-start-col)
                  ;; 应用相对定位偏移
                  (vnode-element-layout--apply-relative-offset child)
                  ;; 更新当前垂直位置
                  (let ((occupied-h (+ child-height margin-top margin-bottom (vnode-element-style-gap node))))
                    (setq current-y (+ current-y occupied-h))
                    ;; 更新内容最大宽度（用于 auto 宽度容器）
                    (when (> (vnode-element-end-col child) max-content-width)
                      (setq max-content-width (vnode-element-end-col child)))))))))))
      ;; 4. 若容器高度为 auto，由内容撑开
      (when auto-height
        (let* ((pad-top (vnode-element-style-padding node :top))
               (pad-bottom (vnode-element-style-padding node :bottom))
               (bor-top (vnode-element-style-border node :top))
               (bor-bottom (vnode-element-style-border node :bottom))
               (content-height-used (- current-y content-y))
               (new-height (+ content-height-used pad-top pad-bottom bor-top bor-bottom))
               (new-height (max (or (vnode-element-style-min-height node) 0)
                                (min new-height (or (vnode-element-style-max-height node) most-positive-fixnum)))))
          (setf (vnode-element-end-row node) (+ start-row new-height))
          ;; 更新 resolved-style 高度
          (vnode-element--set-resolved! node :height new-height)))
      ;; 5. 若容器宽度为 auto，由最宽子元素决定
      (when (vnode-element--auto-p (vnode-element-style-width node))
        (let* ((pad-left (vnode-element-style-padding node :left))
               (pad-right (vnode-element-style-padding node :right))
               (bor-left (vnode-element-style-border node :left))
               (bor-right (vnode-element-style-border node :right))
               ;; 用子元素的最大结束列减去内容起点，得到真实内容宽度
               (content-width-used (- max-content-width content-x))
               (new-width (+ content-width-used pad-left pad-right bor-left bor-right))
               (new-width (max (or (vnode-element-style-min-width node) 0)
                               (min new-width (or (vnode-element-style-max-width node) most-positive-fixnum)))))
          (setf (vnode-element-end-col node) (+ start-col new-width))
          (vnode-element--set-resolved! node :width new-width)))
      ;; 6. 处理绝对定位子元素
      (vnode-element-layout--absolute-children node available-cols available-rows)
      ;; 返回容器结束坐标
      (list (vnode-element-end-row node) (vnode-element-end-col node)))))

;; ═══════════════════════════════════════════════════════════════════
;; Flex 布局器（占位）
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-layout-flex (_node _available-cols _available-rows _start-row _start-col)
  "Flex 布局器（尚未实现）。"
  (error "Flex layout not implemented yet"))

;; ═══════════════════════════════════════════════════════════════════
;; 主入口：根据注册表或 display 分派
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-layout (node available-cols available-rows start-row start-col)
  "对 NODE 及其子树进行布局，返回 (end-row end-col)。
优先使用注册表中的布局函数，否则根据 :display 属性分派。"
  (unless (vnode-element-p node)
    (error "vnode-element-layout: node must be a vnode-element"))
  (let ((registered (vnode-element--find-layout (vnode-element-type node))))
    (if registered
        (funcall registered node available-cols available-rows start-row start-col)
      ;; 未注册 → 容器节点，按 display 分派
      (let ((display (vnode-element-style-display node)))
        (cl-case display
          ((block)   (vnode-element-layout-block node available-cols available-rows start-row start-col))
          ((flex)    (vnode-element-layout-flex node available-cols available-rows start-row start-col))
          ((inline)  (error "Inline layout not implemented yet"))
          ((none)    (vnode-element-layout-none node available-cols available-rows start-row start-col))
          (otherwise (error "Unknown display type: %S" display)))))))

(provide 'vnode-element-layout)
;;; vnode-element-layout.el ends here
