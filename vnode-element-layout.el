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
         (bor-top (vnode-element-style-border-width node :top))
         (bor-right (vnode-element-style-border-width node :right))
         (bor-bottom (vnode-element-style-border-width node :bottom))
         (bor-left (vnode-element-style-border-width node :left))
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
               (bor-top (vnode-element-style-border-width node :top))
               (bor-bottom (vnode-element-style-border-width node :bottom))
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
               (bor-left (vnode-element-style-border-width node :left))
               (bor-right (vnode-element-style-border-width node :right))
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
;; Flex 布局器
;; ═══════════════════════════════════════════════════════════════════

(cl-defstruct (vnode-element-layout-flex--child-info
               (:constructor vnode-element-layout-flex--child-info-create)
               (:conc-name vnode-element-layout-flex--child-info-))
  child base-main main-margin-total final-main final-cross)

(cl-defstruct (vnode-element-layout-flex--line-info
               (:constructor vnode-element-layout-flex--line-info-create)
               (:conc-name vnode-element-layout-flex--line-info-))
  child-infos cross-size cross-pos)

(defun vnode-element-layout-flex (container available-cols available-rows start-row start-col)
  "Flex 格式化上下文。"
  ;; 1. 仅应用宽度，高度暂保留 0（后续由内容决定）
  (let* ((w (vnode-element-style-width container))
         (min-w (or (vnode-element-style-min-width container) 0))
         (max-w (or (vnode-element-style-max-width container) most-positive-fixnum))
         (final-w (if (vnode-element--auto-p w) available-cols w))
         (final-w (max min-w (min final-w max-w))))
    (vnode-element--set-resolved! container :width final-w)
    (vnode-element--set-resolved! container :height 0)
    (setf (vnode-element-start-row container) start-row
          (vnode-element-start-col container) start-col
          (vnode-element-end-row container) start-row
          (vnode-element-end-col container) (+ start-col final-w)))
  ;; 2. 计算内容区域
  (cl-destructuring-bind (content-x content-y content-width content-height)
      (vnode-element-layout--content-rect container)
    (when (< content-height 0) (setq content-height 0))
    (let* ((direction (vnode-element-style-flex-direction container))
           (is-row (or (eq direction 'row) (eq direction 'row-reverse)))
           (is-reverse (or (eq direction 'row-reverse) (eq direction 'column-reverse)))
           (wrap (vnode-element-style-flex-wrap container))
           (gap (vnode-element-style-gap container))
           (main-content-size (if is-row content-width content-height))
           (cross-content-size (if is-row content-height content-width))
           (main-start (if is-row content-x content-y))
           (cross-start (if is-row content-y content-x))
           (normal-children (cl-remove-if-not
                             (lambda (c) (and (vnode-element-p c)
                                              (not (eq (vnode-element-style-display c) 'none))
                                              (not (eq (vnode-element-style-position c) 'absolute))))
                             (vnode-element-children container)))
           (total-main-size 0)
           (max-cross-occupied 0))
      (dolist (child normal-children)
        (setf (vnode-element-parent child) container))
      (when normal-children
        (if (eq wrap 'nowrap)
            (cl-destructuring-bind (line-main line-cross _)
                (vnode-element-layout-flex--layout-line
                 container normal-children is-row is-reverse
                 main-content-size cross-content-size main-start cross-start gap)
              (setq total-main-size line-main)
              (setq max-cross-occupied line-cross))
          (cl-destructuring-bind (tm mc)
              (vnode-element-layout-flex--layout-multiline
               container normal-children is-row is-reverse
               main-content-size cross-content-size main-start cross-start gap)
            (setq total-main-size tm)
            (setq max-cross-occupied mc))))
      ;; 绝对定位
      (vnode-element-layout--absolute-children container available-cols available-rows)
      ;; 调整容器尺寸
      (vnode-element-layout-flex--adjust-container-size
       container total-main-size max-cross-occupied is-row)
      ;; 返回结束坐标
      (list (vnode-element-end-row container) (vnode-element-end-col container)))))

(defun vnode-element-layout-flex--layout-line (container children is-row _is-reverse
                                                main-content-size cross-content-size
                                                main-start cross-start gap)
  "布局一行 Flex 子元素，返回 (line-main-size line-cross-size child-infos)。"
  (let* ((child-infos (vnode-element-layout-flex--measure-main-axis children is-row main-content-size gap))
         (total-flex-grow (apply #'+ (mapcar (lambda (ci)
                                               (or (vnode-element-style-flex-grow (vnode-element-layout-flex--child-info-child ci)) 0))
                                             child-infos)))
         (total-base-main (apply #'+ (mapcar (lambda (ci) (+ (vnode-element-layout-flex--child-info-base-main ci)
                                                              (vnode-element-layout-flex--child-info-main-margin-total ci)))
                                           child-infos)))
         (remaining-space (- main-content-size total-base-main (* gap (1- (length children))))))
    (vnode-element-layout-flex--distribute-free-space child-infos total-flex-grow remaining-space is-row)
    (let* ((align-items (vnode-element-style-align-items container))
           (line-cross-size (vnode-element-layout-flex--measure-cross-axis child-infos is-row cross-content-size align-items))
           (justify (vnode-element-layout-flex--compute-main-justify container children child-infos gap main-content-size main-start))
           (main-offset (nth 0 justify))
           (space-between (nth 1 justify)))
      (vnode-element-layout-flex--place-children child-infos is-row main-offset space-between cross-start)
      (vnode-element-layout-flex--align-cross-items child-infos is-row cross-start line-cross-size align-items)
      (let ((line-main-size (+ (apply #'+ (mapcar (lambda (ci) (+ (vnode-element-layout-flex--child-info-final-main ci)
                                                                   (vnode-element-layout-flex--child-info-main-margin-total ci)))
                                                 child-infos))
                               (* gap (1- (length children))))))
        (list line-main-size line-cross-size child-infos)))))

(defun vnode-element-layout-flex--layout-multiline (container children is-row _is-reverse
                                                     main-content-size cross-content-size
                                                     main-start cross-start gap)
  "多行 Flex 布局，返回 (total-main-size max-cross-occupied)。"
  (let* ((lines (vnode-element-layout-flex--collect-lines children is-row main-content-size cross-content-size gap))
         (is-wrap-reverse (eq (vnode-element-style-flex-wrap container) 'wrap-reverse))
         (line-infos '()))
    (when is-wrap-reverse (setq lines (nreverse lines)))
    (dolist (line lines)
      (cl-destructuring-bind (_line-main line-cross child-infos)
          (vnode-element-layout-flex--layout-line container line is-row nil
                                                   main-content-size cross-content-size
                                                   main-start cross-start gap)
        (push (vnode-element-layout-flex--line-info-create :child-infos child-infos :cross-size line-cross)
              line-infos)))
    (setq line-infos (nreverse line-infos))
    (vnode-element-layout-flex--apply-align-content container line-infos cross-content-size gap)
    (let ((current-cross (if is-wrap-reverse (+ cross-start cross-content-size) cross-start))
          (extra-gap (or (plist-get (vnode-element-props container) :align-content-extra-gap) 0))
          (block-offset (or (plist-get (vnode-element-props container) :align-content-block-offset) 0)))
      (if is-wrap-reverse
          (setq current-cross (- current-cross block-offset))
        (setq current-cross (+ current-cross block-offset)))
      (dolist (line-info line-infos)
        (let* ((line-cross-pos (if is-wrap-reverse
                                   (- current-cross (vnode-element-layout-flex--line-info-cross-size line-info))
                                 current-cross))
               (line-cross-size (vnode-element-layout-flex--line-info-cross-size line-info)))
          (setf (vnode-element-layout-flex--line-info-cross-pos line-info) line-cross-pos)
          (dolist (info (vnode-element-layout-flex--line-info-child-infos line-info))
            (let ((child (vnode-element-layout-flex--child-info-child info)))
              (when (vnode-element-layout-flex--should-stretch child (vnode-element-style-align-items container))
                (let* ((margin-start (vnode-element-get-margin child is-row t t))
                       (margin-end (- (vnode-element-get-margin child is-row t) margin-start))
                       (new-cross (- line-cross-size margin-start margin-end)))
                  (setf (vnode-element-layout-flex--child-info-final-cross info)
                        (vnode-element-clamp-cross-size child new-cross is-row))
                  (if is-row
                      (setf (vnode-element-end-row child) (+ (vnode-element-start-row child) new-cross))
                    (setf (vnode-element-end-col child) (+ (vnode-element-start-col child) new-cross)))))
              (vnode-element-layout-flex--align-cross-item child is-row line-cross-pos line-cross-size
                                                           (vnode-element-style-align-items container)))))
          (if is-wrap-reverse
              (setq current-cross (- line-cross-pos gap extra-gap))
            (setq current-cross (+ line-cross-pos line-cross-size gap extra-gap))))))
    ;; 计算总占用（循环外）
    (let ((min-pos (apply #'min (mapcar #'vnode-element-layout-flex--line-info-cross-pos line-infos)))
          (max-pos (apply #'max (mapcar (lambda (li) (+ (vnode-element-layout-flex--line-info-cross-pos li)
                                                        (vnode-element-layout-flex--line-info-cross-size li)))
                                        line-infos))))
      (list main-content-size (- max-pos cross-start))))

;; ── 辅助函数 ──

(defun vnode-element-layout-flex--measure-main-axis (children is-row main-content-size gap)
  (let ((infos '()))
    (dolist (child children)
      (let* ((base-main (vnode-element-layout-flex--get-base-main child is-row main-content-size))
             (clamped (vnode-element-clamp-main-size child base-main is-row))
             (margin-start (vnode-element-get-margin child is-row nil t))
             (margin-end (- (vnode-element-get-margin child is-row nil) margin-start))
             (margin-total (+ margin-start margin-end)))
        (push (vnode-element-layout-flex--child-info-create
               :child child :base-main clamped :main-margin-total margin-total)
              infos)))
    (nreverse infos)))

(defun vnode-element-layout-flex--get-base-main (child is-row available-cross)
  (let ((basis (vnode-element-style-flex-basis child)))
    (cond ((and (not (vnode-element--auto-p basis)) basis) basis)
          (t (if is-row
                 (vnode-element-get-child-width child available-cross)
               (vnode-element-get-child-height child available-cross))))))

(defun vnode-element-layout-flex--distribute-free-space (child-infos total-flex-grow remaining-space is-row)
  (when (and (> total-flex-grow 0) (> remaining-space 0))
    (let ((space-left remaining-space))
      (dolist (info child-infos)
        (let* ((child (vnode-element-layout-flex--child-info-child info))
               (grow (or (vnode-element-style-flex-grow child) 0))
               (extra (if (> total-flex-grow 0) (round (* grow (/ (float space-left) total-flex-grow))) 0)))
          (setf (vnode-element-layout-flex--child-info-final-main info)
                (+ (vnode-element-layout-flex--child-info-base-main info) extra))
          (setf (vnode-element-layout-flex--child-info-final-main info)
                (max (vnode-element-layout-flex--child-info-final-main info)
                     (if is-row (or (vnode-element-style-min-width child) 0)
                       (or (vnode-element-style-min-height child) 0))))))))
  (dolist (info child-infos)
    (unless (vnode-element-layout-flex--child-info-final-main info)
      (setf (vnode-element-layout-flex--child-info-final-main info)
            (vnode-element-layout-flex--child-info-base-main info)))))

(defun vnode-element-layout-flex--measure-cross-axis (child-infos is-row cross-content-size parent-align)
  (let ((max-occupied 0))
    (dolist (info child-infos)
      (let* ((child (vnode-element-layout-flex--child-info-child info))
             (cross-size
              (if (vnode-element-layout-flex--should-stretch child parent-align)
                  (- cross-content-size (vnode-element-get-margin child is-row t))
                (vnode-element-layout-flex--get-base-cross child is-row
                 (vnode-element-layout-flex--child-info-final-main info))))
             (clamped (vnode-element-clamp-cross-size child cross-size is-row)))
        (setf (vnode-element-layout-flex--child-info-final-cross info) clamped)
        (let ((occupied (+ clamped (vnode-element-get-margin child is-row t))))
          (when (> occupied max-occupied) (setq max-occupied occupied)))))
    max-occupied))

(defun vnode-element-layout-flex--get-base-cross (child is-row main-size)
  (if is-row
      (vnode-element-get-child-height child main-size)
    (vnode-element-get-child-width child main-size)))

(defun vnode-element-layout-flex--should-stretch (child parent-align)
  (let ((self (vnode-element-style-align-self child)))
    (or (eq self 'stretch)
        (and (eq self 'auto) (eq parent-align 'stretch)))))

(defun vnode-element-layout-flex--compute-main-justify (container children child-infos gap main-content-size main-start)
  (let* ((total-final (apply #'+ (mapcar (lambda (ci) (+ (vnode-element-layout-flex--child-info-final-main ci)
                                                          (vnode-element-layout-flex--child-info-main-margin-total ci)))
                                         child-infos)))
         (free-space (- main-content-size total-final (* gap (1- (length children)))))
         (justify (vnode-element-style-justify-content container))
         (main-offset main-start)
         (space-between gap))
    (cond
     ((eq justify 'flex-end) (setq main-offset (+ main-start free-space)))
     ((eq justify 'center) (setq main-offset (+ main-start (floor free-space 2))))
     ((eq justify 'space-between)
      (when (> (length children) 1)
        (setq space-between (+ gap (floor free-space (1- (length children)))))))
     ((eq justify 'space-around)
      (let ((half (floor free-space (length children))))
        (setq main-offset (+ main-start (floor half 2)))
        (setq space-between (+ gap half)))))
    (list main-offset space-between)))

(defun vnode-element-layout-flex--place-children (child-infos is-row main-offset space-between cross-start)
  (let ((current-main main-offset))
    (dolist (info child-infos)
      (let* ((child (vnode-element-layout-flex--child-info-child info))
             (margin-start (vnode-element-get-margin child is-row nil t))
             (margin-end (- (vnode-element-get-margin child is-row nil) margin-start))
             (final-main (vnode-element-layout-flex--child-info-final-main info))
             (final-cross (vnode-element-layout-flex--child-info-final-cross info))
             (child-main-pos (+ current-main margin-start))
             (child-cross-pos (+ cross-start (vnode-element-get-margin child is-row t t)))
             (start-row (if is-row child-cross-pos child-main-pos))
             (start-col (if is-row child-main-pos child-cross-pos))
             (width (if is-row final-main final-cross))
             (height (if is-row final-cross final-main)))
        (vnode-element-layout child width height start-row start-col)
        (setq current-main (+ current-main final-main margin-end space-between))))))

(defun vnode-element-layout-flex--align-cross-item (child is-row line-cross-pos line-cross-size parent-align)
  (let* ((self (vnode-element-style-align-self child))
         (effective (cond ((eq self 'flex-start) 'start)
                          ((eq self 'flex-end) 'end)
                          ((eq self 'center) 'center)
                          ((eq self 'stretch) 'stretch)
                          (t parent-align)))
         (margin-start (vnode-element-get-margin child is-row t t))
         (margin-end (- (vnode-element-get-margin child is-row t) margin-start))
         (child-size (if is-row (- (vnode-element-end-row child) (vnode-element-start-row child))
                       (- (vnode-element-end-col child) (vnode-element-start-col child)))))
    (unless (eq effective 'stretch)
      (let ((new-pos
             (cond
              ((eq effective 'start) line-cross-pos)
              ((eq effective 'end) (+ line-cross-pos line-cross-size (- child-size) (- margin-end)))
              ((eq effective 'center) (+ line-cross-pos margin-start
                                         (floor (- line-cross-size margin-start margin-end child-size) 2)))
              (t line-cross-pos))))
        (if is-row
            (setf (vnode-element-start-row child) new-pos
                  (vnode-element-end-row child) (+ new-pos child-size))
          (setf (vnode-element-start-col child) new-pos
                (vnode-element-end-col child) (+ new-pos child-size)))))))

(defun vnode-element-layout-flex--align-cross-items (child-infos is-row cross-start line-cross-size parent-align)
  (dolist (info child-infos)
    (vnode-element-layout-flex--align-cross-item (vnode-element-layout-flex--child-info-child info)
                                                 is-row cross-start line-cross-size parent-align)))

(defun vnode-element-layout-flex--collect-lines (children is-row main-content-size cross-content-size gap)
  (let ((lines '()) (current-line '()) (current-main-size 0))
    (dolist (child children)
      (let* ((base-main (vnode-element-layout-flex--get-base-main child is-row cross-content-size))
             (clamped (vnode-element-clamp-main-size child base-main is-row))
             (margin (vnode-element-get-margin child is-row nil))
             (child-size (+ clamped margin)))
        (when (and current-line (> (+ current-main-size child-size (if current-line gap 0)) main-content-size))
          (push (nreverse current-line) lines)
          (setq current-line nil)
          (setq current-main-size 0))
        (push child current-line)
        (setq current-main-size (+ current-main-size child-size (if (cdr current-line) gap 0)))))
    (when current-line (push (nreverse current-line) lines))
    (nreverse lines)))

(defun vnode-element-layout-flex--apply-align-content (container line-infos cross-content-size gap)
  (let* ((sum-cross (apply #'+ (mapcar #'vnode-element-layout-flex--line-info-cross-size line-infos)))
         (total-gaps (* gap (1- (length line-infos))))
         (free-cross (- cross-content-size sum-cross total-gaps))
         (align (vnode-element-style-align-content container))
         (extra-gap 0)
         (block-offset 0))
    (when (> (length line-infos) 0)
      (cond
       ((eq align 'stretch)
        (when (> free-cross 0)
          (let ((add-each (floor free-cross (length line-infos))))
            (dolist (li line-infos)
              (cl-incf (vnode-element-layout-flex--line-info-cross-size li) add-each)))))
       ((eq align 'space-between)
        (when (> (length line-infos) 1)
          (setq extra-gap (floor free-cross (1- (length line-infos))))))
       ((eq align 'space-around)
        (setq extra-gap (floor free-cross (length line-infos)))
        (setq block-offset (floor extra-gap 2)))
       ((eq align 'center) (setq block-offset (floor free-cross 2)))
       ((eq align 'flex-end) (setq block-offset free-cross))))
    (setf (vnode-element-props container) (plist-put (vnode-element-props container) :align-content-extra-gap extra-gap))
    (setf (vnode-element-props container) (plist-put (vnode-element-props container) :align-content-block-offset block-offset))))

(defun vnode-element-layout-flex--adjust-container-size (container total-main-size max-cross-occupied is-row)
  "调整容器主轴和交叉轴尺寸（若 auto）。"
  ;; 主轴方向
  (if is-row
      (when (vnode-element--auto-p (vnode-element-style-width container))
        (let* ((pad-left (vnode-element-style-padding container :left))
               (pad-right (vnode-element-style-padding container :right))
               (bor-left (vnode-element-style-border-width container :left))
               (bor-right (vnode-element-style-border-width container :right))
               (new-w (+ total-main-size pad-left pad-right bor-left bor-right))
               (new-w (max (or (vnode-element-style-min-width container) 0)
                           (min new-w (or (vnode-element-style-max-width container) most-positive-fixnum)))))
          (setf (vnode-element-end-col container) (+ (vnode-element-start-col container) new-w))
          (vnode-element--set-resolved! container :width new-w)))
    (when (vnode-element--auto-p (vnode-element-style-height container))
      (let* ((pad-top (vnode-element-style-padding container :top))
             (pad-bottom (vnode-element-style-padding container :bottom))
             (bor-top (vnode-element-style-border-width container :top))
             (bor-bottom (vnode-element-style-border-width container :bottom))
             (new-h (+ total-main-size pad-top pad-bottom bor-top bor-bottom))
             (new-h (max (or (vnode-element-style-min-height container) 0)
                         (min new-h (or (vnode-element-style-max-height container) most-positive-fixnum)))))
        (setf (vnode-element-end-row container) (+ (vnode-element-start-row container) new-h))
        (vnode-element--set-resolved! container :height new-h))))
  ;; 交叉轴方向
  (if is-row
      (when (vnode-element--auto-p (vnode-element-style-height container))
        (let* ((pad-top (vnode-element-style-padding container :top))
               (pad-bottom (vnode-element-style-padding container :bottom))
               (bor-top (vnode-element-style-border-width container :top))
               (bor-bottom (vnode-element-style-border-width container :bottom))
               (new-h (+ max-cross-occupied pad-top pad-bottom bor-top bor-bottom))
               (new-h (max (or (vnode-element-style-min-height container) 0)
                           (min new-h (or (vnode-element-style-max-height container) most-positive-fixnum)))))
          (setf (vnode-element-end-row container) (+ (vnode-element-start-row container) new-h))
          (vnode-element--set-resolved! container :height new-h)))
    (when (vnode-element--auto-p (vnode-element-style-width container))
      (let* ((pad-left (vnode-element-style-padding container :left))
             (pad-right (vnode-element-style-padding container :right))
             (bor-left (vnode-element-style-border-width container :left))
             (bor-right (vnode-element-style-border-width container :right))
             (new-w (+ max-cross-occupied pad-left pad-right bor-left bor-right))
             (new-w (max (or (vnode-element-style-min-width container) 0)
                         (min new-w (or (vnode-element-style-max-width container) most-positive-fixnum)))))
        (setf (vnode-element-end-col container) (+ (vnode-element-start-col container) new-w))
        (vnode-element--set-resolved! container :width new-w)))))

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
      (let ((display (vnode-element-style-display node)))
        (cl-case display
          ((block)   (vnode-element-layout-block node available-cols available-rows start-row start-col))
          ((flex)    (vnode-element-layout-flex node available-cols available-rows start-row start-col))
          ((inline)  (error "Inline layout not implemented yet"))
          ((none)    (vnode-element-layout-none node available-cols available-rows start-row start-col))
          (otherwise (error "Unknown display type: %S" display)))))))

(provide 'vnode-element-layout)
;;; vnode-element-layout.el ends here
