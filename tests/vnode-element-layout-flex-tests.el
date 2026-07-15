;;; vnode-element-layout-flex-tests.el --- 测试 Flex 布局引擎  -*- lexical-binding: t; -*-

(require 'ert)
(require 'vnode-element)
(require 'vnode-element-style)
(require 'vnode-element-measure)
(require 'vnode-element-layout)
(require 'vnode-element-controls)      ; 确保 text 等注册

;; ═══════════════════════════════════════════════════════════════════
;; 辅助函数
;; ═══════════════════════════════════════════════════════════════════

(defun ltest--text-node (text)
  (vnode-element-desc-to-element `(text ,text)))

(defun ltest--layout-root (root &optional cols rows)
  (vnode-element-layout root (or cols 80) (or rows 24) 1 0))

(defun ltest--geom (node)
  (list (vnode-element-start-row node)
        (vnode-element-start-col node)
        (vnode-element-end-row node)
        (vnode-element-end-col node)))

;; ═══════════════════════════════════════════════════════════════════
;; 单行 Row 方向
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-flex-row-simple ()
  "Flex row 容器：两个文本水平排列，宽度累加。"
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :gap 0
                     (text :width 10 "a")
                     (text :width 10 "b")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom (nth 0 (vnode-element-children root))) '(1 0 2 10)))
    (should (equal (ltest--geom (nth 1 (vnode-element-children root))) '(1 10 2 20)))))

(ert-deftest test-flex-row-gap ()
  "Flex row 容器：gap 应产生间距。"
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :gap 2
                     (text :width 10 "a")
                     (text :width 10 "b")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom (nth 0 (vnode-element-children root))) '(1 0 2 10)))
    (should (equal (ltest--geom (nth 1 (vnode-element-children root))) '(1 12 2 22)))))

(ert-deftest test-flex-row-height-max ()
  "Flex row 方向：容器高度取子元素最大高度。"
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row
                     (text :width 10 :height 2 "tall")
                     (text :width 10 "a")))))
    (ltest--layout-root root 80 24)
    ;; 容器高度应为 2（最高子元素高度）
    (should (= (vnode-element-end-row root) 3))))

(ert-deftest test-flex-row-justify-center ()
  "主轴居中对齐。"
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :justify-content center :width 80
                     (text :width 10 "a")
                     (text :width 10 "b")))))
    (ltest--layout-root root 80 24)
    ;; 总宽度 20，可用 80，剩余 60，居中偏移 30
    (should (equal (ltest--geom (nth 0 (vnode-element-children root))) '(1 30 2 40)))
    (should (equal (ltest--geom (nth 1 (vnode-element-children root))) '(1 40 2 50)))))

(ert-deftest test-flex-row-justify-space-between ()
  "主轴两端对齐。"
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :justify-content space-between :width 80
                     (text :width 10 "a")
                     (text :width 10 "b")))))
    (ltest--layout-root root 80 24)
    ;; 剩余空间 60，元素间距为 60
    (should (equal (ltest--geom (nth 0 (vnode-element-children root))) '(1 0 2 10)))
    (should (equal (ltest--geom (nth 1 (vnode-element-children root))) '(1 70 2 80)))))

;; ═══════════════════════════════════════════════════════════════════
;; 单行 Column 方向
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-flex-col-simple ()
  "Flex col 容器：两个文本垂直排列，高度累加。"
  (let ((root (vnode-element-desc-to-element
               '(col :display flex :flex-direction col :gap 0
                     (text :height 1 :width 10 "a")
                     (text :height 1 :width 10 "b")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom (nth 0 (vnode-element-children root))) '(1 0 2 10)))
    (should (equal (ltest--geom (nth 1 (vnode-element-children root))) '(2 0 3 10)))))

(ert-deftest test-flex-col-gap ()
  "Flex col 方向：gap 应产生间距。"
  (let ((root (vnode-element-desc-to-element
               '(col :display flex :flex-direction col :gap 3
                     (text :height 1 :width 10 "a")
                     (text :height 1 :width 10 "b")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom (nth 0 (vnode-element-children root))) '(1 0 2 10)))
    (should (equal (ltest--geom (nth 1 (vnode-element-children root))) '(5 0 6 10)))))

;; ═══════════════════════════════════════════════════════════════════
;; 弹性尺寸 (flex-grow)
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-flex-grow-simple ()
  "单元素 flex-grow 应占满剩余空间。"
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :width 80
                     (text :flex-grow 1 "a")))))
    (ltest--layout-root root 80 24)
    ;; 剩余空间全部分配给 a，宽度为 80
    (should (equal (ltest--geom (car (vnode-element-children root))) '(1 0 2 80)))))

(ert-deftest test-flex-grow-ratio ()
  "两个元素按 flex-grow 比例分配剩余空间。"
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :width 100
                     (text :width 10 :flex-grow 1 "a")
                     (text :width 10 :flex-grow 2 "b")))))
    (ltest--layout-root root 100 24)
    ;; 基准宽度 10+10=20，剩余 80。a 得 80*(1/3)≈26，b 得 80*(2/3)≈53（整数取整）
    (let ((a (nth 0 (vnode-element-children root)))
          (b (nth 1 (vnode-element-children root))))
      (should (= (- (vnode-element-end-col a) (vnode-element-start-col a)) 37))  ; 10+27=37
      (should (= (- (vnode-element-end-col b) (vnode-element-start-col b)) 63))  ; 10+53=63
      (should (= (vnode-element-start-col a) 0))
      (should (= (vnode-element-end-col a) 37))
      (should (= (vnode-element-end-col b) 100)))))   ; 填满容器

;; ═══════════════════════════════════════════════════════════════════
;; 交叉轴对齐 (align-items / align-self)
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-flex-align-items-center ()
  "align-items center 使子元素在交叉轴居中。"
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :height 5 :align-items center
                     (text :width 10 :height 1 "a")))))
    (ltest--layout-root root 80 24)
    (let ((child (car (vnode-element-children root))))
      ;; 居中：(5-1)/2 = 2 偏移
      (should (equal (ltest--geom child) '(3 0 4 10))))))

(ert-deftest test-flex-align-self-override ()
  "align-self 覆盖父容器 align-items。"
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :height 5 :align-items center
                     (text :width 10 :height 1 :align-self flex-start "a")))))
    (ltest--layout-root root 80 24)
    (let ((child (car (vnode-element-children root))))
      (should (equal (ltest--geom child) '(1 0 2 10))))))

;; ═══════════════════════════════════════════════════════════════════
;; 多行换行 (flex-wrap)
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-flex-wrap-basic ()
  "flex-wrap: wrap 应换行。"
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :width 20 :flex-wrap wrap :gap 0
                     (text :width 10 :height 1 "a")
                     (text :width 10 :height 1 "b")
                     (text :width 10 :height 1 "c")))))
    (ltest--layout-root root 20 24)
    ;; 第一行：a, b（共20）；第二行：c
    (let ((a (nth 0 (vnode-element-children root)))
          (b (nth 1 (vnode-element-children root)))
          (c (nth 2 (vnode-element-children root))))
      (should (equal (ltest--geom a) '(1 0 2 10)))
      (should (equal (ltest--geom b) '(1 10 2 20)))
      (should (equal (ltest--geom c) '(2 0 3 10))))))

;; ═══════════════════════════════════════════════════════════════════
;; 绝对定位在 Flex 中
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-flex-absolute-not-in-flow ()
  "Flex 容器中的绝对定位元素不影响正常流。"
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :width 40
                     (text :width 10 "a")
                     (text :position absolute :left 5 :top 1 :width 5 :height 1 "abs")
                     (text :width 10 "b")))))
    (ltest--layout-root root 80 24)
    (let ((a (nth 0 (vnode-element-children root)))
          (abs (nth 1 (vnode-element-children root)))
          (b (nth 2 (vnode-element-children root))))
      ;; 正常流 a 和 b 仍在同一行，abs 出现在容器左上角偏移处
      (should (equal (ltest--geom a) '(1 0 2 10)))
      (should (equal (ltest--geom b) '(1 10 2 20)))
      ;; abs 相对于容器内容区 (0,0) 偏移 left 5, top 1
      (should (equal (ltest--geom abs) '(2 5 3 10))))))

(provide 'vnode-element-layout-flex-tests)
;;; vnode-element-layout-flex-tests.el ends here
