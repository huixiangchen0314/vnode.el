;;; vnode-element-layout-flex-tests.el --- 测试 Flex 布局引擎 (修正)  -*- lexical-binding: t; -*-

(require 'ert)
(require 'vnode-element)
(require 'vnode-element-style)
(require 'vnode-element-measure)
(require 'vnode-element-layout)
(require 'vnode-element-controls)

(defun ltest--text-node (text)
  (vnode-element-desc-to-element `(text ,text)))

(defun ltest--layout-root (root &optional cols rows)
  (vnode-element-layout root (or cols 80) (or rows 24) 1 0))

(defun ltest--geom (node)
  (list (vnode-element-start-row node)
        (vnode-element-start-col node)
        (vnode-element-end-row node)
        (vnode-element-end-col node)))

;; ── 单行 Row 方向 ──
(ert-deftest test-flex-row-simple ()
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :gap 0 :align-items flex-start
                     (text :width 10 "a")
                     (text :width 10 "b")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom (nth 0 (vnode-element-children root))) '(1 0 2 10)))
    (should (equal (ltest--geom (nth 1 (vnode-element-children root))) '(1 10 2 20)))))

(ert-deftest test-flex-row-gap ()
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :gap 2 :align-items flex-start
                     (text :width 10 "a")
                     (text :width 10 "b")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom (nth 0 (vnode-element-children root))) '(1 0 2 10)))
    (should (equal (ltest--geom (nth 1 (vnode-element-children root))) '(1 12 2 22)))))

(ert-deftest test-flex-row-height-max ()
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :height 2 :align-items flex-start
                     (text :width 10 :height 1 "a")
                     (text :width 10 :height 2 "tall")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom (nth 0 (vnode-element-children root))) '(1 0 2 10)))
    (should (equal (ltest--geom (nth 1 (vnode-element-children root))) '(1 10 3 20)))))

(ert-deftest test-flex-row-justify-center ()
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :justify-content center :width 80 :align-items flex-start
                     (text :width 10 "a")
                     (text :width 10 "b")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom (nth 0 (vnode-element-children root))) '(1 30 2 40)))
    (should (equal (ltest--geom (nth 1 (vnode-element-children root))) '(1 40 2 50)))))

(ert-deftest test-flex-row-justify-space-between ()
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :justify-content space-between :width 80 :align-items flex-start
                     (text :width 10 "a")
                     (text :width 10 "b")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom (nth 0 (vnode-element-children root))) '(1 0 2 10)))
    (should (equal (ltest--geom (nth 1 (vnode-element-children root))) '(1 70 2 80)))))

;; ── 单行 Column 方向 ──
(ert-deftest test-flex-col-simple ()
  (let ((root (vnode-element-desc-to-element
               '(col :display flex :flex-direction col :gap 0 :align-items flex-start
                     (text :height 1 :width 10 "a")
                     (text :height 1 :width 10 "b")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom (nth 0 (vnode-element-children root))) '(1 0 2 10)))
    (should (equal (ltest--geom (nth 1 (vnode-element-children root))) '(2 0 3 10)))))

(ert-deftest test-flex-col-gap ()
  (let ((root (vnode-element-desc-to-element
               '(col :display flex :flex-direction col :gap 3 :align-items flex-start
                     (text :height 1 :width 10 "a")
                     (text :height 1 :width 10 "b")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom (nth 0 (vnode-element-children root))) '(1 0 2 10)))
    (should (equal (ltest--geom (nth 1 (vnode-element-children root))) '(5 0 6 10)))))

;; ── 弹性尺寸 ──
(ert-deftest test-flex-grow-simple ()
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :width 80 :align-items flex-start
                     (text :flex-grow 1 "a")))))
    (ltest--layout-root root 80 24)
    (should (equal (ltest--geom (car (vnode-element-children root))) '(1 0 2 80)))))

(ert-deftest test-flex-grow-ratio ()
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :width 100 :align-items flex-start
                     (text :width 10 :flex-grow 1 "a")
                     (text :width 10 :flex-grow 2 "b")))))
    (ltest--layout-root root 100 24)
    (let ((a (nth 0 (vnode-element-children root)))
          (b (nth 1 (vnode-element-children root))))
      (should (= (vnode-element-end-col a) 37))
      (should (= (vnode-element-end-col b) 100)))))

;; ── 交叉轴对齐 ──
(ert-deftest test-flex-align-items-center ()
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :height 5 :align-items center
                     (text :width 10 :height 1 "a")))))
    (ltest--layout-root root 80 24)
    (let ((child (car (vnode-element-children root))))
      (should (equal (ltest--geom child) '(3 0 4 10))))))

(ert-deftest test-flex-align-self-override ()
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :height 5 :align-items center
                     (text :width 10 :height 1 :align-self flex-start "a")))))
    (ltest--layout-root root 80 24)
    (let ((child (car (vnode-element-children root))))
      (should (equal (ltest--geom child) '(1 0 2 10))))))

;; ── 多行换行 ──
(ert-deftest test-flex-wrap-basic ()
  "flex-wrap: wrap 应换行，且不拉伸行高。"
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :width 20 :flex-wrap wrap :gap 0
                     :align-items flex-start :align-content flex-start
                     (text :width 10 :height 1 "a")
                     (text :width 10 :height 1 "b")
                     (text :width 10 :height 1 "c")))))
    (ltest--layout-root root 20 24)
    (let ((a (nth 0 (vnode-element-children root)))
          (b (nth 1 (vnode-element-children root)))
          (c (nth 2 (vnode-element-children root))))
      (should (equal (ltest--geom a) '(1 0 2 10)))
      (should (equal (ltest--geom b) '(1 10 2 20)))
      (should (equal (ltest--geom c) '(2 0 3 10))))))

;; ── 绝对定位 ──
(ert-deftest test-flex-absolute-not-in-flow ()
  ;; 添加 :position relative 使容器成为包含块
  (let ((root (vnode-element-desc-to-element
               '(row :display flex :flex-direction row :width 40 :height 2 :align-items flex-start :position relative
                     (text :width 10 "a")
                     (text :position absolute :left 5 :top 1 :width 5 :height 1 "abs")
                     (text :width 10 "b")))))
    (ltest--layout-root root 80 24)
    (let ((a (nth 0 (vnode-element-children root)))
          (abs (nth 1 (vnode-element-children root)))
          (b (nth 2 (vnode-element-children root))))
      (should (equal (ltest--geom a) '(1 0 2 10)))
      (should (equal (ltest--geom b) '(1 10 2 20)))
      ;; abs 相对于容器内容区 (1,0) left=5, top=1 => (2,5)-(3,10)
      (should (equal (ltest--geom abs) '(2 5 3 10))))))

(provide 'vnode-element-layout-flex-tests)
;;; vnode-element-layout-flex-tests.el ends here
