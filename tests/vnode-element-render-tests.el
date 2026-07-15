;;; vnode-element-render-tests.el --- 测试渲染引擎（修正期望）  -*- lexical-binding: t; -*-

(require 'ert)
(require 'vnode-element)
(require 'vnode-element-style)
(require 'vnode-element-measure)
(require 'vnode-element-layout)
(require 'vnode-element-render)
(require 'vnode-element-controls)
(require 'cl-lib)

;; ═══════════════════════════════════════════════════════════════════
;; 辅助函数
;; ═══════════════════════════════════════════════════════════════════

(defun rtest--render-to-temp-buffer (root &optional cols rows)
  "布局并渲染 ROOT 到一个临时 buffer，返回该 buffer。"
  (let ((buf (generate-new-buffer " *render-test*")))
    (vnode-element-layout root (or cols 80) (or rows 24) 1 0)
    (vnode-element-render root buf)
    buf))

(defun rtest--text-properties-at (root row col &optional cols rows)
  "返回渲染后第 ROW 行第 COL 列的文本属性 plist。"
  (let ((buf (rtest--render-to-temp-buffer root cols rows)))
    (unwind-protect
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- row))
            (move-to-column col)
            (text-properties-at (point))))
      (kill-buffer buf))))

;; ═══════════════════════════════════════════════════════════════════
;; 文本节点渲染测试
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-render-simple-text ()
  "简单文本应被渲染在正确位置。"
  (let ((root (vnode-element-desc-to-element
               '(row (text "hello")))))
    (let ((buf (rtest--render-to-temp-buffer root 80 1)))
      (with-current-buffer buf
        (should (string-match "hello" (buffer-string)))
        (goto-char (point-min))
        (should (looking-at "hello")))
      (kill-buffer buf))))

(ert-deftest test-render-text-multiline ()
  "多行文本应占用多行。"
  (let ((root (vnode-element-desc-to-element
               '(row (text "line1\nline2")))))
    (let ((buf (rtest--render-to-temp-buffer root 80 5)))
      (with-current-buffer buf
        (let ((lines (split-string (buffer-string) "\n")))
          (should (equal (nth 0 lines) "line1"))
          (should (equal (nth 1 lines) "line2"))))
      (kill-buffer buf))))

(ert-deftest test-render-text-align-right ()
  "右对齐文本。"
  (let ((root (vnode-element-desc-to-element
               '(row (text :text-align right :width 10 "hi")))))
    (let ((buf (rtest--render-to-temp-buffer root 80 2)))
      (with-current-buffer buf
        (goto-char (point-min))
        ;; 8 个空格 + "hi"
        (should (looking-at "        hi")))
      (kill-buffer buf))))

(ert-deftest test-render-text-align-center ()
  "居中对齐。"
  (let ((root (vnode-element-desc-to-element
               '(row (text :text-align center :width 10 "hi")))))
    (let ((buf (rtest--render-to-temp-buffer root 80 2)))
      (with-current-buffer buf
        (goto-char (point-min))
        ;; 4 个空格 + "hi" + 4 个空格
        (should (looking-at "    hi    ")))
      (kill-buffer buf))))

(ert-deftest test-render-text-face ()
  "文本应继承指定的 face（符号）。"
  (let ((root (vnode-element-desc-to-element
               '(row (text :face bold "bold")))))
    (let ((props (rtest--text-properties-at root 1 0 80 2)))
      ;; 当只有 face 符号时，应该直接为 bold
      (should (eq (plist-get props 'face) 'bold))
      (kill-buffer (rtest--render-to-temp-buffer root 80 2)))))

(ert-deftest test-render-text-color ()
  "通过 :color 设置前景色。"
  (let ((root (vnode-element-desc-to-element
               '(row (text :color "red" "red text")))))
    (let ((props (rtest--text-properties-at root 1 0 80 2)))
      (let ((face (plist-get props 'face)))
        ;; face 是 plist，包含 :foreground
        (should (plist-get face :foreground))
        (should (equal (plist-get face :foreground) "red")))
      (kill-buffer (rtest--render-to-temp-buffer root 80 2)))))

;; ═══════════════════════════════════════════════════════════════════
;; 按钮节点渲染测试
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-render-button-label ()
  "按钮应显示标签文本。"
  (let ((root (vnode-element-desc-to-element
               '(row (button :label "OK")))))
    (let ((buf (rtest--render-to-temp-buffer root 80 2)))
      (with-current-buffer buf
        (should (string-match " OK " (buffer-string))))
      (kill-buffer buf))))

(ert-deftest test-render-button-keymap ()
  "按钮应设置 keymap 属性。"
  (let ((root (vnode-element-desc-to-element
               '(row (button :label "OK" :on-click :my-event)))))
    (let ((props (rtest--text-properties-at root 1 1 80 2)))
      (let ((map (plist-get props 'keymap)))
        (should (keymapp map))
        (should (lookup-key map (kbd "RET")))
        (should (lookup-key map [mouse-1])))
      (kill-buffer (rtest--render-to-temp-buffer root 80 2)))))

(ert-deftest test-render-button-mouse-face ()
  "按钮应设置 mouse-face 属性。"
  (let ((root (vnode-element-desc-to-element
               '(row (button :label "OK")))))
    (let ((props (rtest--text-properties-at root 1 1 80 2)))
      (should (plist-get props 'mouse-face))
      (kill-buffer (rtest--render-to-temp-buffer root 80 2)))))

;; ═══════════════════════════════════════════════════════════════════
;; 容器装饰测试
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-render-container-background ()
  "容器背景应通过 overlay 设置。"
  (let ((root (vnode-element-desc-to-element
               '(row :background "blue" :width 10 :height 1 (text "a")))))
    (let ((buf (rtest--render-to-temp-buffer root 80 2)))
      (with-current-buffer buf
        (let* ((ovs (overlays-in (point-min) (point-max)))
               (bg-ov (cl-find-if (lambda (ov) (overlay-get ov 'vnode)) ovs)))
          (should bg-ov)
          (let ((face (overlay-get bg-ov 'face)))
            (should (plist-get face :background))
            (should (equal (plist-get face :background) "blue")))))
      (kill-buffer buf))))

(ert-deftest test-render-container-border ()
  "容器边框应通过 overlay 的 :box 属性设置。"
  (let ((root (vnode-element-desc-to-element
               '(row :border-width 1 :border-color "red" :width 10 :height 1 (text "a")))))
    (let ((buf (rtest--render-to-temp-buffer root 80 2)))
      (with-current-buffer buf
        (let* ((ovs (overlays-in (point-min) (point-max)))
               (box-ov (cl-find-if (lambda (ov) (overlay-get ov 'vnode)) ovs)))
          (should box-ov)
          (let ((box (plist-get (overlay-get box-ov 'face) :box)))
            (should box)
            (should (equal (plist-get box :line-width) 1))
            (should (equal (plist-get box :color) "red")))))
      (kill-buffer buf))))

;; ═══════════════════════════════════════════════════════════════════
;; 可见性控制
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-render-visible-false ()
  "不可见节点不应出现在 buffer 中。"
  (let ((root (vnode-element-desc-to-element
               '(row (text :visible nil "hidden") (text "visible")))))
    (let ((buf (rtest--render-to-temp-buffer root 80 2)))
      (with-current-buffer buf
        (should-not (string-match "hidden" (buffer-string)))
        (should (string-match "visible" (buffer-string))))
      (kill-buffer buf))))

;; ═══════════════════════════════════════════════════════════════════
;; 空节点与边界情况
;; ═══════════════════════════════════════════════════════════════════

(ert-deftest test-render-empty-container ()
  "空容器不应报错。"
  (let ((root (vnode-element-desc-to-element
               '(row :width 10 :height 1))))
    (let ((buf (rtest--render-to-temp-buffer root 80 2)))
      (should (bufferp buf))
      (kill-buffer buf))))

(ert-deftest test-render-nested-containers ()
  "嵌套容器应正确渲染子节点。"
  (let ((root (vnode-element-desc-to-element
               '(row (row (text "nested"))))))
    (let ((buf (rtest--render-to-temp-buffer root 80 2)))
      (with-current-buffer buf
        (should (string-match "nested" (buffer-string))))
      (kill-buffer buf))))

(provide 'vnode-element-render-tests)
;;; vnode-element-render-tests.el ends here
