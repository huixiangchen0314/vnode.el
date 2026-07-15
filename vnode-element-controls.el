;;; vnode-element-controls.el --- 内置控件注册（text, button 等）  -*- lexical-binding: t; -*-

(require 'vnode-element)
(require 'vnode-element-style)
(require 'vnode-element-measure)          ; 为了 vnode-element--set-resolved! 等
(require 'vnode-element-registry)
(require 'vnode-element-render)           ; 为了使用其渲染辅助函数

;; ═══════════════════════════════════════════════════════════════════
;; 叶子节点布局实现
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-layout-leaf (node available-cols available-rows start-row start-col)
  "对叶子节点进行布局，直接使用父容器分配的尺寸。"
  (let* ((min-w (or (vnode-element-style-min-width node) 0))
         (max-w (or (vnode-element-style-max-width node) most-positive-fixnum))
         (min-h (or (vnode-element-style-min-height node) 0))
         (max-h (or (vnode-element-style-max-height node) most-positive-fixnum))
         (final-w (max min-w (min available-cols max-w)))
         (final-h (max min-h (min available-rows max-h))))
    (vnode-element--set-resolved! node :width final-w)
    (vnode-element--set-resolved! node :height final-h)
    (setf (vnode-element-start-row node) start-row
          (vnode-element-start-col node) start-col
          (vnode-element-end-row node) (+ start-row final-h)
          (vnode-element-end-col node) (+ start-col final-w))
    (list (+ start-row final-h) (+ start-col final-w))))

;; ═══════════════════════════════════════════════════════════════════
;; text 渲染函数
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-controls--render-text (node)
  "渲染文本节点：按坐标插入内容，支持对齐和样式。"
  (let* ((str (if (vnode-element-children node)
                  (car (vnode-element-children node))
                ""))
         (lines (split-string str "\n" t))
         (start-row (vnode-element-start-row node))
         (start-col (vnode-element-start-col node))
         (end-col (vnode-element-end-col node))
         (avail-width (- end-col start-col))
         (align (vnode-element-style-text-align node))
         (face (vnode-element-render--build-face node)))
    (cl-loop for line in lines
             for row from start-row
             do (let* ((line-width (string-width line))
                       (left-pad 0))
                  (when (> avail-width line-width)
                    (cond
                     ((eq align 'center)
                      (setq left-pad (floor (- avail-width line-width) 2)))
                     ((eq align 'right)
                      (setq left-pad (- avail-width line-width)))))
                  (let ((padded-line (concat (make-string left-pad ?\s) line)))
                    (vnode-element-render--insert-at row start-col padded-line face))))))

;; ═══════════════════════════════════════════════════════════════════
;; button 渲染函数
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-controls--render-button (node)
  "渲染按钮：插入带 keymap 的标签文本。"
  (let* ((label (or (plist-get (vnode-element-props node) :label) "Button"))
         (event (plist-get (vnode-element-props node) :on-click))
         (id (vnode-element-id node))
         (start-row (vnode-element-start-row node))
         (start-col (vnode-element-start-col node))
         (end-col (vnode-element-end-col node))
         (avail-width (- end-col start-col))
         (face (vnode-element-render--build-face node))
         (str (format " %s " label))
         (str-width (string-width str))
         (align (vnode-element-style-text-align node)))
    (when (and (> avail-width str-width) (eq align 'center))
      (setq start-col (+ start-col (floor (- avail-width str-width) 2))))
    (vnode-element-render--insert-at start-row start-col str face)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- start-row))
      (move-to-column start-col)
      (let ((start (point)))
        (forward-char str-width)
        (let ((callback
               (cond ((functionp event) event)
                     (event (lambda () (interactive)
                              (message "Button %s clicked (event %S not wired)" id event)))
                     (t nil))))
          (when callback
            (let ((map (make-sparse-keymap)))
              (define-key map (kbd "RET") callback)
              (define-key map [mouse-1] callback)
              (put-text-property start (point) 'keymap map))))
        (put-text-property start (point) 'mouse-face '(:background "#888"))))))

;; ═══════════════════════════════════════════════════════════════════
;; 注册
;; ═══════════════════════════════════════════════════════════════════

(vnode-element-register-layout 'text #'vnode-element-layout-leaf)
(vnode-element-register-layout 'button #'vnode-element-layout-leaf)

(vnode-element-register-renderer 'text #'vnode-element-controls--render-text)
(vnode-element-register-renderer 'button #'vnode-element-controls--render-button)

(provide 'vnode-element-controls)
;;; vnode-element-controls.el ends here
