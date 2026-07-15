;;; vnode-element-render.el --- 将布局后的 vnode 树渲染到 Emacs buffer  -*- lexical-binding: t; -*-

(require 'vnode-element)
(require 'vnode-element-style)
(require 'cl-lib)

;; ═══════════════════════════════════════════════════════════════════
;; 主入口
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-render (root buffer)
  "将 ROOT 节点及其子树渲染到 BUFFER 中。
BUFFER 会被清空后重新填充。"
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; 预分配足够的行，避免反复检查
      (insert-char ?\n (vnode-element-end-row root))
      (goto-char (point-min))
      (vnode-element-render--node root))
    (goto-char (point-min))))

;; ═══════════════════════════════════════════════════════════════════
;; 内部分派
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-render--node (node)
  "根据节点类型分派渲染。"
  (cond
   ((not (vnode-element-p node)) nil)   ; 跳过字符串等非节点
   ((eq (vnode-element-type node) 'text)
    (vnode-element-render--text node))
   ((eq (vnode-element-type node) 'button)
    (vnode-element-render--button node))
   (t
    ;; 容器节点：先递归子节点，再添加自身装饰
    (dolist (child (vnode-element-children node))
      (vnode-element-render--node child))
    (vnode-element-render--container-decoration node))))

;; ═══════════════════════════════════════════════════════════════════
;; 文本节点
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-render--text (node)
  "将文本节点内容按坐标插入到 buffer 中。"
  (let* ((str (if (vnode-element-children node)
                  (car (vnode-element-children node))
                ""))
         (lines (split-string str "\n"))
         (start-row (vnode-element-start-row node))
         (start-col (vnode-element-start-col node))
         (face (vnode-element-style-face node)))
    (cl-loop for line in lines
             for i from 0
             do (vnode-element-render--insert-at start-row start-col line face)
             (cl-incf start-row))))

(defun vnode-element-render--insert-at (row col string &optional face)
  "在第 ROW 行第 COL 列插入 STRING，并可指定 FACE。"
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- row))
    (move-to-column col t)
    (let ((start (point)))
      (insert string)
      (when face
        (put-text-property start (point) 'face face)))))

;; ═══════════════════════════════════════════════════════════════════
;; 按钮节点
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-render--button (node)
  "渲染按钮：插入带 keymap 的标签文本。"
  (let* ((label (vnode-element-style--get node :label "Button"))
         (event (vnode-element-style--get node :on-click nil))
         (id (vnode-element-id node))
         (start-row (vnode-element-start-row node))
         (start-col (vnode-element-start-col node))
         (map (when event
                (let ((m (make-sparse-keymap)))
                  (define-key m (kbd "RET")
                    (lambda () (interactive)
                      (mrf-dispatch event id)))
                  (define-key m [mouse-1]
                    (lambda (e) (interactive "e")
                      (mrf-dispatch event id)))
                  m)))
         (str (format " %s " label)))
    (vnode-element-render--insert-at start-row start-col str)
    ;; 设置 keymap 和 face
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- start-row))
      (move-to-column start-col)
      (let ((start (point)))
        ;; 移动到插入文本的末尾
        (forward-char (length str))
        (when map
          (put-text-property start (point) 'keymap map))
        (put-text-property start (point) 'face '(:weight bold :background "#555" :foreground "white"))
        (put-text-property start (point) 'mouse-face '(:background "#888"))))))

;; ═══════════════════════════════════════════════════════════════════
;; 容器背景装饰（可扩展）
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-render--container-decoration (node)
  "为容器节点创建 overlay，用于背景色等。"
  (let ((bg (vnode-element-style-background node)))
    (when bg
      (let ((start-pos (vnode-element-render--point-at
                        (vnode-element-start-row node)
                        (vnode-element-start-col node)))
            (end-pos (vnode-element-render--point-at
                      (vnode-element-end-row node)
                      0)))
        (when (and start-pos end-pos)
          (let ((ov (make-overlay start-pos end-pos)))
            (overlay-put ov 'face `(:background ,bg))
            (overlay-put ov 'vnode node)))))))

(defun vnode-element-render--point-at (row col)
  "返回第 ROW 行第 COL 列的缓冲区位置。"
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- row))
    (move-to-column col)
    (point)))

(provide 'vnode-element-render)
;;; vnode-element-render.el ends here
