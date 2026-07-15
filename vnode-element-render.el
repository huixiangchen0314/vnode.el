;;; vnode-element-render.el --- 通用渲染引擎（不注册任何具体控件）  -*- lexical-binding: t; -*-

(require 'vnode-element)
(require 'vnode-element-style)
(require 'vnode-element-registry)
(require 'cl-lib)

;; ═══════════════════════════════════════════════════════════════════
;; 内部工具：样式 → face
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-render--build-face (node)
  "根据 NODE 的视觉样式构建 Emacs face 属性 plist。"
  (let* ((face (vnode-element-style-face node))
         (color (vnode-element-style-color node))
         (weight (vnode-element-style-font-weight node))
         (slant (vnode-element-style-font-style node))
         (size (vnode-element-style-font-size node))
         (family (vnode-element-style-font-family node))
         (decoration (vnode-element-style-text-decoration node))
         (extra '()))
    (when color   (push (cons :foreground color) extra))
    (when weight  (push (cons :weight (if (eq weight 'normal) 'normal weight)) extra))
    (when slant   (push (cons :slant (if (eq slant 'normal) 'normal slant)) extra))
    (when size    (push (cons :height size) extra))
    (when family  (push (cons :family family) extra))
    (when decoration
      (dolist (d (if (listp decoration) decoration (list decoration)))
        (cond ((eq d 'underline)    (push '(:underline t) extra))
              ((eq d 'line-through) (push '(:strike-through t) extra))
              ((eq d 'overline)     (push '(:overline t) extra)))))
    (cond
     ((null face) extra)
     ((symbolp face) `(:inherit ,face ,@extra))
     (t (append face extra)))))

;; ═══════════════════════════════════════════════════════════════════
;; 基础插入函数
;; ═══════════════════════════════════════════════════════════════════

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

(defun vnode-element-render--point-at (row col)
  "返回第 ROW 行第 COL 列的缓冲区位置。"
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- row))
    (move-to-column col)
    (point)))

;; ═══════════════════════════════════════════════════════════════════
;; 默认渲染函数（容器：递归子节点 + 装饰）
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-render--default (node)
  "默认渲染：递归渲染所有子节点，然后为容器添加背景/边框。"
  (dolist (child (vnode-element-children node))
    (when (vnode-element-p child)
      (vnode-element-render--node child)))
  (vnode-element-render--container-decoration node))

;; ═══════════════════════════════════════════════════════════════════
;; 容器装饰（背景、边框）
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-render--container-decoration (node)
  "为容器节点创建 overlay，用于背景色和边框。"
  (let ((bg (vnode-element-style-background node))
        (bw (vnode-element-style-border-width node :top))
        (bc (vnode-element-style-border-color node))
        (start-pos (vnode-element-render--point-at
                    (vnode-element-start-row node)
                    (vnode-element-start-col node)))
        (end-pos (vnode-element-render--point-at
                  (vnode-element-end-row node)
                  0)))
    (when (and start-pos end-pos)
      (let ((ov (make-overlay start-pos end-pos))
            (face-props '()))
        (when bg (push (cons :background bg) face-props))
        (when (and (> bw 0) bc)
          (push (cons :box `(:line-width ,bw :color ,bc)) face-props)
          (unless bg (push (cons :background nil) face-props)))
        (when face-props
          (overlay-put ov 'face face-props))
        (overlay-put ov 'vnode node)))))

;; ═══════════════════════════════════════════════════════════════════
;; 分派函数
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-render--node (node)
  "根据节点类型分派渲染函数。"
  (unless (and (vnode-element-p node)
               (vnode-element-style-visible node))
    (cl-return-from vnode-element-render--node))
  (let ((render-fn (or (vnode-element--find-renderer (vnode-element-type node))
                       #'vnode-element-render--default)))
    (funcall render-fn node)))

;; ═══════════════════════════════════════════════════════════════════
;; 主入口
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-render (root buffer)
  "将 ROOT 节点及其子树渲染到 BUFFER 中。"
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-char ?\n (max 1 (vnode-element-end-row root)))
      (goto-char (point-min))
      (vnode-element-render--node root))
    (goto-char (point-min))))

(provide 'vnode-element-render)
;;; vnode-element-render.el ends here
