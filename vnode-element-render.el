;;; vnode-element-render.el --- 通用渲染引擎（修复对齐与 face）  -*- lexical-binding: t; -*-

(require 'vnode-element)
(require 'vnode-element-style)
(require 'vnode-element-registry)
(require 'cl-lib)

;; ═══════════════════════════════════════════════════════════════════
;; 内部工具：样式 → face
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-render--build-face (node)
  "根据 NODE 的视觉样式构建 Emacs face 属性 plist。
优先使用 :face，细粒度样式会覆盖或补充 :face 中的对应属性。"
  (let* ((face (vnode-element-style-face node))
         (color (vnode-element-style-color node))
         (weight (vnode-element-style-font-weight node))
         (slant (vnode-element-style-font-style node))
         (size (vnode-element-style-font-size node))
         (family (vnode-element-style-font-family node))
         (decoration (vnode-element-style-text-decoration node))
         (extra '()))
    ;; 只在值有意义（非默认且非 nil）时才写入 plist
    (when color   (setq extra (plist-put extra :foreground color)))
    (when (and weight (not (eq weight 'normal)))
      (setq extra (plist-put extra :weight weight)))
    (when (and slant (not (eq slant 'normal)))
      (setq extra (plist-put extra :slant slant)))
    (when size    (setq extra (plist-put extra :height size)))
    (when family  (setq extra (plist-put extra :family family)))
    (when decoration
      (dolist (d (if (listp decoration) decoration (list decoration)))
        (cond ((eq d 'underline)    (setq extra (plist-put extra :underline t)))
              ((eq d 'line-through) (setq extra (plist-put extra :strike-through t)))
              ((eq d 'overline)     (setq extra (plist-put extra :overline t))))))
    ;; 组合 face
    (cond
     ;; 只有 face 符号，且没有任何额外属性 → 直接返回符号
     ((and (null extra) (symbolp face)) face)
     ;; face 是符号，但有额外属性 → (:inherit face ...)
     ((symbolp face) `(:inherit ,face ,@extra))
     ;; face 本身已经是 plist，或为 nil
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
            (face-plist '()))
        (when bg (setq face-plist (plist-put face-plist :background bg)))
        (when (and (> bw 0) bc)
          (setq face-plist (plist-put face-plist :box `(:line-width ,bw :color ,bc)))
          (unless bg (setq face-plist (plist-put face-plist :background nil))))
        (when face-plist
          (overlay-put ov 'face face-plist))
        (overlay-put ov 'vnode node)))))

;; ═══════════════════════════════════════════════════════════════════
;; 分派函数
;; ═══════════════════════════════════════════════════════════════════

(cl-defun vnode-element-render--node (node)
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
