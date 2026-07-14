;;; vnode-element-registry.el --- vnode 布局器注册表  -*- lexical-binding: t; -*-

(require 'vnode-element)
(require 'cl-lib)

;; ═══════════════════════════════════════════════════════════════════
;; 注册表存储
;; ═══════════════════════════════════════════════════════════════════

(defvar vnode-element--layout-registry (make-hash-table :test 'eq)
  "哈希表，键为节点类型符号，值为布局函数。
布局函数的签名: (node available-cols available-rows start-row start-col) -> (end-row end-col)。")

;; ═══════════════════════════════════════════════════════════════════
;; 注册与查找
;; ═══════════════════════════════════════════════════════════════════

(defun vnode-element-register-layout (type layout-fn)
  "将节点类型 TYPE 的布局函数注册为 LAYOUT-FN。
LAYOUT-FN 应接受 (node available-cols available-rows start-row start-col) 并返回 (end-row end-col)。"
  (puthash type layout-fn vnode-element--layout-registry))

(defun vnode-element--find-layout (type)
  "返回节点类型 TYPE 的布局函数，若未注册则返回 nil。"
  (gethash type vnode-element--layout-registry))

(defun vnode-element--has-layout-p (type)
  "判断节点类型 TYPE 是否已注册布局函数。"
  (not (null (vnode-element--find-layout type))))

;; ═══════════════════════════════════════════════════════════════════
;; 预置叶子节点的布局（可作为默认注册）
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

;; 默认注册 text 和 button 为叶子节点
(vnode-element-register-layout 'text #'vnode-element-layout-leaf)
(vnode-element-register-layout 'button #'vnode-element-layout-leaf)

(provide 'vnode-element-registry)
;;; vnode-element-registry.el ends here
