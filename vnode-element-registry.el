;;; vnode-element-registry.el --- vnode 布局/渲染统一注册表（纯净）  -*- lexical-binding: t; -*-

(require 'vnode-element)
(require 'cl-lib)

;; ═══════════════════════════════════════════════════════════════════
;; 布局注册表
;; ═══════════════════════════════════════════════════════════════════

(defvar vnode-element--layout-registry (make-hash-table :test 'eq)
  "哈希表，键为节点类型符号，值为布局函数。
布局函数的签名: (node available-cols available-rows start-row start-col) -> (end-row end-col)。")

(defun vnode-element-register-layout (type layout-fn)
  "将节点类型 TYPE 的布局函数注册为 LAYOUT-FN。"
  (puthash type layout-fn vnode-element--layout-registry))

(defun vnode-element--find-layout (type)
  "返回节点类型 TYPE 的布局函数，若未注册则返回 nil。"
  (gethash type vnode-element--layout-registry))

(defun vnode-element--has-layout-p (type)
  "判断节点类型 TYPE 是否已注册布局函数。"
  (not (null (vnode-element--find-layout type))))

;; ═══════════════════════════════════════════════════════════════════
;; 渲染注册表
;; ═══════════════════════════════════════════════════════════════════

(defvar vnode-element--render-registry (make-hash-table :test 'eq)
  "哈希表，键为节点类型符号，值为渲染函数。
渲染函数签名: (node) ，负责将节点绘制到当前 buffer。")

(defun vnode-element-register-renderer (type render-fn)
  "将节点类型 TYPE 的渲染函数注册为 RENDER-FN。"
  (puthash type render-fn vnode-element--render-registry))

(defun vnode-element--find-renderer (type)
  "返回节点类型 TYPE 的渲染函数，若未注册则返回 nil。"
  (gethash type vnode-element--render-registry))

(provide 'vnode-element-registry)
;;; vnode-element-registry.el ends here
