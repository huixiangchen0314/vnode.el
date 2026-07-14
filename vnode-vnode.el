


;; 虚拟节点表示
(require 'cl-lib)

(cl-defstruct (vnode (:constructor nil)
                     (:constructor vnode-create
                       (type &key id key props children element hooks)))
  "虚拟 DOM 节点。"
  id          ; 唯一标识，默认 nil
  type        ; 标签，如 row, col, text, button
  key         ; 稳定 key，用于 diff，默认取 id
  props       ; 属性 plist，如 (:label "OK" :on-click :my-event)
  children    ; 子节点列表，每个元素是 vnode 或字符串
  element     ; 关联的 DOM 元素（对 Emacs 而言可能是 buffer 中的 marker 或 overlay）
  hooks)      ; 生命周期钩子，使用 plist 存储，如 (:did-mount (lambda ...) :will-unmount ...)


(provide vnode-vnode.el)
