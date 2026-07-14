;;; vnode-element.el — UI 树节点核心（对齐 Clojure 行为，含 resolved-style 和 parent）  -*- lexical-binding: t; -*-

(require 'cl-lib)

;; ═══════════════════════════════════════════════════════
;; 结构体定义
;; ═══════════════════════════════════════════════════════
(cl-defstruct (vnode-element
               (:constructor nil)
               (:constructor vnode-element--internal
                (type &key id key props children resolved-style hooks
                      start-row end-row start-col end-col overlay parent)))
  "UI 树中的一个节点。"
  id              ; 唯一标识
  type            ; 'row | 'col | 'text | 'button ...
  key             ; 稳定键，默认同 id
  props           ; 用户设置的原始属性 plist
  children        ; 子节点列表（可以是字符串或 vnode-element）
  resolved-style  ; 布局后确定的样式 plist（如 :width 30 :height 5），由布局引擎写入
  hooks           ; 生命周期钩子 plist（预留）
  start-row       ; 布局后：起始行 (1-based)
  end-row
  start-col       ; 布局后：起始列 (0-based)
  end-col
  overlay         ; 渲染后关联的 overlay
  parent)         ; 父节点（布局时设置）

;; ═══════════════════════════════════════════════════════
;; 公共构造函数
;; ═══════════════════════════════════════════════════════

(defun vnode-element-create-element (type &rest args)
  "创建一个新的 vnode-element 实例。
TYPE 为组件类型符号，如 'row。
ARGS 为关键字参数，支持 :id, :key, :props, :children, :resolved-style, :hooks, :parent 等。
字符串子节点会保持为字符串，不自动包装。"
  (let* ((id (or (plist-get args :id) (gensym "vnode-")))
         (key (or (plist-get args :key) id))
         (props (or (plist-get args :props) '()))
         (children (or (plist-get args :children) '()))
         (resolved-style (plist-get args :resolved-style))
         (hooks (plist-get args :hooks))
         (start-row (plist-get args :start-row))
         (end-row (plist-get args :end-row))
         (start-col (plist-get args :start-col))
         (end-col (plist-get args :end-col))
         (overlay (plist-get args :overlay))
         (parent (plist-get args :parent)))
    (vnode-element--internal type
                             :id id :key key :props props
                             :children children
                             :resolved-style resolved-style
                             :hooks hooks
                             :start-row start-row :end-row end-row
                             :start-col start-col :end-col end-col
                             :overlay overlay
                             :parent parent)))

;; ═══════════════════════════════════════════════════════
;; Hiccup → vnode-element 树（对齐 Clojure 行为）
;; ═══════════════════════════════════════════════════════
(defun vnode-element-desc-to-element (desc)
  "将 Hiccup 风格描述 DESC 转换为 vnode-element 树或字符串。
DESC 格式: (tag [attr-plist] children...)
- 若 DESC 为字符串，直接返回该字符串。
- 若 (car DESC) 本身是列表，则整个节点被视为匿名片段，返回一个列表，
  其中每一项由递归转换得到。
- 否则，创建指定类型的节点，属性从紧随 tag 后的 plist 解析，
  剩余元素作为子节点递归转换。子节点中的字符串保持原样。"
  (cond
   ((stringp desc) desc)           ; 直接返回字符串，不包装
   ((listp desc)
    (let ((tag (car desc)))
      (if (listp tag)
          ;; 匿名组件/片段：对整个列表递归转换，返回列表
          (mapcar #'vnode-element-desc-to-element desc)
        ;; 正常标签
        (let* ((rest (cdr desc))
               (attrs (when (and rest (keywordp (car rest)))
                        (let ((plist '()))
                          (while (and rest (keywordp (car rest)))
                            (push (pop rest) plist)
                            (when rest (push (pop rest) plist)))
                          (nreverse plist))))
               (children rest)
               ;; 递归子节点，并展平匿名片段返回的列表
               (child-nodes (cl-mapcan (lambda (child)
                                         (let ((result (vnode-element-desc-to-element child)))
                                           (if (listp result)
                                               result
                                             (list result))))
                                       children)))
          (vnode-element-create-element tag
                                        :id (or (plist-get attrs :id) (gensym "vnode-"))
                                        :key (or (plist-get attrs :key) (plist-get attrs :id))
                                        :props (if attrs
                                                   (cl-loop for (k v) on attrs by #'cddr
                                                            unless (eq k :key)
                                                            collect k collect v)
                                                 '())
                                        :children child-nodes)))))
   ;; 其他类型（数字、符号等）转换为字符串返回
   (t (format "%s" desc))))

(provide 'vnode-element)
;;; vnode-element.el ends here
