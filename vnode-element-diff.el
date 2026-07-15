;;; vnode-element-diff.el --- VNode 差异协调器（简化版）  -*- lexical-binding: t; -*-

(require 'vnode-element)
(require 'vnode-element-style)
(require 'vnode-element-measure)
(require 'vnode-element-layout)
(require 'vnode-element-render)
(require 'cl-lib)

(defun vnode-element-diff (old-root new-root buffer)
  "比较 OLD-ROOT 和 NEW-ROOT，将差异应用到 BUFFER。"
  (unless (and (vnode-element-p old-root) (vnode-element-p new-root))
    (error "vnode-element-diff: roots must be vnode-element"))
  (with-current-buffer buffer
    (let ((needs-full-relayout (vnode-element-diff--needs-full-relayout old-root new-root)))
      (if needs-full-relayout
          (let ((avail-cols (- (vnode-element-end-col old-root)
                               (vnode-element-start-col old-root)))
                (avail-rows (- (vnode-element-end-row old-root)
                               (vnode-element-start-row old-root))))
            (when (<= avail-cols 0) (setq avail-cols (frame-width)))
            (when (<= avail-rows 0) (setq avail-rows (frame-height)))
            (vnode-element-layout new-root avail-cols avail-rows
                                  (vnode-element-start-row old-root)
                                  (vnode-element-start-col old-root))
            (vnode-element-render new-root buffer))
        (vnode-element-diff--patch-node old-root new-root))))
  new-root)

(defun vnode-element-diff--needs-full-relayout (old-node new-node)
  (or (not (eq (vnode-element-type old-node) (vnode-element-type new-node)))
      (vnode-element-diff--props-changed-p old-node new-node)
      (vnode-element-diff--children-changed-p old-node new-node)))

(defun vnode-element-diff--props-changed-p (old-node new-node)
  (let ((old-props (vnode-element-props old-node))
        (new-props (vnode-element-props new-node)))
    (cl-loop for prop in '(:width :height :min-width :max-width :min-height :max-height
                           :margin :padding :border-width :flex-grow :flex-shrink :flex-basis
                           :flex-direction :justify-content :align-items :align-content
                           :gap :position :left :right :top :bottom :display)
             thereis (not (equal (plist-get old-props prop) (plist-get new-props prop))))))

(defun vnode-element-diff--children-changed-p (old-node new-node)
  (let ((old-children (cl-remove-if-not #'vnode-element-p (vnode-element-children old-node)))
        (new-children (cl-remove-if-not #'vnode-element-p (vnode-element-children new-node))))
    (or (/= (length old-children) (length new-children))
        (cl-loop for old-child in old-children
                 for new-child in new-children
                 thereis (or (not (eq (vnode-element-type old-child)
                                      (vnode-element-type new-child)))
                             (not (equal (vnode-element-key old-child)
                                         (vnode-element-key new-child))))))))

(defun vnode-element-diff--patch-node (old-node new-node)
  (vnode-element-diff--update-visual-props old-node new-node)
  (when (eq (vnode-element-type new-node) 'text)
    (vnode-element-diff--update-text-content old-node new-node))
  (let ((old-children (vnode-element-children old-node))
        (new-children (vnode-element-children new-node)))
    (cl-loop for old-child in old-children
             for new-child in new-children
             when (vnode-element-p old-child)
             do (vnode-element-diff--patch-node old-child new-child))))

(defun vnode-element-diff--update-visual-props (_old-node new-node)
  (when-let ((ov (vnode-element-overlay new-node)))
    (let ((bg (vnode-element-style-background new-node))
          (bw (vnode-element-style-border-width new-node :top))
          (bc (vnode-element-style-border-color new-node)))
      (let ((face (overlay-get ov 'face)))
        (when bg (setq face (plist-put face :background bg)))
        (when (and (> bw 0) bc)
          (setq face (plist-put face :box `(:line-width ,bw :color ,bc))))
        (overlay-put ov 'face face))))
  (when (eq (vnode-element-type new-node) 'text)
    (let ((start (vnode-element-render--point-at (vnode-element-start-row new-node) (vnode-element-start-col new-node)))
          (end   (vnode-element-render--point-at (vnode-element-end-row new-node) 0))
          (face  (vnode-element-render--build-face new-node)))
      (when (and start end)
        (put-text-property start end 'face face)))))

(defun vnode-element-diff--update-text-content (old-node new-node)
  (let ((old-str (car (vnode-element-children old-node)))
        (new-str (car (vnode-element-children new-node))))
    (when (and (stringp old-str) (stringp new-str) (not (string= old-str new-str)))
      (let ((old-lines (split-string old-str "\n"))
            (new-lines (split-string new-str "\n")))
        (when (= (length old-lines) (length new-lines))
          (let ((start (vnode-element-render--point-at (vnode-element-start-row new-node) (vnode-element-start-col new-node)))
                (end   (vnode-element-render--point-at (vnode-element-end-row new-node) 0)))
            (when (and start end)
              (delete-region start end)
              (goto-char start)
              (insert new-str)
              (let ((face (vnode-element-render--build-face new-node)))
                (put-text-property start (point) 'face face))
              (setf (vnode-element-children new-node) (list new-str)))))))))

(provide 'vnode-element-diff)
;;; vnode-element-diff.el ends here
