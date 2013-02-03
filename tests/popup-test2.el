(require 'ert)

(require 'popup)
;; for "every" function
(require 'cl)

(when (< (frame-width) (length "long long long long line"))
  (set-frame-size (selected-frame) 80 35))

(defun popup-test-helper-buffer-contents ()
    (loop with start = (point-min)
          with contents = nil
          for overlay in (sort* (overlays-in (point-min) (point-max))
                                '< :key 'overlay-start)
          for overlay-start = (overlay-start overlay)
          for overlay-end = (overlay-end overlay)
          for prefix = (buffer-substring start overlay-start)
          for befstr = (overlay-get overlay 'before-string)
          for substr = (or (overlay-get overlay 'display)
                           (buffer-substring overlay-start overlay-end))
          for aftstr = (overlay-get overlay 'after-string)
          collect prefix into contents
          unless (overlay-get overlay 'invisible) collect
          (concat befstr substr aftstr) into contents
          do (setq start overlay-end)
          finally (return (concat (apply 'concat contents)
                                  (buffer-substring start (point-max))))))

(defun popup-test-helper-line-move-visual (arg)
  "This function is workaround. Because `line-move-visual' can not work well in
batch mode."
  (let ((cur-col
         (- (current-column)
            (save-excursion (vertical-motion 0) (current-column)))))
    (vertical-motion arg)
    (move-to-column (+ (current-column) cur-col))))

(defun popup-test-helper-rectangle-match (str)
  (goto-char (point-max))
  (let ((strings (split-string str "\n")))
    (search-backward (car strings) nil t)
    (every
     'identity
     (mapcar
      (lambda (elem)
        (let ((temporary-goal-column (or temporary-goal-column
                                         (current-column))))
          (line-move-visual 1 t))
        (looking-at (regexp-quote elem)))
      (cdr strings)))))

(defmacro popup-test-with-common-setup (&rest body)
  (declare (indent 0) (debug t))
  `(save-excursion
     (get-buffer-create "*dump*")
     (switch-to-buffer (get-buffer-create "*popup-test*"))
     ,@body
     (switch-to-buffer "*popup-test*")
     (remove-overlays)
     (erase-buffer)
     (switch-to-buffer "*dump*")
     (remove-overlays)
     (erase-buffer)
     )
  )

(defun popup-test-helper-create-popup ()
  (setq popup (popup-create (point) 10 10))
  (popup-set-list popup '("foo" "bar" "baz"))
  (popup-draw popup))

(defun popup-test-helper-popup-exist-p (str)
  (let ((buffer-contents (popup-test-helper-buffer-contents)))
    (switch-to-buffer "*dump*")
    (insert buffer-contents)
    (popup-test-helper-rectangle-match str)
    ))

(ert-deftest popup-test-simple ()
  (popup-test-with-common-setup
   (popup-test-helper-create-popup)
   (should (popup-test-helper-popup-exist-p "\
foo
bar
baz"))
   (should (eq (current-column) 0))))

(ert-deftest popup-test-delete ()
  (popup-test-with-common-setup
   (popup-test-helper-create-popup)
   (popup-delete popup)
   (should-not (popup-test-helper-popup-exist-p "\
foo
bar
baz"))
   ))

(ert-deftest popup-test-hide ()
  (popup-test-with-common-setup
    (popup-test-helper-create-popup)
    (popup-hide popup)
    (should-not (popup-test-helper-popup-exist-p "\
foo
bar
baz"))
    ))

(ert-deftest popup-test-check-column ()
  (popup-test-with-common-setup
    (insert " ")
    (popup-test-helper-create-popup)
    (should (popup-test-helper-popup-exist-p "\
foo
bar
baz"))
    (should (eq (current-column) 1))
    ))
