;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I create popup\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (something)
        (setq popup (popup-create (point) 10 10))
        (popup-set-list popup (split-string something "\n"))
        (popup-draw popup)
        ))

(When "^I delete popup$"
      (lambda ()
        (popup-delete popup)
        ))

(When "^I hide popup$"
      (lambda ()
        (popup-hide popup)
        ))

(defun popup-test-helper-buffer-contents ()
  (with-output-to-string
    (loop with start = (point-min)
          for overlay in (sort* (overlays-in (point-min) (point-max))
                                '< :key 'overlay-start)
          for overlay-start = (overlay-start overlay)
          for overlay-end = (overlay-end overlay)
          for prefix = (buffer-substring-no-properties start overlay-start)
          for befstr = (overlay-get overlay 'before-string)
          for substr = (or (overlay-get overlay 'display)
                           (buffer-substring-no-properties
                            overlay-start overlay-end))
          for aftstr = (overlay-get overlay 'after-string)
          do (princ prefix)
          unless (overlay-get overlay 'invisible) do
          (when befstr (princ befstr))
          (princ substr)
          (when aftstr (princ aftstr))
          do (setq start overlay-end)
          finally (princ (buffer-substring-no-properties start (point-max))))
    ))

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

(Then "^The popup should be\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (contents)
        (let ((buffer-contents (popup-test-helper-buffer-contents)))
          (switch-to-buffer "*dump*")
          (insert buffer-contents)
          (assert (eq t (popup-test-helper-rectangle-match contents)) nil
                  "buffer string:\n%s\n" (buffer-string)))
        ))

(Then "^The popup should not be\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (contents)
        (let ((buffer-contents (popup-test-helper-buffer-contents)))
          (switch-to-buffer "*dump*")
          (insert buffer-contents)
          (assert (eq nil (popup-test-helper-rectangle-match contents))))
        ))

(Then "^Column number should be \"\\([0-9]+\\)\"$"
      (lambda (column)
        (assert (eq (string-to-number column) (current-column)))
        ))
