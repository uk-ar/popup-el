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
  (let ((buffer-contents (popup-test-helper-buffer-contents)))
    (with-temp-buffer
      (insert buffer-contents)
      (goto-char (point-min))
      (let ((strings (split-string str "\n")))
        (when (search-forward (car strings) nil t)
          (goto-char (match-beginning 0))
          (every
           'identity
           (mapcar
            (lambda (elem)
              (popup-test-helper-line-move-visual 1)
              (looking-at (regexp-quote elem)))
            (cdr strings)))
          )
        ))))

(defmacro popup-test-with-common-setup (&rest body)
  (declare (indent 0) (debug t))
  `(save-excursion
     (with-temp-buffer
       (switch-to-buffer (current-buffer))
       (delete-other-windows)
       (erase-buffer)
       ,@body
       )
     ))

(defun popup-test-helper-create-popup (str)
  (setq popup (popup-create (point) 10 10))
  (popup-set-list popup (split-string str "\n"))
  (popup-draw popup))

(defun popup-test-helper-in-popup-p ()
  (let* ((faces (get-text-property (point) 'face))
         (faces (if (listp faces) faces (list faces))))
    (or (memq 'popup-tip-face faces)
        (memq 'popup-face faces))))

(defun popup-test-helper-popup-start-line ()
  (let ((buffer-contents (popup-test-helper-buffer-contents)))
    (with-temp-buffer
      (insert buffer-contents)
      (goto-char (point-min))
      (let ((end (point)))
        (while (and (not (eobp))
                    (not (popup-test-helper-in-popup-p)))
          (goto-char (or (next-single-property-change (point) 'face)
                         (point-max))))
        (if (popup-test-helper-in-popup-p)
            ;; todo visual line
            (line-number-at-pos (point)) nil)
        ))))

(defun popup-test-helper-popup-start-column ()
  (let ((buffer-contents (popup-test-helper-buffer-contents)))
    (with-temp-buffer
      (insert buffer-contents)
      (goto-char (point-min))
      (let ((end (point)))
        (while (and (not (eobp))
                    (not (popup-test-helper-in-popup-p)))
          (goto-char (or (next-single-property-change (point) 'face)
                         (point-max))))
        (if (popup-test-helper-in-popup-p)
            (current-column) nil)
        ))))

(defun popup-test-helper-popup-end-line ()
  (let ((buffer-contents (popup-test-helper-buffer-contents)))
    (with-temp-buffer
      (insert buffer-contents)
      (goto-char (point-max))
      (let ((end (point)))
        (while (and (not (bobp))
                    (not (popup-test-helper-in-popup-p)))
          (goto-char (or (previous-single-property-change (point) 'face)
                         (point-min))))
        (if (popup-test-helper-in-popup-p)
            ;; todo visual line
            (line-number-at-pos (point)) nil)
        ))))

(defun popup-test-helper-debug ()
  (let ((buffer-contents (popup-test-helper-buffer-contents)))
    (with-current-buffer (get-buffer-create "*dump*")
      (insert buffer-contents)
      (buffer-string)
      )))
;; Test for popup-el
(ert-deftest popup-test-simple ()
  (popup-test-with-common-setup
    (popup-test-helper-create-popup "\
foo
bar
baz")
    (should (popup-test-helper-rectangle-match "\
foo
bar
baz"))
    (should (eq (popup-test-helper-popup-start-column) 0))))

(ert-deftest popup-test-delete ()
  (popup-test-with-common-setup
    (popup-test-helper-create-popup "\
foo
bar
baz")
    (popup-delete popup)
    (should-not (popup-test-helper-rectangle-match "\
foo
bar
baz"))
    ))

(ert-deftest popup-test-hide ()
  (popup-test-with-common-setup
    (popup-test-helper-create-popup "\
foo
bar
baz")
    (popup-hide popup)
    (should-not (popup-test-helper-rectangle-match "\
foo
bar
baz"))
    ))

(ert-deftest popup-test-at-colum1 ()
  (popup-test-with-common-setup
    (insert " ")
    (popup-test-helper-create-popup "\
foo
bar
baz")
    (should (popup-test-helper-rectangle-match "\
foo
bar
baz"))
    (should (eq (popup-test-helper-popup-start-column) 1))
    ))

(ert-deftest popup-test-tip ()
  (popup-test-with-common-setup
    (popup-tip "\
Start isearch on POPUP. This function is synchronized, meaning
event loop waits for quiting of isearch.

CURSOR-COLOR is a cursor color during isearch. The default value
is `popup-isearch-cursor-color'.

KEYMAP is a keymap which is used when processing events during
event loop. The default value is `popup-isearch-keymap'.

CALLBACK is a function taking one argument. `popup-isearch' calls
CALLBACK, if specified, after isearch finished or isearch
canceled. The arguments is whole filtered list of items.

HELP-DELAY is a delay of displaying helps."
               :nowait t)
    (should (popup-test-helper-rectangle-match "\
KEYMAP is a keymap which is used when processing events during
event loop. The default value is `popup-isearch-keymap'."))
    ))

(ert-deftest popup-test-folding-long-line-right-top ()
  (popup-test-with-common-setup
    ;; To use window-width because Emacs 23 does not have window-body-width
    (insert (make-string (- (window-width) 3) ? ))
    (popup-tip "long long long long line" :nowait t)
    (should (popup-test-helper-rectangle-match "long long long long line"))
    (should (eq (popup-test-helper-popup-start-line)
                2))
    (should (eq (popup-test-helper-popup-end-line) 2))
    ))

(ert-deftest popup-test-folding-long-line-left-bottom ()
  (popup-test-with-common-setup
    (insert (make-string (- (window-body-height) 1) ?\n))
    (popup-tip "long long long long line" :nowait t)
    (should (popup-test-helper-rectangle-match "long long long long line"))
    (should (eq (popup-test-helper-popup-start-line)
                (- (window-body-height) 1)))
    (should (eq (popup-test-helper-popup-end-line) (- (window-body-height) 1)))
    ))

(ert-deftest popup-test-folding-long-line-right-bottom ()
  (popup-test-with-common-setup
   (insert (make-string (- (window-body-height) 1) ?\n))
   (insert (make-string (- (window-width) 3) ? ))
   (popup-tip "long long long long line" :nowait t)
   (should (popup-test-helper-rectangle-match "long long long long line"))
   (should (eq (popup-test-helper-popup-start-line)
               (- (window-body-height) 1)))
   (should (eq (popup-test-helper-popup-end-line) (- (window-body-height) 1)))
   ))

(ert-deftest popup-test-folding-short-line-right-top ()
  (popup-test-with-common-setup
   (insert (make-string (- (window-width) 3) ? ))
   (popup-tip "bla\nbla\nbla\nbla\nbla" :nowait t)
   (should (popup-test-helper-rectangle-match "bla\nbla\nbla\nbla\nbla"))
   (should (eq (popup-test-helper-popup-start-line) 2))
   ))

(ert-deftest popup-test-folding-short-line-left-bottom ()
  (popup-test-with-common-setup
   (insert (make-string (- (window-body-height) 1) ?\n))
   (popup-tip "bla\nbla\nbla\nbla\nbla" :nowait t)
   (should (popup-test-helper-rectangle-match "bla\nbla\nbla\nbla\nbla"))
   (should (eq (popup-test-helper-popup-end-line) (- (window-body-height) 1)))))

(ert-deftest popup-test-folding-short-line-right-bottom ()
  (popup-test-with-common-setup
   (insert (make-string (- (window-body-height) 1) ?\n))
   (insert (make-string (- (window-width) 3) ? ))
   (popup-tip "bla\nbla\nbla\nbla\nbla" :nowait t)
   (should (popup-test-helper-rectangle-match "bla\nbla\nbla\nbla\nbla"))
   (should (eq (popup-test-helper-popup-end-line) (- (window-body-height) 1)))
   ))

(ert-deftest popup-test-margin-at-column1 ()
  (popup-test-with-common-setup
   (insert " ")
   (popup-tip "Margin?" :nowait t :margin t)
   (should (popup-test-helper-rectangle-match "Margin?"))
   ;; ToDo:fix
   ;; (should (eq (popup-test-helper-popup-start-column)
   ;;             1))
   ))

(ert-deftest popup-test-height-limit ()
  (popup-test-with-common-setup
     (popup-tip "Foo\nBar\nBaz" :nowait t :height 2)
     (should (popup-test-helper-rectangle-match "Foo\nBar\n"))
     (should-not (popup-test-helper-rectangle-match "Baz"))
     (should (eq (popup-test-helper-popup-start-line) 2))
     (should (eq (popup-test-helper-last-popup-line)  3))
     ))

(ert-deftest popup-test-height-limit-bottom ()
  (popup-test-with-common-setup
   (insert (make-string (- (window-body-height) 1) ?\n))
   (popup-tip "Foo\nBar\nBaz" :nowait t :height 2)
   (should (popup-test-helper-rectangle-match "Foo\nBar\n"))
   (should-not (popup-test-helper-rectangle-match "Baz"))
   (should (eq (popup-test-helper-popup-end-line) (- (window-body-height) 1)))
   ))

(ert-deftest popup-test-scroll-bar ()
  (popup-test-with-common-setup
   (let ((popup-scroll-bar-foreground-char
           (propertize "f" 'face 'popup-scroll-bar-foreground-face))
          (popup-scroll-bar-background-char
           (propertize "b" 'face 'popup-scroll-bar-background-face)))
     (popup-tip "Foo\nBar\nBaz\nFez\nOz"
                       :nowait t :height 3 :scroll-bar t :margin t)
     (should (popup-test-helper-rectangle-match "Foo f\nBar b\nBaz b"))
     (should-not (popup-test-helper-rectangle-match "Fez"))
     (should-not (popup-test-helper-rectangle-match "Oz"))
     (should (eq (popup-test-helper-popup-start-line) 2))
     (should (eq (popup-test-helper-last-popup-line)  4))
     )))

(ert-deftest popup-test-scroll-bar-right-no-margin ()
  (popup-test-with-common-setup
   (insert (make-string (- (window-width) 1) ? ))
     (let ((popup-scroll-bar-foreground-char
            (propertize "f" 'face 'popup-scroll-bar-foreground-face))
           (popup-scroll-bar-background-char
            (propertize "b" 'face 'popup-scroll-bar-background-face)))
       (popup-tip "Foo\nBar\nBaz\nFez\nOz"
                  :nowait t :height 3 :scroll-bar t)
       (should (popup-test-helper-rectangle-match "Foof\nBarb\nBazb"))
       (should-not (popup-test-helper-rectangle-match "Fez"))
       (should-not (popup-test-helper-rectangle-match "Oz"))
       (should (eq (popup-test-helper-popup-start-line) 2))
       (should (eq (popup-test-helper-last-popup-line)  4))
       ;; (should (eq (popup-test-helper-popup-start-column)
       ;;             (- (window-width) 5)))
       )))

(ert-deftest popup-test-min-height ()
  (popup-test-with-common-setup
   (insert (make-string (- (window-width) 1) ? ))
   (popup-tip "Hello" :nowait t :min-height 10)
   (should (popup-test-helper-rectangle-match "Hello"))
   (should (eq (popup-test-helper-popup-start-line) 2))
   (should (eq (popup-test-helper-last-popup-line) 11))
   ))

