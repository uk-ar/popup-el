;; This is an example of how you could set up this file. This setup
;; requires a directory called util in the project root and that the
;; util directory contains the testing tools ert and espuds.

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq popup-el-root-path project-directory)
  (setq popup-el-util-path (expand-file-name "util" popup-el-root-path)))

(add-to-list 'load-path popup-el-root-path)
(add-to-list 'load-path (expand-file-name "espuds" popup-el-util-path))
(add-to-list 'load-path (expand-file-name "ert" popup-el-util-path))

(require 'popup)
(require 'espuds)
(require 'ert)


(Setup
 ;; Before anything has run
 (get-buffer-create "*popup-test*")
 (get-buffer-create "*dump*")
 )

(Before
 (switch-to-buffer "*popup-test*")
 ;; Before each scenario is run
 )

(After
 (switch-to-buffer "*popup-test*")
 (remove-overlays)
 (erase-buffer)
 (switch-to-buffer "*dump*")
 (remove-overlays)
 (erase-buffer)
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
