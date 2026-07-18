(normal-top-level-add-to-load-path (list))
(when (boundp (quote native-comp-eln-load-path)) (let ((needle (expand-file-name "../../../lib/emacs/native-site-lisp"))) (setq native-comp-eln-load-path (mapcan (lambda (dir) (if (equal dir needle) (append (quote ()) (list dir)) (list dir))) native-comp-eln-load-path))))
