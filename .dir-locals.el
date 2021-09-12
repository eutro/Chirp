((nil . ((eval . (unless (featurep 'chirp-mode)
                   (let ((found (locate-dominating-file 
                                 (or load-file-name
                                     (buffer-file-name)
                                     default-directory)
                                 "emacs/chirp-mode.el")))
                     (when found
                       (let ((load-path (cons (concat found "emacs") load-path)))
                         (require 'chirp-mode)))))))))
