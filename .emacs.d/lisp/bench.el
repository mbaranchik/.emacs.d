;;; -*- lexical-binding: t -*-

(defmacro bench (name &rest code)
  `(cond ((string= "y" (getenv "EMACS_BENCHMARK"))
          (progn
             (message "Benchmark for '%s':" ,name)
             (benchmark-progn
               ,@code
               )
             ))
         (t (progn
               ,@code
               ))
         )
  )

;;(macroexpand '(bench "tmp"
;;                     (setq sdakjds "sda")
;;                     (setq sdakjds "sda")))

(provide 'bench)
