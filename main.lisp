(defun my-eval (expr env)
  (cond
   ((consp expr)
     (cond
      ((eq (first expr) 'quote)
        (second expr))
      ((eq (first expr) 'if)
        (if (my-eval (second expr) env)
            (my-eval (third expr) env)
            (my-eval (cadddr expr) env)))
      ((eq (first expr) 'lambda)
        (list
         ; tag
         'closure
         ; params
         (second expr)
         ; body
         (third expr)
         ; saved-env
         env))
      ; function call t=bool
      (t
        (let ((fn (my-eval (first expr) env))
              (args (mapcar (lambda (e) (my-eval e env)) (cdr expr))))
          (my-apply fn args)))))
   ((numberp expr) expr)
   ((stringp expr) expr)
   ((symbolp expr)
     (cdr (assoc expr env)))
   (t "not implemented")))

(defun my-apply (fn args)
  (cond
   ;;closure
   ((and (consp fn) (eq (first fn) 'closure))
     (my-eval (third fn) (pairlis (second fn) args (cadddr fn))))

   (t (apply fn args))))
