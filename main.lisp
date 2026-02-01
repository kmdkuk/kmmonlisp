(defparameter *global-env* (list
                            (cons '+ #'+)
                            (cons '- #'-)
                            (cons 'list #'list)
                            (cons 'cons #'cons)
                            (cons 'car #'car)
                            (cons 'cdr #'cdr)))

(defun my-eval (expr env)
  (cond
   ((consp expr)
     (cond
      ((eq (first expr) 'define)
        (let ((var (second expr))
              (val (my-eval (third expr) env)))
          (push (cons var val) *global-env*); letを挟まないとpushの結果が返ってしまう。
          var)); ここは変数名を返す
      ((eq (first expr) 'quote)
        (second expr))
      ((eq (first expr) 'if)
        (if (my-eval (second expr) env)
            (my-eval (third expr) env)
            (my-eval (fourth expr) env)))
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
     (if (cdr (assoc expr env))
         (cdr (assoc expr env))
         (cdr (assoc expr *global-env*))))
   (t "not implemented")))

(defun my-apply (fn args)
  (cond
   ;;closure
   ((and (consp fn) (eq (first fn) 'closure))
     (my-eval (third fn) (pairlis (second fn) args (fourth fn))))

   (t (apply fn args))))
