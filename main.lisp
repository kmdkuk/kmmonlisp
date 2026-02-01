(defparameter *global-env* (list
                            (cons '+ #'+)
                            (cons '- #'-)
                            (cons '* #'*)
                            (cons '= #'=)
                            (cons '< #'<)
                            (cons '> #'>)
                            (cons 'list #'list)
                            (cons 'cons #'cons)
                            (cons 'car #'car)
                            (cons 'cdr #'cdr)))

(defun my-eval (expr env)
  (cond
   ((consp expr)
     (cond
      ((eq (first expr) 'progn)
        (eval-progn (rest expr) env))
      ((eq (first expr) 'defun)
        (let ((name (second expr))
              (params (third expr))
              (body (cdddr expr)))
          (my-eval (list 'define name (append (list 'lambda params) body)) env)))
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
         (cddr expr)
         ; saved-env
         env))
      ; function call t=bool
      (t
        (let ((fn (my-eval (first expr) env))
              (args (mapcar (lambda (e) (my-eval e env)) (rest expr))))
          (my-apply fn args)))))
   ((numberp expr) expr)
   ((stringp expr) expr)
   ((symbolp expr)
     (if (rest (assoc expr env))
         (rest (assoc expr env))
         (rest (assoc expr *global-env*))))
   (t "not implemented")))

(defun my-apply (fn args)
  (cond
   ;;closure
   ((and (consp fn) (eq (first fn) 'closure))
     (let ((params (second fn))
           (body (third fn))
           (saved-env (fourth fn)))
       (eval-progn body (pairlis params args saved-env))))

   (t (apply fn args))))

(defun eval-progn (exprs env)
  (cond
   ((null exprs) nil)
   ((null (rest exprs))
     (my-eval (first exprs) env))
   (t
     (my-eval (first exprs) env)
     (eval-progn (rest exprs) env))))
