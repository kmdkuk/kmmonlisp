(declaim (ftype (function (t t) t) my-eval))

(defparameter *global-env* (list
                            (cons 't 't)
                            (cons 'nil nil)
                            (cons '+ #'+)
                            (cons '- #'-)
                            (cons '* #'*)
                            (cons '= #'=)
                            (cons '< #'<)
                            (cons '> #'>)
                            (cons 'list #'list)
                            (cons 'cons #'cons)
                            (cons 'eq #'eq)
                            (cons 'null #'null)
                            (cons 'not #'not)
                            (cons 'car #'car)
                            (cons 'cdr #'cdr)))

(defun lookup (symbol env)
  (let ((local-search (assoc symbol env)))
    (if local-search
        (cdr local-search)
        (cdr (assoc symbol *global-env*)))))

(defun bind-params (params args env)
  (cond
   ((null params) env)
   ((eq (first params) '&rest)
     (cons (cons (second params) args) env))
   (t
     (cons (cons (first params) (first args))
           (bind-params (rest params) (rest args) env)))))


(defun eval-progn (exprs env)
  (cond
   ((null exprs) nil)
   ((null (rest exprs))
     (my-eval (first exprs) env))
   (t
     (my-eval (first exprs) env)
     (eval-progn (rest exprs) env))))

; list内の式をmy-evalして結果をリストで返す(関数適用の引数処理)
(defun eval-list (exprs env)
  (mapcar (lambda (e) (my-eval e env)) exprs))

(defun apply-macro (macro-def args)
  (let ((params (second macro-def))
        (body (third macro-def))
        (saved-env (fourth macro-def)))
    (eval-progn body (bind-params params args saved-env))))

(defun my-apply (fn args)
  (cond
   ;;closure
   ((and (consp fn) (eq (first fn) 'closure))
     (let ((params (second fn))
           (body (third fn))
           (saved-env (fourth fn)))
       (eval-progn body (bind-params params args saved-env))))
   (t (apply fn args))))

(defun my-eval (expr env)
  (cond
   ((symbolp expr)
     (lookup expr env))

   ; number and strings and t/nil
   ((atom expr) expr)

   ; special forms

   ; (quote (hoge a b))と'(hoge a b)は同じ。糖衣構文
   ((eq (first expr) 'quote)
     (second expr))

   ((eq (first expr) 'if)
     (if (my-eval (second expr) env)
         (my-eval (third expr) env)
         (my-eval (fourth expr) env)))

   ((eq (first expr) 'progn)
     (eval-progn (rest expr) env))

   ((eq (first expr) 'define)
     (let ((var (second expr))
           (val (my-eval (third expr) env)))
       (push (cons var val) *global-env*); letを挟まないとpushの結果が返ってしまう。
       var)); ここは変数名を返す

   ((eq (first expr) 'defmacro)
     (let ((name (second expr))
           (params (third expr))
           (body (cdddr expr)))
       (push (cons name (list 'macro params body env)) *global-env*)
       name)); defineと同じようにmacro名を返す

   ((eq (first expr) 'defun)
     (let ((name (second expr))
           (params (third expr))
           (body (cdddr expr)))
       ; quoteはそのまま処理をしない
       ; backquoteは一部を処理させることが可能 fmt.printfみたいな感じ?,varが%sでvarを指定したみたいな感じ。
       ; ,@はbodyの外側のlistを外してばら撒く splicing 接ぎ木というらしい?
       ; same: (my-eval (list 'define name (append (list 'lambda params) body)) env)))
       (my-eval `(define ,name (lambda ,params ,@body)) env)))

   ((eq (first expr) 'lambda)
     (let ((params (second expr))
           (body (cddr expr)))
       (list 'closure params body env)))

   ((eq (first expr) 'macroexpand)
     ; expr (macroexpand '(unless t a b))
     ; (second expr) -> '(unless t a b)
     ; target-form -> (unless t a b)
     (let* ((target-form (my-eval (second expr) env))
            (macro-name (first target-form))
            (fn-def (lookup macro-name env)))
       (if (and (consp fn-def) (eq (first fn-def) 'macro))
           (apply-macro fn-def (rest target-form))
           target-form)))

   ; function/macro call t=bool
   (t
     (let ((fn-def (if (symbolp (first expr))
                       (lookup (first expr) env)
                       nil)))
       (cond
        ; macro
        ((and (consp fn-def) (eq (first fn-def) 'macro))
          (let ((expanded-expr (apply-macro fn-def (rest expr))))
            (my-eval expanded-expr env)))
        ; function
        (t
          (let ((fn (my-eval (first expr) env))
                (args (eval-list (rest expr) env)))
            (my-apply fn args))))))))
