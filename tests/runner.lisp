;; プロジェクトルートの main.lisp をロードする
(load (merge-pathnames "../main.lisp" *load-truename*))

;; ---------------------------------------------------------
;; テストユーティリティ
;; ---------------------------------------------------------
(defvar *failed-count* 0)

;; 引数 env を追加（デフォルトは nil）
(defun assert-equal (input expected &optional (env nil))
  (let ((actual (my-eval input env))) ;; <--- ここで env を渡す！
    (if (equal actual expected)
        (format t "✅ [PASS] ~S -> ~S~%" input actual)
        (progn
         (format t "❌ [FAIL] ~S~%" input)
         (format t "   Expected: ~S~%" expected)
         (format t "   Actual:   ~S~%" actual)
         (incf *failed-count*)))))

;; ---------------------------------------------------------
;; テストケース定義
;; ---------------------------------------------------------
(defun run-all-tests ()
  (format t "=== Running Lisp Tests ===~%")

  ;; Step 1: リテラル
  (assert-equal 10 10)
  (assert-equal "Hello" "Hello")

  ;; Step 2: 変数
  (let ((env '((x . 10) (y . 20) (apple . "red"))))

    (assert-equal 'x 10 env) ;; x は 10 か？
    (assert-equal 'apple "red" env) ;; apple は "red" か？
    (assert-equal 'z nil env)) ;; 知らない変数は nil か？

  ;; Step 3: 特殊形式 (quote と if)
  ;; t と nil を環境に入れておくとテストしやすいです
  (let ((env '((t . t) (nil . nil) (x . 10))))

    ;; --- Quote のテスト ---
    ;; (quote x) は、xの中身(10)ではなく、シンボル x そのものを返すはず
    (assert-equal '(quote x) 'x env)
    ;; リストも評価されずにそのまま返るはず
    (assert-equal '(quote (1 2 3)) '(1 2 3) env)

    ;; --- If のテスト ---
    ;; 条件が真(t)なら、第2引数(10)が返る
    (assert-equal '(if t 10 20) 10 env)
    ;; 条件が偽(nil)なら、第3引数(20)が返る
    (assert-equal '(if nil 10 20) 20 env)

    ;; Ifの重要な性質: 選ばれなかった方は「評価されない」ことの確認
    ;; もし評価されていたら、未定義変数 y でエラーやnilになるはずだが、
    ;; 正しく実装されていれば x (10) が返るはず
    (assert-equal '(if t x y) 10 env))

  ;; Step 4: 関数適用 (Primitive Functions)
  ;; 環境に「シンボル」と「実際のCommon Lispの関数」のペアを用意します
  ;; #' (シャープクォート) は「関数オブジェクトそのもの」を取り出す記法です
  ;; 1. 基本的な計算
  (assert-equal '(+ 1 2) 3 nil)

  ;; 2. 入れ子の計算 (引数が評価されてから足し算されるか)
  ;; (+ 1 (+ 2 3)) -> (+ 1 5) -> 6
  (assert-equal '(+ 1 (+ 2 3)) 6 nil)

  ;; 3. リスト操作関数
  (assert-equal '(list 1 2) '(1 2) nil)
  (assert-equal '(car (cons 1 2)) 1 nil)

  ;; 4. 変数との組み合わせ
  ;; (let ((env (cons (cons 'x 10) global-env))) ... ) のように拡張してテスト
  (let ((env (append '((x . 10) (y . 20)) nil)))
    (assert-equal '(+ x y) 30 env))

  ;; Step 5: Lambda (ユーザー定義関数)

  ;; 1. 基本的なLambdaの即時実行
  ;; ((lambda (x) (+ x 1)) 10) -> xに10が入って計算される -> 11
  (assert-equal '((lambda (x) (+ x 1)) 10) 11 nil)

  ;; 2. 引数が複数の場合
  (assert-equal '((lambda (x y) (+ x y)) 10 20) 30 nil)

  ;; Step 6: Define (変数の定義)

  ;; 重要: Defineは「地球全体の環境 (*global-env*)」を書き換える副作用を持ちます。
  ;; テストのたびに環境が汚れるのを防ぐため、本来はリセット処理が必要ですが、
  ;; 簡易的に「毎回違う変数名」を使ってテストします。

  ;; 1. 単純な変数の定義
  ;; (define my-var 100) -> 評価結果はシンボル my-var (または値) が返ればOK
  (assert-equal '(define my-var 100) 'my-var)

  ;; 2. 定義した変数が参照できるか？
  ;; ここで my-var が 100 と評価されれば成功
  (assert-equal 'my-var 100)

  ;; 3. 関数の定義 (Syntactic Sugar ではなく、まずは define + lambda で)
  ;; (define inc (lambda (x) (+ x 1)))
  (assert-equal '(define inc (lambda (x) (+ x 1))) 'inc)

  ;; 4. 定義した関数が使えるか？
  (assert-equal '(inc 10) 11)

  ;; 5. ローカル変数とグローバル変数の優先順位
  ;; ローカルの x=999 が、グローバルの my-var=100 より優先されるか
  (let ((local-env '((my-var . 999))))
    (assert-equal 'my-var 999 local-env))

  ;; Step 7: 再帰関数 (Recursion)

  ;; 1. 階乗関数の定義
  ;; 再帰呼び出しをするためには、= と * が必要です
  (assert-equal '(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) 'fact)

  ;; 2. 実行テスト
  (assert-equal '(fact 5) 120) ;; 5 * 4 * 3 * 2 * 1 = 120

  ;; 3. フィボナッチ数列 (分岐が2つある再帰)
  ;; fib(n) = fib(n-1) + fib(n-2)
  ;; fib(0)=0, fib(1)=1
  ;; fib(6) -> 8
  (assert-equal
   '(define fib (lambda (n)
                  (if (< n 2)
                      n
                      (+ (fib (- n 1)) (fib (- n 2))))))
   'fib)

  (assert-equal '(fib 6) 8)

  ;; Step 8: Syntactic Sugar (defun)

  ;; 1. defun で関数定義
  ;; (defun add3 (x) (+ x 3))
  ;; これが内部で (define add3 (lambda (x) (+ x 3))) に変換されて実行される
  (assert-equal '(defun add3 (x) (+ x 3)) 'add3)

  ;; 2. 定義した関数の実行
  (assert-equal '(add3 10) 13)

  ;; 3. 再帰関数も defun で書けるか (階乗の再定義)
  ;; 以前の fact を上書きします
  (assert-equal
   '(defun fact (n)
      (if (= n 0)
          1
          (* n (fact (- n 1)))))
   'fact)

  (assert-equal '(fact 5) 120)

  ;; Step 9: Progn (複文の実行)

  ;; 1. 特殊形式 progn のテスト
  ;; 10, 20 と順番に評価されるが、結果として返るのは最後の 30
  (assert-equal '(progn 10 20 30) 30)

  ;; 2. 副作用の確認
  ;; define で変数を書き換えた後、その変数を使うことができるか？
  (assert-equal
   '(progn
     (define g-val 999)
     g-val)
   999)

  ;; 3. Lambda/Defun の暗黙の progn (Implicit Progn)
  ;; 関数定義の中で複数行書けるようにする
  (assert-equal
   '(defun multi-line (x)
      (define temp x) ;; 1行目
      (+ temp 1)) ;; 2行目
   'multi-line)

  (assert-equal '(multi-line 10) 11)
  (assert-equal 'temp 10) ;; 副作用が残っているか確認

  ;; Step 10: Macros (defmacro)

  ;; 1. マクロの定義 (unless)
  ;; (unless pred then else) -> (if pred else then) に変換するマクロ
  ;; ※ バッククォート (`) はCommon Lispのリーダーに依存して複雑になるため、
  ;;    ここでは安全に (list ...) を使ってコードを組み立てます。
  (assert-equal
   '(defmacro unless (pred a b)
      (list 'if pred b a))
   'unless)

  ;; 2. マクロの実行
  ;; (= 1 2) は偽(nil) なので、a ('ok) が返るはず。
  ;; もしこれが関数なら、'ng も評価されてしまいますが、
  ;; if に展開されるので 'ng は評価されないはずです（重要）。
  (assert-equal '(unless (= 1 2) 'ok 'ng) 'ok)
  (assert-equal '(unless (= 1 1) 'ok 'ng) 'ng)

  ;; 3. マクロ展開の確認 (macroexpand) - おまけ
  ;; 実装できれば、デバッグ用にマクロがどう展開されるか見る機能もあると便利です
  (assert-equal '(macroexpand '(unless t 1 2)) '(if t 2 1))

  ;; ---------------------------------------------------------
  ;; Step 11: Recursive Macros (cond)
  ;; ---------------------------------------------------------

  ;; 再帰的なマクロを書くためには、リストの終端判定などが必要になります。
  ;; main.lisp の *global-env* に eq, null, not が登録されている前提です。

  ;; 1. cond マクロの定義
  ;; (cond (p1 e1) (p2 e2) ...) 
  ;; -> (if p1 e1 (cond (p2 e2) ...)) という形に展開します
  (assert-equal
   '(defmacro cond (&rest clauses)
      (if (null clauses)
          nil
          (list 'if
                (car (car clauses)) ;; 条件式: (car first-clause)
                (car (cdr (car clauses))) ;; 処理:   (cadr first-clause)
                (cons 'cond (cdr clauses))))) ;; 再帰:   (cond rest-clauses)
   'cond)

  ;; 2. cond の動作テスト
  (assert-equal
   '(cond ((= 1 2) 'one)
          ((= 1 1) 'two)
          (t 'three))
   'two)

  ;; 3. 再帰の終端 (該当なしで nil が返るか)
  (assert-equal
   '(cond ((= 1 2) 'one))
   nil)

  ;; ---------------------------------------------------------
  ;; Step 12: Let Macro
  ;; ---------------------------------------------------------

  ;; 1. ヘルパー関数の定義
  ;; letマクロが展開されるときに、リスト操作のために map や cadr が必要になります。

  ;; cadr: リストの2番目の要素を取る (car (cdr x))
  (assert-equal '(define cadr (lambda (x) (car (cdr x)))) 'cadr)

  ;; map: リストの各要素に関数を適用する
  (assert-equal
   '(define map (lambda (f lst)
                  (if (null lst)
                      nil
                      (cons (f (car lst)) (map f (cdr lst))))))
   'map)

  ;; 2. let マクロの定義
  ;; バインディングリスト ((v1 i1) (v2 i2)) を
  ;; 変数リスト (v1 v2) と 初期値リスト (i1 i2) に分解し、
  ;; ((lambda (v1 v2) body...) i1 i2) というリストを構築します。
  (assert-equal
   '(defmacro let (bindings &rest body)
      (cons (cons 'lambda
                  (cons (map car bindings)
                        body))
            (map cadr bindings)))
   'let)

  ;; 3. let の動作テスト
  (assert-equal
   '(let ((x 10) (y 20))
      (+ x y))
   30)

  ;; 4. スコープとシャドウイングのテスト
  ;; 内側の let で x を再定義したら、内側では新しい値になるか
  (assert-equal
   '(let ((x 10))
      (let ((x 999))
        x))
   999)

  ;; 5. letのネストと計算
  (assert-equal
   '(let ((x 10))
      (let ((y (+ x 5)))
        (+ x y)))
   25)

  ;; ---------------------------------------------------------
  ;; 集計と終了コード
  ;; ---------------------------------------------------------
  (if (= *failed-count* 0)
      (progn
       (format t "🎉 All tests passed!~%") ;; 修正: ここでカッコを閉じ
       (sb-ext:exit :code 0))
      (progn
       (format t "🔥 ~A tests failed!~%" *failed-count*) ;; 修正: ここでカッコを閉じ
       (sb-ext:exit :code 1))))

;; テスト実行
(run-all-tests)
