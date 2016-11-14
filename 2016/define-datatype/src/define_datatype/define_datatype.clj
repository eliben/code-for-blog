;;; Clojure implementation of the 'define-datatypes' and 'cases' macros from
;;; the "Elements of Programming Languages" (EOPL) book - third edition.
;;;
;;; Given:
;;;
;;; (define-datatype lc-exp lc-exp?
;;;   (var-exp
;;;    (var symbol?))
;;;   (lambda-exp
;;;    (bound-var symbol?)
;;;    (body lc-exp?))
;;;   (app-exp
;;;    (rator lc-exp?)
;;;    (rand lc-exp?)))
;;;
;;; We createa a "type" named 'lc-exp, with 3 variants: 'var-exp, 'lambda-exp
;;; and 'app-exp. The following functions are defined automatically:
;;;
;;; Predicate -- is arg an lc-exp?:
;;;   lc-exp?: fn [arg] ; predicate: is arg an lc-exp?
;;;
;;; Constructors that verify that their arguments comply to the predicates
;;; provided in the define-datatype specification, and predicates that check
;;; that the given object is a variant:
;;;   var-exp: fn [var]
;;;   var-exp?: fn [arg]
;;;   lambda-exp: fn [bound-var body]
;;;   lambda-exp?: fn [arg?]
;;;   app-exp: fn [rator rand]
;;;   app-exp?: fn [arg]
;;;
;;; Accessors for fields:
;;;   var-exp->var: fn [arg] 
;;;   lambda-exp->bound-var: fn [arg]
;;;   lambda-exp->body: fn [arg]
;;;   app-exp->rator: fn [arg]
;;;   app-exp->rand: fn [arg]
;;;
;;; Also, the macro 'cases' permits compact actions on objects created with
;;; define-datatypes. See tests below.
;;;
;;; This system creates "objects" as follows: 
;;;
;;;   (typename variant-name field1 field2 ... fieldN)
;;;
;;; typename is the name of the encompassing type (such as 'lc-exp);
;;; variant-name is the name of the specific variant of the type (such as
;;; 'app-exp); field1...N are the fields of the variant (for example the variant
;;; 'app-exp has two fields)
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(ns define-datatype.define-datatype
  (:use clojure.test))

(defn internfunc
  "Helper for interning a function with the given name (as a string) in the
  current namespace."
  [strname func]
  (intern *ns* (symbol strname) func))

(defn make-accessor
  "Creates an accessor for field 'fielname' in variant 'variant-name'.
  The accessor is positional using 'nth'."
  [variant-name fieldname fieldpos]
  (letfn [(func [obj]
            (nth obj fieldpos))]
    (internfunc (str variant-name "->" fieldname) func)))

(defn make-predicate
  "Creates a predicate function to test whether a given object belongs to a type
  variant. numfields is the number of fields the variant object is expected to
  have."
  [typename variant-name numfields]
  (letfn [(func [obj]
            (and (= (count obj) (+ 2 numfields))
                 (= (first obj) typename)
                 (= (second obj) variant-name)))]
    (internfunc (str variant-name "?") func)))

(defn make-ctor
  "Creates a constructor function for the given variant of a type. predicates is
  a list of predicates for the fields passed to the constructor."
  [typename variant-name predicates]
  (letfn [(func [& field-initializers]
            (do
              (assert (= (count field-initializers) (count predicates)))
              (assert (every? true? (map (fn [pred field]
                                           ((resolve pred) field))
                                         predicates
                                         field-initializers)))
              (cons typename (cons variant-name field-initializers))))]
    (internfunc (str variant-name) func)))

(defn make-type-variant
  "Creates a type variant with a constructor, predicate and accessors.

  The typename and variant-name are symbols; field-descriptors is a list of
  pairs, each consisting of two symbols: field name and a predicate to test for
  the validity of this field when creating a type variant. The predicate is a
  symbol, not an actual Clojure function."
  [typename variant-name field-descriptors]
  (let [numfields (count field-descriptors)
        predicates (map second field-descriptors)]
    (do
      ;; Create the constructor for this type.
      (make-ctor typename variant-name predicates)
      ;; Create the predicate for this type.
      (make-predicate typename variant-name numfields)
      ;; Create field accessors. 
      (doseq [[field-index [fieldname field-predicate]] 
              (map-indexed vector field-descriptors)]
        (make-accessor variant-name fieldname (+ 2 field-index))))))

(defn define-datatype-aux
  "Creates a datatype from the specification. This is a function, so all its
  arguments are symbols or quoted lists. In particular, variant-descriptors is a
  quoted list of all the descriptors."
  [typename predicate-name variant-descriptors]
  (do
    (letfn [(pred-func [obj]
              (= (first obj) typename))]
      (internfunc (str predicate-name) pred-func))
    (doseq [[variant-name & field-descriptors] variant-descriptors]
      (make-type-variant typename variant-name field-descriptors))))

(defmacro define-datatype
  "Simple macro wrapper around define-datatype-aux, so that the type name,
  predicate name and variant descriptors don't have to be quoted but rather can
  be regular Clojure symbols."
  [typename predicate-name & variant-descriptors]
  (define-datatype-aux typename predicate-name variant-descriptors))

(defn make-cond-case
  "Helper function for cases that generates a single case for the variant cond.

  variant-case is one variant case as given to the cases macro.
  obj-variant is the actual object variant (a symbol) as taken from the object.
  obj-fields is the list of the actual object's fields.

  Produces the code for '(cond-case cond-action)."
  [variant-case obj-variant obj-fields]
  `((= (quote ~(first variant-case)) ~obj-variant)
    (apply (fn [~@(second variant-case)] ~(last variant-case)) ~obj-fields)))

(defmacro cases
  [typename obj & variant-cases]
  (let [obj-type-sym (gensym 'type)
        obj-variant-sym (gensym 'variant)
        obj-fields-sym (gensym 'fields)]
    `(let [[~obj-type-sym ~obj-variant-sym & ~obj-fields-sym] ~obj]
       (assert (= ~obj-type-sym (quote ~typename)) "Unexpected type")
       (cond
         ~@(mapcat (fn [vc] (make-cond-case vc obj-variant-sym obj-fields-sym))
                   variant-cases)
         :else (assert false "Unsupported variant")))))

;;; ------------------   Testing  ------------------

;;; A silly tuple is either a single symbol, or a pair of integers.
(define-datatype sillytuple sillytuple?
  (single (s symbol?))
  (pair (i integer?) (j integer?)))

(deftest test-sillytuple-single
  (let [s (single 'k)]
    (is (sillytuple? s))
    (is (single? s))
    (is (not (pair? s)))
    (is (= 'k (single->s s)))))

(deftest test-sillytuple-bad-single
  (is (thrown? AssertionError (single '(k))))
  (is (thrown? AssertionError (single 20))))

(deftest test-sillytuple-pair
  (let [p (pair 10 20)]
    (is (sillytuple? p))
    (is (pair? p))
    (is (not (single? p)))
    (is (= 10 (pair->i p)))
    (is (= 20 (pair->j p)))))

(deftest test-sillytuple-bad-pair
  (is (thrown? AssertionError (pair 'foo 10)))
  (is (thrown? AssertionError (pair 20 21.5))))

(deftest test-sillytuple-simple-cases
  ;; Test that a simple cases definition works well
  (letfn [(good? [st]
            (cases sillytuple st
                   (single (s) (= s 'good))
                   (pair (i j) (> i j))))]
    (is (not (good? (single 'food))))
    (is (good? (single 'good)))
    (is (good? (pair 10 9)))
    (is (not (good? (pair 10 19))))
    (is (thrown-with-msg? 
         AssertionError #"Unexpected" (good? '(kuku da))))
    ;; Force-create a bad silly tuple (inexistent variant) and good? should
    ;; throw as expected.
    (is (thrown-with-msg? 
         AssertionError #"Unsupported variant"
         (good? '(sillytuple foobar 20))))))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

;;; Trivial 'cases' for testing
(defn lc-exp-numargs
  [exp]
  (cases lc-exp exp
         (var-exp (variable) 1)
         (lambda-exp (bound-var body) 2)
         (app-exp (rator rand) 3)))

(deftest test-lc-exp-simple
  (let [ve (var-exp 't)]
    (is (lc-exp? ve))
    (is (var-exp? ve))
    (is (not (app-exp? ve)))
    (is (= 1 (lc-exp-numargs ve))))
  (let [v (lambda-exp 'v (app-exp (var-exp 'v) (var-exp 't)))]
    (is (lc-exp? v))
    (is (lambda-exp? v))
    (is (= 'v (lambda-exp->bound-var v)))
    (is (app-exp? (lambda-exp->body v)))
    (is (= 2 (lc-exp-numargs v)))
    (is (= 3 (lc-exp-numargs (lambda-exp->body v))))
    (is (= 1 (lc-exp-numargs (app-exp->rator (lambda-exp->body v)))))
    (is (= 1 (lc-exp-numargs (app-exp->rand (lambda-exp->body v)))))))

(defn occurs-free?
  "Does search-var occur as a free variable in exp?"
  [search-var exp]
  (cases lc-exp exp
         (var-exp (variable) (= variable search-var))
         (lambda-exp (bound-var body) 
                     (and (not (= search-var bound-var))
                          (occurs-free? search-var body)))
         (app-exp (rator rand)
                  (or
                   (occurs-free? search-var rator)
                   (occurs-free? search-var rand)))))

(deftest test-lc-exp-occurs-free
  (let [ve (var-exp 't)]
    (is (occurs-free? 't ve))
    (is (not (occurs-free? 'r ve)))
    (let [ae (app-exp (var-exp 't) (app-exp (var-exp 'd) (var-exp 'e)))]
      (is (occurs-free? 't ae))
      (is (occurs-free? 'd ae))
      (is (occurs-free? 'e ae))
      (is (not (occurs-free? 'p ae))))
    (let [le (lambda-exp 'v (app-exp (var-exp 'v) (var-exp 't)))]
      (is (occurs-free? 't le))
      (is (not (occurs-free? 'v le))))
    (let [ne (app-exp 
              (var-exp 't)
              (app-exp
               (var-exp 'd)
               (lambda-exp 'v (app-exp (var-exp 'c) (var-exp 'v)))))]
      (is (occurs-free? 't ne))
      (is (occurs-free? 'd ne))
      (is (occurs-free? 'c ne))
      (is (not (occurs-free? 'v ne))))))

;;; Exercise 2.21: the environment data structure, from section 2.2.2

(defn symbol-or-number? [v] (or (symbol? v) (number? v)))

(define-datatype env env?
  (empty-env)
  (extend-env
   (var symbol?)
   (val symbol-or-number?)
   (env env?)))

(defn apply-env
  [e search-var]
  (cases env e
         (empty-env () (assert false "apply-env on empty env"))
         (extend-env (var val saved-env)
                     (if (= search-var var)
                       val
                       (apply-env saved-env search-var)))))

(defn has-binding?
  [e search-var]
  (cases env e
         (empty-env () false)
         (extend-env (var val saved-env)
                     (or (= search-var var)
                         (has-binding? saved-env search-var)))))

(deftest test-env
  (let [emp (empty-env)]
    (is (env? emp)))
  (let [ext1 (extend-env 'v 20 (empty-env))]
    (is (env? ext1))
    (is (= (extend-env->var ext1) 'v))
    (is (= (apply-env ext1 'v) 20))
    (is (has-binding? ext1 'v))
    (is (thrown? AssertionError (apply-env ext1 'jk)))
    (is (not (has-binding? ext1 'jf)))
    (is (= (extend-env->val ext1) 20)))
  (let [ext3 (extend-env
              'a 1 (extend-env 'b 2 (extend-env 'c 3 (empty-env))))]
    (is (= (apply-env ext3 'a) 1))
    (is (has-binding? ext3 'a))
    (is (= (apply-env ext3 'b) 2))
    (is (has-binding? ext3 'b))
    (is (= (apply-env ext3 'c) 3))
    (is (has-binding? ext3 'c))
    (is (not (has-binding? ext3 'd)))
    (is (thrown? AssertionError (apply-env ext3 'd)))))

;;; Exercise 2.24: binary tree

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(defn bintree-to-list
  [bt]
  (cases bintree bt
         (leaf-node (num) (list 'leaf-node num))
         (interior-node (key left right)
                        (list 'interior-node
                              key 
                              (bintree-to-list left)
                              (bintree-to-list right)))))

(defn max-interior
  [bt]
  ;; exercise 2.25
  (cases bintree bt
         (leaf-node (num) [num nil])
         (interior-node (key left right)
                        (let [[leftsum leftsym] (max-interior left)
                              [rightsum rightsym] (max-interior right)
                              mysum (+ leftsum rightsum)]
                          (cond
                            (and (> mysum rightsum) (> mysum leftsum))
                            [mysum key]
                            (and (> leftsum rightsum) (> leftsum mysum))
                            [leftsum leftsym]
                            :else 
                            [rightsum rightsym])))))

(deftest test-bintree
  (let [bt1 (interior-node 'k
                           (interior-node 'p (leaf-node 20) (leaf-node 30))
                           (leaf-node 40))]
    (is (bintree? bt1))
    (is (= (bintree-to-list bt1)
           '(interior-node k
                           (interior-node p (leaf-node 20) (leaf-node 30))
                           (leaf-node 40)))))
  (let [tree-1 (interior-node 'foo (leaf-node 2) (leaf-node 3))
        tree-2 (interior-node 'bar (leaf-node -1) tree-1)
        tree-3 (interior-node 'baz tree-2 (leaf-node 1))]
    (is (= [5 'foo] (max-interior tree-2)))
    (is (= [6 'baz] (max-interior tree-3)))))

(run-tests)
