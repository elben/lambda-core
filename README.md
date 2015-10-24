# LAMBDA-CORE

LAMBDA-CORE is a minimal Haskell implementation of the typed lambda calculus with type inference, in the Lisp syntax. Its goal is to be an easy-to-understand implementation for those interested in learning Haskell and programming languages.

Includes:

- Parser
- Type checker and type inferencer
- Interpreter

Example:

```
(+ 1 1)
=> 2 : Int

(fn (x) x)
=> (fn (x) x) : (-> a a)

(fn (f x) (f x))
=> (fn (f x) (f x)) : (-> (-> a b) b)

(fn (x) (+ x 1))
=> (fn (x) (+ x 1)) : (-> Int Int)

(let (x 10
      y 20)
  (+ x y))
=> 30 : Int

(let (double (fn (f x) (f (f x)))
      plus-two (double (fn (x) (+ x 1)) 10)
      exclaimed (double (fn (x) (+ x "!"))))
  0)
=> 0 : Int
```

# Understanding LAMBDA-CORE

# Development

```
cd ~/code/lambda-core

stack install
stack build
stack ghci
```
