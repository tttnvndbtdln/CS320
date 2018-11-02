This module implements a big-step call-by-value operational semantics for an
applied lambda calculus, as covered in the week 8 lecture. Since we're
evaluating expressions down to values of Haskell types, it can also be
considered a denotational semantics with Haskell as the metalanguage.

> module LambdaBigStep where

> import Lambda
> import LambdaParse

An evaluation environment is a mapping from String variable names to values. In
Haskell, this is easy to implement as a list of (String, Val) pairs. A common
convention is to use the lowercase Greek letter rho (ρ) to represent an
environment, so in the code in this file environments are represented with the
letter p, which looks sort of similar.

> type EvalEnv = [(String, Val)]

A value is either a number, a boolean, a closure, or an error.

VCls is the closure constructor. In the syntax used in the slides:

  VCls x e p = <p, (\x. e)>

> data Val
>   = VNum Int
>   | VBool Bool
>   | VCls String Exp EvalEnv
>   | VErr String

> showVal :: Val -> String
> showVal (VNum n) = show n
> showVal (VBool b) = show b
> showVal (VCls x e p) = "<" ++ show p ++ ", " ++ "(\\" ++ x ++ ". " ++ show e ++ ">"
> showVal (VErr e) = e

> instance Show Val where show = showVal

As a denotational semantics, this says that the meaning of an Exp is a function
from an environment to a value (since the function arrow associates to the
right in the type of "eval"). As a Haskell function, it lets us plug in an
expression and an environment and get back a value.

Errors are "caught" and "rethrown" in each case of the evaluator that has
subexpressions; this gives a little bit of diagnostic output about where the
error is in the original expression. If evaluating a subexpression produces an
error, we "catch" the error by pattern matching against VErr and then "rethrow"
it by returning a new VErr value whose error message is the old error's message
concatenated with a string specifying where the error happened.

> eval :: Exp -> EvalEnv -> Val

  ρ(x) = v
------------
 <ρ, x> ⇓ v

> eval (Var x) p =
>   case lookup x p of
>     Just v -> v
>     Nothing -> VErr ("variable out of scope: " ++ x)

------------
 <ρ, x> ⇓ n

> eval (Num n) p = VNum n

------------
 <ρ, b> ⇓ b

> eval (Bool b) p = VBool b

-----------------------------
 <ρ, (\x. e)> ⇓ <ρ, (\x. e)>

> eval (Lam x t e) p = VCls x e p

The App rule is a little complicated:

 <ρ, e2> ⇓ v2   <ρ, e1> ⇓ <ρ', (\x. e1')>   <(x |-> v2, ρ'), e1'> ⇓ v
----------------------------------------------------------------------
                         <ρ, (e1 e2)> ⇓ v

First we evaluate the argument to a value (strict evaluation), then we evaluate
the function to a closure, then we evaluate the body of the closure in its
saved environment extended with the closure argument name mapped to the
argument value. If the function operand evalautes to a non-closure, we throw a
dynamic type error.

> eval (App e1 e2) p =
>   case eval e2 p of
>     VErr err -> VErr ("application right operand:\n" ++ err)
>     v2 ->
>       case eval e1 p of
>         VErr err -> VErr ("application left operand:\n" ++ err)
>         VCls x e1' p' -> eval e1' ((x,v2):p')
>         v1 -> VErr ("can't apply non-function " ++ show v1 ++ " to argument")

The Plus rule evaluates the left operand to a number, then evaluates the right
operand to a number, then returns their sum. If either operand evaluates to a
non-number, a dynamic type error is thrown.

 <ρ, e1> ⇓ n1   <ρ, e2> ⇓ n2   n1 + n2 = n
-------------------------------------------
            <ρ, (e1 + e2)> ⇓ n

> eval (Plus e1 e2) p =
>   case eval e1 p of
>     VErr err -> VErr ("addition left operand:\n" ++ err)
>     VNum n1 ->
>       case eval e2 p of
>         VErr err -> VErr ("addition right operand:\n" ++ err)
>         VNum n2 -> VNum (n1 + n2)
>         v2 -> VErr ("can't add non-numeric right operand " ++ show v2)
>     v1 -> VErr ("can't add non-numeric left operand " ++ show v1)

 <ρ, e1> ⇓ n1   <ρ, e2> ⇓ n2   n1 * n2 = n
-------------------------------------------
            <ρ, (e1 * e2)> ⇓ n

> eval (Times e1 e2) p =
>   case eval e1 p of
>     VErr err -> VErr ("multiplication left operand:\n" ++ err)
>     VNum n1 ->
>       case eval e2 p of
>         VErr err -> VErr ("multiplication right operand:\n" ++ err)
>         VNum n2 -> VNum (n1 * n2)
>         v2 -> VErr ("can't multiply non-numeric right operand " ++ show v2)
>     v1 -> VErr ("can't multiply non-numeric left operand " ++ show v1)

The If case implements both if/then/else rules:

   <ρ, e1> ⇓ true   <ρ, e2> ⇓ v        <ρ, e1> ⇓ false    <ρ, e2> ⇓ v
----------------------------------   ----------------------------------
 <ρ, (if e1 then e2 else e3)> ⇓ v     <ρ, (if e1 then e2 else e3)> ⇓ v

(Note that it doesn't throw a dynamic type error if the types of the two
branches are different.)

> eval (If e1 e2 e3) p =
>   case eval e1 p of
>     VErr err -> VErr ("if/then/else conditional:\n" ++ err)
>     VBool True -> eval e2 p
>     VBool False -> eval e3 p
>     v1 -> VErr ("can't use non-boolean value " ++ show v1 ++ " as if/then/else conditional")

The "readEval" function parses a string to an expression and then evaluates it.

> readEval :: String -> EvalEnv -> Val
> readEval e = eval (readExp e)

One nice feature of this style of evaluator is that it's easy to use free
variables to represent "standard library" values built out of the primitives of
the language, which enables a style of macro programming. For example, to build
a "not" function for Boolean expressions, we can construct a closure
programmatically:

> notVal :: Val
> notVal = readEval "\\x. if x then false else true" []

Now we can use this function under the name "not" in the evaluator by adding
it to the initial context. For example, this will return 2 in GHCi:

  readEval "if not true then 1 else 2" [("not", notVal)]

We can also construct constants using Haskell expressions, to build values that
would be inconvenient to express in this minimal object language. For example,
this "megabytes" function expresses an arbitrary number of bytes in megabytes,
rounded up. ("fromIntegral" here converts an Int to a Float, and "ceiling"
rounds a Float up to become an Int.)

> megabytes :: Int -> Val
> megabytes n = VNum (ceiling (fromIntegral n / (2 ^ 20)))

Now we can use this function to construct values in the initial context. For
example, this will return 24452 in GHCi:

  readEval "someMB * 2" [("someMB", megabytes 12819463045)]
