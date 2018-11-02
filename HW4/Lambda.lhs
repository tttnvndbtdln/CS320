This module defines an abstract syntax for an applied lambda calculus, as defined in the week 7 and 8 slides.

> module Lambda where

As in Haskell, function arrows associate to the right, so that e.g.
"BOOL :-> BOOL :-> BOOL" parses as "BOOL :-> (BOOL :-> BOOL)".

> infixr 1 :->

The types we're working with are numbers, booleans, and functions. The :-> operator constructs a function type out of a domain type and a codomain type.

  t ::= num | bool | t1 -> t2

> data Type
>   = NUM
>   | BOOL
>   | Type :-> Type
>   deriving Eq

> showType :: Type -> String
> showType NUM = "num"
> showType BOOL = "bool"
> showType (t1 :-> t2) = "(" ++ showType t1 ++ " -> " ++ showType t2 ++ ")"

> instance Show Type where show = showType

The terms are variables, numbers, booleans, lambdas, applications, additions, multiplications, and if/then/else expressions.

  e ::= x | n | b | (\x. e) | (\(x: t). e) | (e1 e2)
      | e1 + e2 | e1 * e2 | e1 < e2 | if e1 then e2 else e3

The Lam constructor is used for both typed and untyped lambda expressions:

  (\x. e)      = Lam x Nothing e
  (\(x: t). e) = Lam x (Just t) e

> data Exp
>   = Var String
>   | Num Int
>   | Bool Bool
>   | Lam String (Maybe Type) Exp
>   | App Exp Exp
>   | Plus Exp Exp
>   | Times Exp Exp
>   | Less Exp Exp
>   | If Exp Exp Exp

> showOp :: String -> Exp -> Exp -> String
> showOp o e1 e2 = "(" ++ showExp e1 ++ o ++ showExp e2 ++ ")"

> showExp :: Exp -> String
> showExp (Var x) = x
> showExp (Num n) = show n
> showExp (Bool b) = show b
> showExp (App e1 e2) = showOp " " e1 e2
> showExp (Lam x Nothing e) = "(\\" ++ x ++ ". " ++ showExp e ++ ")"
> showExp (Lam x (Just t) e) = "(\\(" ++ x ++ ": " ++ show t ++ "). " ++ showExp e ++ ")"
> showExp (Plus e1 e2) = showOp " + " e1 e2
> showExp (Less e1 e2) = showOp " < " e1 e2
> showExp (Times e1 e2) = showOp " * " e1 e2
> showExp (If e1 e2 e3) = "(if " ++ showExp e1 ++ " then " ++ showExp e2 ++ " else " ++ showExp e3 ++ ")"

> instance Show Exp where show = showExp
