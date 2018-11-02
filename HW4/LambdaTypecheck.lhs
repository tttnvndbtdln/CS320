This module implements a typechecking procedure for an applied lambda calculus,
as covered in the week 8 slides.

> module LambdaTypecheck where

> import Lambda
> import LambdaParse

A typing environment is a mapping from String variable names to types. In
Haskell, this is easy to implement as a list of (String, Type) pairs. A common
convention is to use the capital Greek letter gamma (Γ) to represent an
environment, so in the code in this file environments are represented with the
letter g, for "gamma".

> type TypeEnv = [(String, Type)]

Since typechecking a term might fail (if the term is ill-typed), the TypeResult
type represents a value that's either an error or a type.

> data TypeResult = Error String | Type Type

> showTypeResult :: TypeResult -> String
> showTypeResult (Error e) = e
> showTypeResult (Type t) = show t

> instance Show TypeResult where show = showTypeResult

Errors are "caught" and "rethrown" in each case of the evaluator that has
subexpressions; this gives a little bit of diagnostic output about where the
error is in the original expression. If evaluating a subexpression produces an
error, we "catch" the error by pattern matching against Error and then
"rethrow" it by returning a new Error value whose error message is the old
error's message concatenated with a string specifying where the error happened.

> typecheck :: Exp -> TypeEnv -> TypeResult

  Γ(x) = t
 -----------
  Γ ⊢ x : t

> typecheck (Var x) g =
>   case lookup x g of
>     Just t -> Type t
>     Nothing -> Error ("variable out of scope: " ++ x)

 -------------
  Γ ⊢ n : num

> typecheck (Num n) g = Type NUM

 --------------
  Γ ⊢ b : bool

> typecheck (Bool b) g = Type BOOL

This algorithm requires type annotations on every function argument. The syntax
is the same as in the week 8 slides - for example:

  (\(f: num -> bool) (x: num). f x) (\(a: num). a + 1) 3

The Lam case implements the rule for typechecking lambdas: the body of the
lambda is typechecked in an environment extended with the argument name mapped
to the argument type.

      (x: t1), Γ ⊢ e : t2
 -----------------------------
  Γ ⊢ (\(x: t). e) : t1 -> t2

> typecheck (Lam x Nothing e) g = Error ("can't typecheck untyped argument: " ++ x)
> typecheck (Lam x (Just t) e) g =
>   case typecheck e ((x,t):g) of
>     Error err -> Error ("function body:\n" ++ err)
>     Type t' -> Type (t :-> t')

The App case implements the rule for typechecking applications; a type error is
thrown if the left operand is not of function type or if the right argument's
type isn't the same as the domain of the function type.

  Γ ⊢ e1 : t1 -> t2   Γ ⊢ e2 : t1
 ---------------------------------
         Γ ⊢ (e1 e2) : t2

> typecheck (App e1 e2) g =
>   case typecheck e1 g of
>     Error err -> Error ("application left operand:\n" ++ err)
>     Type (t1 :-> t2) ->
>       case typecheck e2 g of
>         Error err -> Error ("application right operand:\n" ++ err)
>         Type t1' ->
>           if t1 == t1' then
>             Type t2
>           else
>             Error ("can't apply function type " ++ show (t1 :-> t2) ++ " to argument type " ++ show t1')
>     Type t -> Error ("can't apply non-function type " ++ show t ++ " to argument")

ASSIGNMENT 4 HERE
The Numeric case implements the less than operator; a type error is thrown if
either or both operand is not of type NUM and return a bool if the first
expression is less than the second

  Γ ⊢ e1 : num   Γ ⊢ e2 : num
 -----------------------------
      Γ ⊢ (e1 < e2) : bool

> typecheck (Numeric e1 e2) g =
>   case typecheck e1 g of
>     Error err -> Error ("less than operator left operand:\n" ++ err)
>     Type NUM ->
>       case typecheck e2 g of
>         Error err -> Error ("less than operator right operand: \n" ++ err)
>         Type NUM -> Type NUM
>         Type t -> Error ("can't compare right operand of type " ++ show t)
>     Type t -> Error ("can't compare left operand of type " ++ show t)

  Γ ⊢ e1 : num   Γ ⊢ e2 : num
 -----------------------------
     Γ ⊢ (e1 + e2) : num

> typecheck (Plus e1 e2) g =
>   case typecheck e1 g of
>     Error err -> Error ("addition left operand:\n" ++ err)
>     Type NUM ->
>       case typecheck e2 g of
>         Error err -> Error ("addition right operand:\n" ++ err)
>         Type NUM -> Type NUM
>         Type t -> Error ("can't add right operand of type " ++ show t)
>     Type t -> Error ("can't add left operand of type " ++ show t)

  Γ ⊢ e1 : num   Γ ⊢ e2 : num
 -----------------------------
     Γ ⊢ (e1 * e2) : num

> typecheck (Times e1 e2) g =
>   case typecheck e1 g of
>     Error err -> Error ("multiplication left operand:\n" ++ err)
>     Type NUM ->
>       case typecheck e2 g of
>         Error err -> Error ("multiplication right operand:\n" ++ err)
>         Type NUM -> Type NUM
>         Type t -> Error ("can't multiply right operand of type " ++ show t)
>     Type t -> Error ("can't multiply left operand of type " ++ show t)

The If case throws an error if the conditional is not of Boolean type or if the
two branches don't have the same type.

  Γ ⊢ e1 : bool   Γ ⊢ e2 : t   Γ ⊢ e3 : t
 -----------------------------------------
      Γ ⊢ (if e1 then e2 else e3) : t

> typecheck (If e1 e2 e3) g =
>   case typecheck e1 g of
>     Error err -> Error ("if/then/else conditional:\n" ++ err)
>     Type BOOL ->
>       case typecheck e2 g of
>         Error err -> Error ("if/then/else true branch:\n" ++ err)
>         Type t2 ->
>           case typecheck e3 g of
>             Error err -> Error ("if/then/else false branch:\n" ++ err)
>             Type t3 ->
>               if t2 == t3 then
>                 Type t2
>               else
>                 Error ("can't have if branches of different types: " ++ show t2 ++ " and " ++ show t3)
>     Type t -> Error ("can't use value of type " ++ show t ++ " as if/then/else conditional")

The "readTypecheck" function parses a string to an expression and then typechecks
it.

> readTypecheck :: String -> TypeEnv -> TypeResult
> readTypecheck e = typecheck (readExp e)

One nice feature of this style of typechecker is that it's easy to use free
variables to represent "standard library" values, which enables us to extend
the language with typed constants. For example, we can typecheck an expression
that uses a Boolean "not" function that's not defined as a primitive language
construct:

  readTypecheck "if not true then 1 else 2" [("not", BOOL :-> BOOL)]
