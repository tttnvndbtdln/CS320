This module implements a small-step call-by-value operational semantics for an
applied lambda calculus, as covered in the week 7 lecture.

> module LambdaSmallStep where

> import Lambda
> import LambdaParse

The "free" function produces a list of the free variables in an expression. The
Lam case is the only subtle part: the free variables in a lambda are all the
free variables in the body _except_ the variable bound by the lambda.

> free :: Exp -> [String]
> free (Var x) = [x]
> free (Lam x t e) = filter (\x' -> x /= x') (free e)
> free (App e1 e2) = free e1 ++ free e2
> free (Plus e1 e2) = free e1 ++ free e2
> free (Times e1 e2) = free e1 ++ free e2
> free (If e1 e2 e3) = free e1 ++ free e2 ++ free e3
> free _ = []

The "next" function produces a new name from an old name. The "succ" function
is from the Haskell standard library, and in this usage it returns the next
character after its input in ASCII ordering.

> next :: String -> String
> next ('z' : xs) = 'A' : xs
> next ('Z' : xs) = "aZ" ++ xs
> next (x : xs) = succ x : xs
> next "" = ""

The "fresh" function uses "next" to generate a variable name similar to the
first argument that's not a member of the list given as the second argument.
This is useful during capture-avoiding substitution: when alpha-conversion
needs to be invoked, "fresh" is used to produce a name that's guaranteed not to
be free in the relevant expression.

> fresh :: String -> [String] -> String
> fresh x xs = if elem x xs then fresh (next x) xs else x

The "rename" function, also used during alpha-conversion, renames every free
occurrence of x1 to x2 in the given expression.

> rename :: String -> String -> Exp -> Exp
> rename x1 x2 (Var x) = Var (if x == x1 then x2 else x)
> rename x1 x2 (Lam x t e) = Lam x t (if x == x1 then e else rename x1 x2 e)
> rename x1 x2 (App e1 e2) = App (rename x1 x2 e1) (rename x1 x2 e2)
> rename x1 x2 (Plus e1 e2) = Plus (rename x1 x2 e1) (rename x1 x2 e2)
> rename x1 x2 (Times e1 e2) = Times (rename x1 x2 e1) (rename x1 x2 e2)
> rename x1 x2 (If e1 e2 e3) = If (rename x1 x2 e1) (rename x1 x2 e2) (rename x1 x2 e3)
> rename x1 x2 e = e

The "subst" function implements non-capture-avoiding substitution. In the
"step" function below, the App case uses alpha-conversion to avoid variable
capture.

> subst :: String -> Exp -> Exp -> Exp
> subst x (Var x') e' = if x == x' then e' else Var x'
> subst x (Lam x' t e) e' = Lam x' t (if x == x' then e else subst x e e')
> subst x (App e1 e2) e' = App (subst x e1 e') (subst x e2 e')
> subst x (Plus e1 e2) e' = Plus (subst x e1 e') (subst x e2 e')
> subst x (Times e1 e2) e' = Times (subst x e1 e') (subst x e2 e')
> subst x (If e1 e2 e3) e' = If (subst x e1 e') (subst x e2 e') (subst x e3 e')
> subst x e e' = e

The "step" function implements a small-step semantics for this language by
giving a case for each rule in the semantics. It returns the next normalization
step from the input expression, or Nothing if there is no rule that applies (in
which case the expression is either a value or stuck).

> step :: Exp -> Maybe Exp

The first App case implements this rule:

   x is not free in e2
 ------------------------
  (\x. e1) e2 ⇒ e1[e2/x]

If x is free in e2, it satisfies the condition by first generating some new
name that is not free in e2 and renaming every unbound occurrence of x in e1 to
the new name.

> step (App (Lam x t e1) e2) =
>   if elem x (free e2) then
>     let x' = fresh x (free e2) in
>       Just (App (Lam x' t (rename x x' e1)) e2)
>   else
>     Just (subst x e1 e2)

The second App case implements the other two App rules:

       e1 ⇒ e1'              e2 ⇒ e1'
 --------------------  --------------------
  (e1 v2) ⇒ (e1' v2)    (e1 e2) ⇒ (e1' e2)

The call-by-value restriction that requires the argument to be evaluated first
is enforced by first checking if e2 can take a step, and then if it can't,
checking if e1 can take a step.

> step (App e1 e2) =
>   case step e2 of
>     Just e2' -> Just (App e1 e2')
>     Nothing ->
>       case step e1 of
>         Just e1' -> Just (App e1' e2)
>         Nothing -> Nothing

Similarly, the restriction that the left operand of an addition must be
evaluated before the right operand is enforced by first checking if e1 can take
a step, and then if it can't, checking if e2 can take a step.

  n1 + n2 = n3         e1 ⇒ e1'              e2 ⇒ e2'
 --------------  --------------------  --------------------
  n1 + n2 ⇒ n3    e1 + e2 ⇒ e1' + e2    v1 + e2 ⇒ v1 + e2'

> step (Plus (Num n1) (Num n2)) = Just (Num (n1 + n2))
> step (Plus e1 e2) =
>   case step e1 of
>     Just e1' -> Just (Plus e1' e2)
>     Nothing ->
>       case step e2 of
>         Just e2' -> Just (Plus e1 e2')
>         Nothing -> Nothing

  n1 * n2 = n3          e1 ⇒ e1'              e2 ⇒ e2'
 ---------------  --------------------  --------------------
  n1 * n2 ⇒ n3     e1 * e2 ⇒ e1' * e2    v1 * e2 ⇒ v1 * e2'

> step (Times (Num n1) (Num n2)) = Just (Num (n1 * n2))
> step (Times e1 e2) =
>   case step e1 of
>     Just e1' -> Just (Times e1' e2)
>     Nothing ->
>       case step e2 of
>         Just e2' -> Just (Times e1 e2')
>         Nothing -> Nothing

 ------------------------------  -------------------------------
  if true then e1 else e2 ⇒ e1    if false then e1 else e2 ⇒ e2
 
                            e1 ⇒ e1'
        ------------------------------------------------
         if e1 then e2 else e3 ⇒ if e1' then e2 else e3

> step (If (Bool True) e1 e2) = Just e1
> step (If (Bool False) e1 e2) = Just e2
> step (If e1 e2 e3) =
>   case step e1 of
>     Just e1' -> Just (If e1' e2 e3)
>     Nothing -> Nothing

Any expression that doesn't match any of the above patterns is either a value
or a stuck term, so we know there's no step it could take.

> step _ = Nothing

The "eval" function evaluates an expression by applying normalization steps
until there are no more to apply. (Remember the omega combinator from the
slides: this might not always terminate!)

> eval :: Exp -> Exp
> eval e = case step e of
>   Just e' -> eval e'
>   Nothing -> e

The "readEval" function parses a string to an expression and then evaluates it.

> readEval :: String -> Exp
> readEval e = eval (readExp e)

The "val" function returns whether or not an expression is a value.

> val :: Exp -> Bool
> val (Var x) = True
> val (Num n) = True
> val (Bool b) = True
> val (Lam x t e) = True
> val e = freeApp e

The "freeApp" function checks whether an expression is in the form
"x v_1 ... v_n", for the rule that an application of a free variable to some
values is a value.

> freeApp :: Exp -> Bool
> freeApp (App (Var x) e) = val e
> freeApp (App e1 e2) = freeApp e1 && val e2
> freeApp _ = False

The "stuck" function returns whether or not an expression is stuck, meaning
there are no reduction rules that apply but it's not a value.

> stuck :: Exp -> Bool
> stuck e =
>   case step e of
>     Just _ -> False
>     Nothing -> not (val e)
