> module Prop where

This is a "literate Haskell" source file, intended to accompany the
Week 1 lectures for CS 320, Principles of Programing Languages in
Winter 2018.

The term "literate Haskell" means that this is a text file in which
the lines of Haskell code are marked explicitly by a > character
in the first column. All other lines are treated as comments.
Note also that the literate Haskell file format requires (at least)
one blank line between sections of code and sections of plain text.

Although you are welcome to read and study the contents of this file if
you wish, you are not expected to do that, or to understand all of the
included Haskell code, especially at this early stage of the course.
Instead, if you have downloaded this file to your CS Linux account,
then you should be able to start up a Haskell interpreter and start
typing in expressions to be evaluated by using the following command,
substituting either "ghci" or "hugs" (without quotes) for INTERP:

  INTERP Prop.lhs

-- Abstract syntax for propositions: ---------------------------------

> data Prop = AND Prop Prop
>           | OR  Prop Prop
>           | NOT Prop
>           | TRUE
>           | FALSE
>           | IN String
>           deriving (Eq, Show)

Here are some functions that define the label and subtrees of each
node in a Prop abstract syntax tree.

> label :: Prop -> String
> label (AND p q) = "AND"
> label (OR p q)  = "OR"
> label (NOT p)   = "NOT"
> label TRUE      = "TRUE"
> label FALSE     = "FALSE"
> label (IN v)    = v

> subtrees :: Prop -> [Prop]
> subtrees (AND p q) = [p, q]
> subtrees (OR p q)  = [p, q]
> subtrees (NOT p)   = [p]
> subtrees TRUE      = []
> subtrees FALSE     = []
> subtrees (IN v)    = []

-- Some examples using the abstract syntax constructors: -------------

> expr :: Prop
> expr =
>   OR
>     (AND (NOT (IN "A")) (IN "B"))
>     (AND (IN "A") (IN "B"))

> sharing0 :: Prop
> sharing0 =
>   xor
>     (OR (IN "A") (AND (IN "B") (IN "C")))
>     (OR (AND (IN "B") (IN "C")) (IN "D"))

> sharing1 :: Prop
> sharing1 =
>   let s = AND (IN "B") (IN "C")
>   in xor (OR (IN "A") s) (OR s (IN "D"))

-- Computing over abstract syntax trees: -----------------------------
The following function calculates the list of all inputs
appearing in a given AST:

> vars :: Prop -> [String]
> vars (AND p q) = vars p ++ vars q
> vars (OR p q)  = vars p ++ vars q
> vars (NOT p)   = vars p
> vars TRUE      = []
> vars FALSE     = []
> vars (IN v)    = [v]

-- Abstraction: ------------------------------------------------------
The Prop type does not include an XOR node, but we can fill
that gap by defining our own xor function:

> xor :: Prop -> Prop -> Prop
> xor p q = OR (AND p (NOT q)) (AND (NOT p) q)

Half and full adders (https://en.wikipedia.org/wiki/Adder_(electronics)):

> half :: Prop -> Prop -> (Prop, Prop)
> half a b =
>   let
>     s = xor a b
>     c = AND a b
>   in
>     (s, c)

> full :: Prop -> Prop -> Prop -> (Prop, Prop)
> full a b cin =
>   let
>     (s1, c1) = half a b
>     (s,  c2) = half cin s1
>     cout     = OR c1 c2
>   in
>     (cout, s)

A four bit adder:

> adder4 ::
>   (Prop, Prop, Prop, Prop) ->
>   (Prop, Prop, Prop, Prop) ->
>   Prop ->
>   (Prop, (Prop, Prop, Prop, Prop))
> adder4 (a3, a2, a1, a0) (b3, b2, b1, b0) c0 =
>   let
>     (c1, s0) = full a0 b0 c0
>     (c2, s1) = full a1 b1 c1
>     (c3, s2) = full a2 b2 c2
>     (c4, s3) = full a3 b3 c3
>   in
>     (c4, (s3, s2, s1, s0))

A generalized N-bit adder:

> adder :: [Prop] -> [Prop] -> Prop -> (Prop, [Prop])
> adder []     []     cin = (cin, [])
> adder (a:as) (b:bs) cin =
>   let
>     (c, ss)   = adder as bs cin
>     (cout, s) = full a b c
>   in
>     (cout, s:ss)

-- Environments: -----------------------------------------------------
Environments are mappings from input identifiers to (Boolean) values;
we need to specify an environment when we evaluate an expression, or
else we won't know what values to use for any inputs that appear
in the expression.

We will represent environments here as a simple list of pairs,
each of which includes a string (naming a input) and a Bool
(specifying the value of the input). This data structure is
also commonly referred to as an "association list":

> type Env = [(String, Bool)]

Some simple examples:

> env :: Env
> env = [("A", True), ("B", False)]

> simpleEnv :: Env
> simpleEnv =
>   [ ("A", True),
>     ("B", False),
>     ("C", False),
>     ("D", True) ]

----------------------------------------------------------------------
Evaluation of a proposition in a given environment produces
a Boolean result:

> eval :: Prop -> (Env -> Bool)
> eval (AND p q) env = eval p env && eval q env
> eval (OR p q)  env = eval p env || eval q env
> eval (NOT p)   env = not (eval p env)
> eval TRUE      env = True
> eval FALSE     env = False
> eval (IN v)    env =
>   case lookup v env of
>     Just b  -> b
>     Nothing -> error ("No definition for " ++ v)
