This module implements the brute force parsing algorithm described in the week
4 slides. As with Lex.lhs, you're encouraged but not required to understand
this stuff; it's provided as an example implementation of the concepts we've
been studying, to illustrate how the theory translates into actual code.

> module Parse where

The Data.List module is part of the Haskell standard library, which is already
part of your Haskell installation, so you won't need any extra files to make
this work. The only function we need from it is find, which is described in
Basics.lhs from week 3.

> import Data.List (find)

A symbol is either a nonterminal or a terminal; the Symbol type is
parameterized over both the type of nonterminals and the type of terminals.

> data Symbol n t = NT n | T t

A production is a sequence of symbols.

> type Production n t = [Symbol n t]

A CFG is a list pairing nonterminals with productions.

> type CFG n t = [(n, Production n t)]

(-->) is just a slightly more convenient syntax for typing out CFG rules, to
make our Haskell implementation of them look more similar to the way we write
them on paper.

> (-->) :: n -> Production n t -> (n, Production n t)
> n --> p = (n, p)

A parse tree (in this implementation) is either a leaf terminal, an unexpanded
nonterminal, or an expanded nonterminal with a list of subtrees; it represents
some intermediate state in a derivation. For our purposes, we don't really care
which nonterminal was expanded once it's been expanded - all the data we care
about will be in the leaves of the final parse tree - so an expanded node
doesn't contain any data.

> data ParseTree n t
>   = Leaf t
>   | Unexpanded n
>   | Expanded [ParseTree n t]
>   deriving Eq

Here are some example ParseTrees in graphical notation, where a period
represents an expanded node:

  Unexpanded P

        P


  Expanded [Leaf '(', Unexpanded P, Leaf ')']

       .
      /|\
     ( P ) 


  Expanded [Leaf '(', Expanded [Unexpanded P, Leaf '&', Unexpanded P], Leaf ')']

       .
      /|\
     ( . )
      /|\
     P & P

This showParseTree function defines a slightly nicer display format for parse
trees than the default format; it's not as easy to read as the graphical
notation used above, but it's something. Trees are represented sideways, so for
example:

       .               .
      /|\    becomes     (
     ( P )               P
                         )

       .               .
      /|\                (
     ( . )   becomes     .
      /|\                  P
     P & P                 &
                           P
                         )

> showParseTree :: (Show n, Show t) => Int -> ParseTree n t -> String
> showParseTree indent (Leaf t) = replicate indent ' ' ++ show t
> showParseTree indent (Unexpanded n) = replicate indent ' ' ++ show n
> showParseTree indent (Expanded pts) =
>   concat
>     [ replicate indent ' ',
>       ".\n",
>       unlines (map (showParseTree (indent + 2)) pts) ]

> instance (Show n, Show t) => Show (ParseTree n t) where show = showParseTree 0

A concrete syntax tree is a fully expanded parse tree, representing the end of
a derivation - a tree where each leaf is a terminal.

> data CST t
>   = Token t
>   | Node [CST t]
>   deriving Eq

> showCST :: Show t => Int -> CST t -> String
> showCST indent (Token t) = replicate indent ' ' ++ show t
> showCST indent (Node csts) =
>   concat
>     [ replicate indent ' ',
>       ".\n",
>       unlines (map (showCST (indent + 2)) csts) ]

> instance Show t => Show (CST t) where show = showCST 0

Below here are a bunch of functions that operate over Symbols, Productions,
CFGs, ParseTrees, and CSTs. Many of them might seem kind of arbitrary in
isolation, but they come together at the end of the file, where a couple
high-level functions combine these smaller functions into tools for generating
CFG productions and parsing token streams.

The rulesFor function gets a list of each production that shows up on the right
hand side of a rule for a given nonterminal. The syntax "Eq n => ..." in the
type is a constraint, which says that this function only works if the
nonterminals in the input CFG can be compared with (==).

> rulesFor :: Eq n => CFG n t -> n -> [Production n t]
> rulesFor cfg n = map snd (filter (\rule -> fst rule == n) cfg)

The leaves function gets a list of all the leaves (terminals) in a parse tree.
The concatMap function used here is part of the Haskell Prelude - it maps a
function over a list to get a list of lists and then flattens the result down
to a single list.

> leaves :: ParseTree n t -> [t]
> leaves (Leaf t) = [t]
> leaves (Unexpanded n) = []
> leaves (Expanded pts) = concatMap leaves pts

Similar to the leaves function, the symbols function gets a list of all the
symbols in a parse tree, which includes terminals and unexpanded nonterminals.

> symbols :: ParseTree n t -> [Symbol n t]
> symbols (Leaf t) = [T t]
> symbols (Unexpanded n) = [NT n]
> symbols (Expanded pts) = concatMap symbols pts

The tokens function is just like the leaves function, but for finished trees
(CSTs) - it gets the list of terminals at the leaves of a CST.

> tokens :: CST t -> [t]
> tokens (Token t) = [t]
> tokens (Node csts) = concatMap tokens csts

In this implementation, a derivation is a sequence of ParseTrees. Since the
ParseTree at the end of a completed derivation has only terminals, it can be
turned into a CST without losing any information. The fullyExpanded function
checks to see if a ParseTree is fully expanded, and returns a CST if it is or
Nothing if it isn't.

The sequence function used here is also part of the Prelude - as used here, it
takes a list of Maybes and returns a list of the contents if all of them are
constructed with Just or returns Nothing otherwise. For example:

  sequence [Just 0, Just 1, Just 2]  == Just [0, 1, 2]
  sequence [Nothing, Just 1]         == Nothing
  sequence [Just 0, Nothing, Just 2] == Nothing

> fullyExpanded :: ParseTree n t -> Maybe (CST t)
> fullyExpanded (Leaf t) = Just (Token t)
> fullyExpanded (Unexpanded n) = Nothing
> fullyExpanded (Expanded pts) =
>   case sequence (map fullyExpanded pts) of
>     Just csts -> Just (Node csts)
>     Nothing -> Nothing

A finished derivation will often have a lot of nodes with only a single
subtree, which are messy to deal with; the collapse function collapses all of
those nodes into just the single subtrees. For example:

   .
   |  =>  a
   a

     .          .
    /|\        /|\
   . b .  =>  a b c
   |   |
   a   c 

> collapse :: CST t -> CST t
> collapse (Token t) = Token t
> collapse (Node [cst]) = collapse cst
> collapse (Node xs) = Node (map collapse xs)

The singletonTree function turns a Symbol into a ParseTree: a terminal becomes
a leaf, and a nonterminal becomes an unexpanded node.

> singletonTree :: Symbol n t -> ParseTree n t
> singletonTree (T t) = Leaf t
> singletonTree (NT n) = Unexpanded n

This implementation of draws is tricky code to understand, but the behavior of
the function isn't too hard to describe: it takes a list of lists and returns
every list that can be created by drawing a single item from each input list.
For example:

  draws [[1,2], [3,4]]     == [[1,3], [1,4], [2,3], [2,4]]
  draws ["abc", "de", "f"] == ["adf", "aef", "bdf", "bef", "cdf", "cef"]

The syntax [x:xs' | x <- xs, xs' <- draws xss] means "for each element x of the
list xs, and each element xs' of the list draws xss, return the value x:xs',
collecting all the returned values into a list". If you're familiar with Python
list comprehensions, this is just a slightly different syntax for them: this
comprehension could be written in pseudo-Python as [x:xs' for x in xs for xs'
in draws(xss)].

> draws :: [[a]] -> [[a]]
> draws [] = [[]]
> draws (xs:xss) = [x:xs' | x <- xs, xs' <- draws xss]

Now we're finally getting into the meat of this module. The steps function
takes a CFG and a ParseTree representing some step in a derivation and returns
the list of all ParseTrees that can be created by expanding the unexpanded
nodes according to the rules of the CFG - which is to say each possible next
step in the derivation.

The dot operator is function composition, as in mathematics: (f . g) x = f (g x). So for example:

  map (not . even) [0, 1, 2] == [not (even 0), not (even 1), not (even 2)]
                             == [not True, not False, not True]
                             == [False, True, False]

The steps function is relatively sophisticated compared to what we've seen so
far in Haskell; in particular, nested applications of map are often hard to get
a handle on. Don't worry if you don't understand the implementation, but if
you're interested in understanding it please feel free to ask!

> steps :: Eq n => CFG n t -> ParseTree n t -> [ParseTree n t]
> steps g (Leaf t) = [Leaf t]
> steps g (Unexpanded n) = map (Expanded . map singletonTree) (rulesFor g n)
> steps g (Expanded pts) = map Expanded (draws (map (steps g) pts))

The productions function implements (roughly) the procedure described in the
slides, and is the workhorse of this parsing algorithm: it returns all possible
(terminal) productions of a CFG. The first argument is a function used for
pruning the work queue, which is how the productions function knows when to
stop expanding trees (in order to keep the total number of productions finite
and minimize the work queue).

> productions :: Eq n =>
>   (ParseTree n t -> Bool) ->
>   CFG n t ->
>   ParseTree n t ->
>   [CST t]
> productions prune g pt =
>   if prune pt then
>     []
>   else
>     case fullyExpanded pt of
>       Just cst -> [collapse cst]
>       Nothing -> concatMap (productions prune g) (steps g pt)

The pruneByLength function is one possible pruning function that can be used as
the first argument to productions. It's the simple length heuristic described
in the slides: we pass in a maximum length as the first argument, and if the
ParseTree under consideration has more leaves than that length it gets pruned.

> pruneByLength :: Int -> ParseTree n t -> Bool
> pruneByLength n pt = length (leaves pt) > n

So lengthPrunedProductions is guaranteed to terminate, as explained in the
slides: there are a finite number of productions with up to N terminals.

> lengthPrunedProductions :: Eq n =>
>   Int ->
>   CFG n t ->
>   ParseTree n t ->
>   [CST t]
> lengthPrunedProductions n = productions (pruneByLength n)

In order to implement the more aggressive pruning heuristic described in the
slides where a parse tree is discarded as a candidate if the leftmost
contiguous sequence of terminals in the leaves doesn't match the same-length
prefix of the input, we need a way to get that sequence of terminals from a
ParseTree. This function does part of the work - it takes a list of symbols and
returns all of the terminals from the start up until the first nonterminal.

> prefixTerminals :: [Symbol n t] -> [t]
> prefixTerminals (T t : syms) = t : prefixTerminals syms
> prefixTerminals _ = []

The pruneByPrefix function implements that more aggressive pruning heuristic,
then.

> pruneByPrefix :: Eq t => [t] -> ParseTree n t -> Bool
> pruneByPrefix ts pt = 
>   pruneByLength (length ts) pt ||
>     let pr = prefixTerminals (symbols pt) in
>       (take (length pr) ts /= pr)

The prefixPrunedProductions function starts with some intermediate ParseTree
and returns all derivations of that ParseTree with a terminal prefix that
matches the same-length prefix of the input stream. It's still pretty slow, but
it's significantly less slow than lengthPrunedProductions in many cases.

> prefixPrunedProductions :: (Eq n, Eq t) =>
>   [t] ->
>   CFG n t ->
>   ParseTree n t ->
>   [CST t]
> prefixPrunedProductions ts = productions (pruneByPrefix ts)

Finally, we have a working parsing function! It takes a CFG and a token stream
and returns a CST for that token stream, if one exists, by generating all
possible productions of the CFG (taking the nonterminal on the left-hand side
of the first rule as the start symbol) and seeing if any one has a list of
leaves that match the input stream. Try uncommenting the
lengthPrunedProductions line and commenting out the prefixPrunedProductions
line to compare the performance of the two on different inputs.

> parse :: (Eq n, Eq t) => CFG n t -> [t] -> Maybe (CST t)
> parse g ts =
>   find
>     (\cst -> tokens cst == ts)
>     -- (lengthPrunedProductions (length ts) g (Unexpanded (fst (head g))))
>     (prefixPrunedProductions ts g (Unexpanded (fst (head g))))
