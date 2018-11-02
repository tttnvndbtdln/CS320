This module defines the context-free grammar for a concrete syntax for Prop,
using the tokens from PropLex.lhs.

> module PropCFG where

> import Lex
> import Parse
> import Prop
> import PropLex

This Prop grammar is factored to eliminate ambiguity - the ! operator has a
lower precedence than the | operator, which has a lower precedence than the &
operator, and both binary operators are right-associative, so for example the
expression "!A | !!B & C & D" will parse as "(!A) | ((!!B) & (C & D))".

To this end, there are four nonterminals:

> data PropNT = A | N | C | D deriving (Eq, Show)
  
- An atom (A) is a literal, an input, or a parenthesized disjunction.

- A negation (N) is an atom or ! applied to an negation.

- A conjunction (C) is a negation or & applied to a left conjunction and a
  right negation.

- A disjunction (D) is a conjunction or | applied to a left disjunction and a
  right conjunction.

This mirrors the factoring of the Arithmetic grammar in the week 3 slides to
avoid ambiguity, just with a different set of operators.

Here's the whole CFG in traditional syntax (where 'c' is understood to be any
single upper-case letter):

  D --> C | D
  D --> C
  C --> N & C
  C --> N
  N --> !N
  N --> A
  A --> (D)
  A --> TRUE
  A --> FALSE
  A --> IN c

> propCFG :: CFG PropNT PropTok
> propCFG =
>   [ D --> [NT C, T T_OR, NT D],
>     D --> [NT C],
>     C --> [NT N, T T_AND, NT C],
>     C --> [NT N],
>     N --> [T T_NOT, NT N],
>     N --> [NT A],
>     A --> [T T_LPAREN, NT D, T T_RPAREN],
>     A --> [T T_TRUE],
>     A --> [T T_FALSE] ] ++
>   [ A --> [T (T_IN [x])] | x <- ['A'..'Z'] ]

The last line of propCFG is shorthand for 26 individual rules:

  [A --> [T (T_IN [x])] | x <- ['A'..'Z']] == [ A --> T (T_IN "A"),
                                                A --> T (T_IN "B"),
                                                ...,
                                                A --> T (T_IN "Z") ]

Now, having defined both a lexical grammar and a CFG for Prop, we can parse a
string to a concrete syntax tree (CST).

> parsePropCST :: String -> CST PropTok
> parsePropCST cs =
>   case parse propCFG (lexProp cs) of
>     Just cst -> cst
>     Nothing -> error ("couldn't parse: " ++ cs)

We're almost done! But that's not quite all - a CST with PropToks at the leaves
isn't quite the same thing as a Prop value (an AST). In order to bridge that
gap, we have to drop some detail from a CST. For a couple examples in graphical
notation, where p/q are arbitrary subtrees (CSTs) and [p] is the AST obtained
by dropping some detail from the CST p:

    .
    |   =>    IN "A"
    A

    .
   /|\  =>    [p]
  ( p )

    .         NOT
   / \  =>     |
  !   p       [p]

    .         AND
   /|\  =>   /   \
  p & q    [p]   [q]

Assuming our CFG is correct, the seven cases of this function should cover
every possible output of parsePropCST. The last case, the error case, is
included just as a sanity check.

> cstToAst :: CST PropTok -> Prop
> cstToAst (Token T_TRUE) = TRUE
> cstToAst (Token T_FALSE) = FALSE
> cstToAst (Token (T_IN x)) = IN x
> cstToAst (Node [Token T_LPAREN, p, Token T_RPAREN]) = cstToAst p
> cstToAst (Node [p, Token T_AND, q]) = AND (cstToAst p) (cstToAst q)
> cstToAst (Node [p, Token T_OR, q]) = OR (cstToAst p) (cstToAst q)
> cstToAst (Node [Token T_NOT, p]) = NOT (cstToAst p)
> cstToAst cst = error ("invalid Prop CST: " ++ show cst)

Finally, we have a parser!

> bruteForceParseProp :: String -> Prop
> bruteForceParseProp cs = cstToAst (parsePropCST cs)

Try it out! With this file loaded in your REPL, you should be able to enter,
for example:

  bruteForceParseProp "!A & B"

Food for thought: why does "!!!!!!!!!!A" parse almost immediately, while
"A & A & A & A & A & A", which has the same number of terminals, parses
extremely slowly? (Hint: how does the "aggressive" pruning heuristic described
in the slides apply to each of these examples?)
