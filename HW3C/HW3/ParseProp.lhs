This module implements a recursive descent parser for a grammar for Prop with
infix operators (the same one as in PropCFG.lhs from the week 4 material):

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

You're responsible for filling out a couple definitions in the file, to finish
the parser. 

> module ParseProp where

The ParserCombinators module contains some definitions that enable the parser
syntax we'll be using in this file; it's effectively a very minimal subset of
the parsers in the standard Parsec library for Haskell. You definitely don't
have to understand the contents of it - it involves several Haskell features
that we won't be covering in this class - but please ask if you're interested
in how it works!

In order to import these modules, you'll need to be running your interpreter
from within the HW3 folder:

  cd <path to HW3 folder>
  ghci ParseProp.lhs

> import ParserCombinators
> import Prop
> import PropLex

This function is the goal of this module - once all the definitions in here are
implemented, readProp will be able to parse a String in concrete Prop syntax
into a Prop value. (This is a good time to mention that the definitions in a
Haskell file can be in any order: readProp depends on parseDisjunction, which
is defined below.)

Note that readProp calls parseDisjunction to parse the input token stream,
because D is the start symbol for our CFG. The runParser function takes a
parser and an input stream and executes the parser using that input.

> readProp :: String -> Prop
> readProp cs =
>   case runParser parseDisjunction (lexProp cs) of
>     Just (p, []) -> p
>     Just (p, ts) -> error ("leftover tokens after parsing: " ++ show ts)
>     Nothing -> error ("couldn't parse: " ++ cs)

Here are a couple tests to see if the parser is working correctly; if the
definitions you fill in below are correct, these should all evaluate to True
when run in the REPL.

> test1 = readProp "A & B" == AND (IN "A") (IN "B")
> test2 = readProp "!!A | B & C | D" == OR (NOT (NOT (IN "A"))) (OR (AND (IN "B") (IN "C")) (IN "D"))

After filling in the rest of the file, write out three more tests here that
demonstrate that the parser works as intended.

> test3 = readProp "A & B | C & D" == OR (AND (IN "A") (IN "B")) (AND (IN "C") (IN "D")) --error "test1 unimplemented"
> test4 = readProp "!A & B | C & !D" == OR (AND (NOT (IN "A")) (IN "B")) (AND (IN "C") (NOT (IN "D"))) --error "test2 unimplemented"
> test5 = readProp "!A | !!B | !C" == OR (NOT (IN "A")) (OR (NOT (NOT (IN "B"))) (NOT (IN "C")))  --error "test3 unimplemented"

Every definition below here is a Prop parser, with type Parser PropTok Prop: a
parser that operates on a stream of PropTok tokens and returns a Prop value.
Since Haskell can infer the types of any definitions we give it, we don't need
to bother giving each one a type annotation.

The "do" construct used in this module is used for a lot of different things in
Haskell, but in this module we're only using it to write parsers; you can think
of it as an implementation of the pseudocode syntax used in the recursive
descent parser code in the week 4 slides. The current position in the input
stream (the "i" variable in the pseudocode in the slides) is left implicit; the
functions from the ParserCombinators library manage it behind the scenes.

parseOr implements the single rule D --> C | D. The definition says:
  
  - parse a conjunction AST and name it "p"
  - ensure that the next symbol in the stream is T_OR
  - parse a disjunction AST and name it "q"
  - return the AST "AND p q"

> parseOr = do
>   p <- parseConjunction
>   eat T_OR
>   q <- parseDisjunction
>   return (OR p q)

parseDisjunction implements the CFG rules for the D nonterminal:

  D --> C | D
  D --> C

> parseDisjunction = parseOr <|> parseConjunction

parseAnd implements the rule C --> N & C.

> parseAnd = do
>   p <- parseNegation
>   eat T_AND
>   q <- parseConjunction
>   return (AND p q)

parseConjunction should implement the rules for the C nonterminal:

  C --> N & C
  C --> N

It will probably look almost identical to parseDisjunction, but with different
parsers being combined.

> parseConjunction = parseAnd <|> parseNegation --error "parseConjunction unimplemented"

parseNot should implement the rule N --> !N.

> parseNot = do --error "parseNot unimplemented"
>   eat T_NOT
>   p <- parseNegation
>   return (NOT p) 

parseNegation should implement the rules for the N nonterminal:

  N --> !N
  N --> A

> parseNegation = parseNot <|> parseAtom  --error "parseNegation unimplemented"

parseParens should implement the rule A --> (D).

> parseParens = do -- error "parseParens unimplemented"
>   eat T_LPAREN
>   p <- parseDisjunction
>   eat T_RPAREN
>   return p 

parseTrue implements the rule A --> TRUE.

> parseTrue = do
>   eat T_TRUE
>   return TRUE

parseFalse implements the rule A --> FALSE.

> parseFalse = do
>   eat T_FALSE
>   return FALSE

parseLit implements the rules for boolean literals:

  A --> TRUE
  A --> FALSE

> parseLit = parseTrue <|> parseFalse

parseInput implements the rule for parsing an input name, A --> IN c. Since
we're dealing with a token stream, not a raw character stream, an input name is
only one token regardless of how long the string used for the name is. The
"advance" parser returns the token at the current position in the stream and
advances to the next position.

> parseInput = do
>   t <- advance
>   case t of
>     T_IN i -> return (IN i)
>     _ -> giveUp

parseAtom implements the rules for the A nonterminal:

> parseAtom = parseParens <|> parseLit <|> parseInput
