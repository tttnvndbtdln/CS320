CS 320 Principles of Programming Languages
Homework 3
Due Monday 2/19

The only Haskell file you're required to interact with for this homework is
ParseProp.lhs. The other files are dependencies that it uses to construct a
full parser:

  - Prop.lhs contains the Prop definition (it's the same file from week 1)
  - Lex.lhs contains a lexer generator function
  - PropLex.lhs contains a lexical grammar for Prop expressions
  - ParserCombinators.hs contains a minimal parser combinator library

You're encouraged to read through Lex.lhs and PropLex.lhs, to get a look at
lexer construction and usage in practice; they're each documented for
consumption.

ParserCombinators.hs uses some more advanced Haskell features that we won't
cover in this class and is left undocumented, so while you're more than welcome
to read through it and ask questions about it, you can also feel free to ignore
it. The comments in ParseProp.lhs explain how to use the relevant parser
combinators, which shouldn't require an understanding of how they're
implemented.

1. (8 points)

For each of the following, give a fixity (precedence and associativity) for the
infix operators in the string so that the string would parse as intended or
give a reason why it's impossible.

For example, if we wanted the string "A * B * C + D + E" to parse as
"((A * (B * C)) + D) + E", we could give the operators the following fixities:
  
  + is left-associative with precedence 1
  * is right-associative with precedence 2

However, the string "A + B + C + D" can't possibly parse as
"(A + B) + (C + D)", because the + operator must be left-associative,
right-associative, or non-associative, and none of those associativity choices
choice will produce that parenthesization.

In most cases there will be multiple valid answers, but you only have to give
one.

  a) "A + B * C + D" parses as "A + ((B * C) + D)"
  b) "A & B | C ^ D ^ E" parses as "(A & B) | ((C ^ D) ^ E)"
  c) "A % B @ C = D @ E % F" parses as "((A % B) @ C) = ((D @ E) % F)"
  d) "A + B * C = A * B + A * C" parses as "(A + (B * C)) = ((A * B) + (A * C))"


2. (6 points)

The week 4 slides define an unambiguous CFG for a small arithmetic language
(where "n" is taken to be any natural number):

  E --> P
  E --> E + P
  P --> A
  P --> P * A
  A --> (E)
  A --> n

In this grammar, both the + and the * operator are left-associative, and * has
a higher precedence than +. This is enforced by the rules of the grammar, which
divides arithmetic expressions into three categories:

  - E represents expressions involving the + operator, only allowing other E
    expressions on the right if they're within parentheses.

  - P represents expressions involving the * operator, only allowing other P
    expressions on the right or E expressions on either side if they're within
    parentheses.

  - A represents numeric literals or parenthesized expressions.

Here are some example leftmost derivations using this CFG:

  "E" -> "P" -> "A" -> "10"

  "E" -> "E + P" -> "P + P" -> "A + P" -> "1 + P" -> "1 + A" -> "1 + 2"

  "E" -> "E + P" -> "E + P + P" -> "P + P + P" -> "A + P + P" ->
  "1 + P + P" -> "1 + A + P" -> "1 + (E) + P" -> "1 + (P) + P" ->
  "1 + (P * A) + P" -> "1 + (A * A) + P" -> "1 + (2 * A) + P" ->
  "1 + (2 * 3) + P" -> "1 + (2 * 3) + A" -> "1 + (2 * 3) + 4"

Modify the CFG to add a new nonterminal X, representing expressions involving
an infix exponent operator (^), so that the ^ operator is right-associative and
has lower precedence than both + and *. The CFG should remain unambiguous, so
that there is exactly one valid leftmost derivation that produces any given
expression in the language. Give two example leftmost derivations that involve
the ^ operator.

3. (8 points)

Fill in the definitions of parseConjunction, parseNot, parseNegation, and
parseParens in ParseProp.lhs.


4. (6 points)

Fill in the definitions of test3, test4, and test5 in ParseProp.lhs with three
tests that demonstrate that the parser is working correctly.


5. (2 points)

The Prop grammar used in ParseProp.lhs defines both of the infix operators
(& and |) as right-associative. We could define them as left-associative
instead by changing just two rules:

  D --> C | D becomes D --> D | C
  C --> N & C becomes C --> C & N

This is a valid grammar, but a problem would come up if we tried to implement a
recursive descent parser for it by hand the way ParseProp.lhs does, with one
parsing function representing each rule and each nonterminal. What problem
would we run into, and what's the name of the transformation we could do to the
grammar to solve it (while keeping the operators left-associative)? (Hint:
there's a slide in the week 4 slides dedicated to this problem.)
