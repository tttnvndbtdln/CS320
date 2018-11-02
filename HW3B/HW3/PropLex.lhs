This module defines a lexical grammar for a concrete syntax for Prop. The week
4 lecture slides cover the concrete syntax with prefix operators that we've
been using in Haskell (e.g. "AND (OR TRUE FALSE) (NOT TRUE)"); in this file and
ParseProp.lhs we're going to be defining a more conventional infix syntax (e.g.
"(TRUE | FALSE) & !TRUE"). Then, by applying the lexer generator from Lex.lhs
to this grammar, we'll have a working tokenizer for the infix Boolean syntax we
all know and love!

> module PropLex where

For these imports to work, you'll need Lex.lhs in the same folder as this file;
when you start the interpreter, you'll need to start it from the folder
containing all these files.

> import Lex

A PropTok is a token in the grammar of the Prop language. They're all prefixed
with T_ to avoid name collisions, since we're already using the names
TRUE/FALSE/AND/etc. for Prop constructors. Almost all of these tokens classify
just a single lexeme - T_LPAREN is exactly "(", T_AND is exactly "&", etc. -
with the exception of T_IN, which represents an input name and gets tagged with
the String value representing the name.

> data PropTok
>   = T_LPAREN
>   | T_RPAREN
>   | T_AND
>   | T_OR
>   | T_NOT
>   | T_TRUE
>   | T_FALSE
>   | T_IN String
>   deriving Eq

> showPropTok :: PropTok -> String
> showPropTok T_LPAREN = "("
> showPropTok T_RPAREN = ")"
> showPropTok T_AND = "&"
> showPropTok T_OR = "|"
> showPropTok T_NOT = "!"
> showPropTok T_TRUE = "TRUE"
> showPropTok T_FALSE = "FALSE"
> showPropTok (T_IN x) = x

> instance Show PropTok where show = showPropTok

The definition of LexGrammar is in Lex.lhs. Here we give a lexical grammar for
Prop in the form of a list of rules, where each rule is a regular expression
(to classify a token type) paired with a function to turn a matched lexeme
(String) into a token. Since most of the tokens in this grammar have no
attributes and can only match exactly one lexeme, most of the functions ignore
their input; the one exception is the rule for T_IN, which uses the matched
lexeme as the token attribute. The other thing to note here is that the
function for the whitespace rule returns Nothing, which throws away whitespace
without adding any tokens to the input stream.

> propLexGrammar :: LexGrammar PropTok
> propLexGrammar =
>   [ (Sing '(',             \cs -> Just T_LPAREN),
>     (Sing ')',             \cs -> Just T_RPAREN),
>     (Sing '&',             \cs -> Just T_AND),
>     (Sing '|',             \cs -> Just T_OR),
>     (Sing '!',             \cs -> Just T_NOT),
>     (string "TRUE",        \cs -> Just T_TRUE),
>     (string "FALSE",       \cs -> Just T_FALSE),
>     (Plus (range 'A' 'Z'), \cs -> Just (T_IN cs)),
>     (Plus whitespace,      \cs -> Nothing) ]

Now that we have a lexical grammar for Prop, we can use the tokenize function
from Lex.lhs to construct a tokenizer. The tokenize function tokenizes as much
of the input string as it can, but it might not be able to tokenize the whole
thing, so it returns back both a list of tokens and whatever's left of the
string; here we treat it as an error if there's any leftover text that couldn't
be tokenized.

> lexProp :: String -> [PropTok]
> lexProp cs =
>   case tokenize propLexGrammar cs of
>     (ts, "") -> ts
>     (ts, cs') -> error ("leftover text after tokenizing: " ++ cs')

Try it out! With this file loaded in your REPL, you should be able to enter,
for example:

  lexProp "A & !B | !(C & D)"
