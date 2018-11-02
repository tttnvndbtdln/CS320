This is a modified version of HW2.lhs, expanded into a tiny framework for
constructing lexers/tokenizers. You're not required to understand everything in
this file, but it'll be helpful to read through for the sake of exposure to
both Haskell and lexer construction principles. Please ask if anything is
unclear, or if anything catches your interest that you'd like to know more
about!

> module Lex where

> data Regex
>   = Epsilon         -- empty string
>   | Sing Char       -- single character
>   | Regex :.: Regex -- concatenation
>   | Regex :|: Regex -- alternation
>   | Plus Regex      -- repetition (one or more)

> infixr 2 :|:
> infixr 3 :.:

> showRegex :: Regex -> String
> showRegex Epsilon = "ε"
> showRegex (Sing c) = [c]
> showRegex (r1 :.: r2) = "(" ++ showRegex r1 ++ " • " ++ showRegex r2 ++ ")"
> showRegex (r1 :|: r2) = "(" ++ showRegex r1 ++ " | " ++ showRegex r2 ++ ")"
> showRegex (Plus r) = "(" ++ showRegex r ++ ")+"

> instance Show Regex where show = showRegex

> string :: String -> Regex
> string [] = Epsilon
> string (c:cs) = Sing c :.: string cs

> star :: Regex -> Regex
> star r = Plus r :|: Epsilon

The choice function is for an N-way alternation - it matches any one of the
items in the list.

> choice :: [Regex] -> Regex
> choice [] = error "can't choose from an empty list"
> choice [r] = r
> choice (r:rs) = r :|: choice rs

There are more kinds of whitespace out in the wild, but this'll be enough for
our purposes. The map function applies a function to every element of a list:
  map Sing [' ', '\n', '\r', '\t']
  = [Sing ' ', Sing '\n', Sing '\r', Sing '\t']

> whitespace :: Regex
> whitespace = choice (map Sing [' ', '\n', '\r', '\t'])

The range function implements a character range, like [a-z] in usual regex
syntax: it matches any character between the first char argument and the second
char argument (inclusive). The Haskell syntax [c..c'] represents an inclusive
range, and works on many different types - for example, [1..5] = [1,2,3,4,5],
and ['a'..'c'] = ['a', 'b', 'c'].  

> range :: Char -> Char -> Regex
> range c c' = choice (map Sing [c..c'])

This is a slightly more sophisticated version of the check function from HW2 -
if it finds a match in some prefix of the given string, it returns a tuple
containing both the matched prefix and the unmatched suffix. For example:

  match (Sing 'x') "xyz" == Just ("x", "yz")
  match (Sing 'x') "abc" == Nothing

  match (star (string "xy")) "xyxyz" == Just ("xyxy", "z")
  match (star (string "xy")) "ababc" == Nothing

> match :: Regex -> String -> Maybe (String, String)
> match Epsilon cs = Just ("", cs)
> match (Sing c) (c':cs) = if c == c' then Just ([c], cs) else Nothing
> match (r1 :.: r2) cs =
>   case match r1 cs of
>     Just (m, cs') ->
>       case match r2 cs' of
>         Just (m', cs'') -> Just (m ++ m', cs'')
>         Nothing -> Nothing
>     Nothing -> Nothing
> match (r1 :|: r2) cs =
>   case match r1 cs of
>     Just (m, cs') -> Just (m, cs')
>     Nothing ->
>       case match r2 cs of
>         Just (m, cs') -> Just (m, cs')
>         Nothing -> Nothing
> match (Plus r) cs =
>   case match r cs of
>     Just (m, cs') ->
>       case match (Plus r) cs' of
>         Just (m', cs'') -> Just (m ++ m', cs'')
>         Nothing -> Just (m, cs')
>     Nothing -> Nothing
> match _ _ = Nothing

A LexGrammar is a description of a lexical grammar, which is used to specify
the behavior of a lexer/tokenizer. It takes the form of a list of grammar
rules, where each rule is a regular expression paired with a function that
turns a matched string into a token (or Nothing, which tells the lexer to throw
out the input for e.g. whitespace and comments). This is a generic type
definition, where any type can be substituted in for t - for example,
LexGrammar Char = [(Regex, String -> Maybe Char)].

> type LexGrammar t = [(Regex, String -> Maybe t)]

The lexToken function attempts to lex one token from a string. If there's a
rule in the grammar that matches, it applies the function from that rule to get
a token (or Nothing), and returns that along with the rest of the string that
comes after the token (the unmatched suffix); if there are no rules that match
the input, it returns Nothing to indicate failure. Note that Maybe is serving
two different purposes here - a return value of Nothing indicates that no rules
matched the input, while a return value of Just (Nothing, cs) indicates that
some rule did match but didn't produce a token (like for a rule that discards
whitespace).

> lexToken :: LexGrammar t -> String -> Maybe (Maybe t, String)
> lexToken [] cs = Nothing
> lexToken ((r,f):rs) cs = -- r :: Regex, f :: String -> Maybe t
>   case match r cs of
>     Just (m, cs') -> Just (f m, cs') -- m :: String, f m :: Maybe t
>     Nothing -> lexToken rs cs

Finally, the tokenize function attempts to lex a string into a list of tokens.
Again, it may only be able to tokenize some prefix of the string, so if it
succeeds it returns both a token stream and any untokenized suffix that might
be left over.

> tokenize :: LexGrammar t -> String -> ([t], String)
> tokenize rs cs =
>   case lexToken rs cs of
>     Just (t, cs') ->
>       let (ts, cs'') = tokenize rs cs' in
>         case t of
>           Just t' -> (t':ts, cs'')
>           Nothing -> (ts, cs'')
>     Nothing -> ([], cs)
