> module HW2 where

There are several different equivalent ways to define the syntax of regular
expressions; for this assignment we'll be considering the following constructs
as primitive, and building other constructs out of them.

> data Regex 
>   = Epsilon           -- empty string
>   | Sing Char         -- single character
>   | (:.:) Regex Regex -- concatenation
>   | (:|:) Regex Regex -- alternation
>   | Plus Regex        -- repetition (one or more)

Haskell allows us to specify the associativity and precedence of custom
operators with the "infixl" (left associative) and "infixr" (right
associative) commands; the number given is the precedence, where higher
numbers bind tighter. It's conventional for the concatenation operator to bind
tighter than the alternation operator in regular expression syntax, so that
e.g. "Sing 'a' :.: Sing 'b' :|: Sing 'c' :.: Sing 'd'" parses as
"(Sing 'a' :.: Sing 'b') :|: (Sing 'c' :.: Sing 'd')".

> infixr 2 :|:
> infixr 3 :.:

As with the "Prop" syntax definition from week 1, we can define functions that
operate on Regex values by giving a case for each constructor; here we define
a "pretty printing" function to make Regex values a little more readable. It's
somewhat nontrivial to build a pretty printing function that only shows
parentheses when they're required to avoid ambiguity, so this function
conservatively wraps every binary operator application in parentheses.

In Haskell, a String is simply a list of characters, so to turn a Char into a
String all we have to do is create a list that contains only the Char. (++) is
the append function for lists.

> showRegex :: Regex -> String
> showRegex Epsilon = "ε"
> showRegex (Sing c) = [c]
> showRegex (r1 :.: r2) = "(" ++ showRegex r1 ++ " • " ++ showRegex r2 ++ ")"
> showRegex (r1 :|: r2) = "(" ++ showRegex r1 ++ " | " ++ showRegex r2 ++ ")"
> showRegex (Plus r) = "(" ++ showRegex r ++ ")+"

For the purposes of this course, all you need to know about this "instance"
construct is that it lets us tell the REPL (GHCi/Hugs) how to display a data
type. (If you're interested, "Show" is an example of a typeclass, which is
Haskell's mechanism for ad-hoc overloading - we'll come back to what "ad-hoc
overloading" means later in the course, but to a first approximation a
typeclass is similar to an interface/abstract class in OOP.)

> instance Show Regex where show = showRegex

The obvious application of a programmatic definition of regular expression
syntax is to build a function that matches strings against a regex - which is
to say a function that checks whether or not an input string is a member of
the language specified by a given regex. The function we'll define here is
actually a little more general: it checks whether any prefix of the input
string is a member of the specified language, and returns the rest of the
string (the unmatched suffix) if it finds a match. For example, the regex
"Sing 'a' :.: Sing 'b'" matched against the string "abcd" would return a
suffix of "cd".

Haskell has no "null" value; when a function says that it returns a String,
it's required to return an actual String value and not the absence of a
string. The Maybe type lets us write functions that may or may not return a
value. It has two constructors, Just and Nothing: Just takes a single argument
(in this case a String) and represents the presence of a value, and Nothing,
as you might imagine, takes no arguments and represents the absence of a
value.

Your job is to fill in the cases for Sing and (:.:). For the Sing case, note
that the input string is split into a head character (c') and a tail string
(cs). This works because (:) is a constructor of List; you might imagine that
we could split the input to the (:.:) case similarly with (++), but this isn't
possible in Haskell because (++) is not a constructor. Instead, your
implementation of the (:.:) case will probably look similar to the given
implementation of the (:|:) case, pattern matching on the result of a
recursive call to "check".

> check :: Regex -> String -> Maybe String 
> check Epsilon cs = Just cs
> check (Sing c) (c':cs) =
>   if c == c'
>     then Just cs
>   else Nothing
> check (r1 :.: r2) cs =
>   case check r1 cs of
>     Just cs' -> check r2 cs'
>     Nothing -> Nothing
> check (r1 :|: r2) cs =
>   case check r1 cs of
>     Just cs' -> Just cs'
>     Nothing -> check r2 cs
> check (Plus r) cs =
>   case check r cs of
>     Just cs' -> case check (Plus r) cs' of
>       Just cs'' -> Just cs''
>       Nothing -> Just cs'
>     Nothing -> Nothing
> check _ _ = Nothing

Another useful property of our regex representation is that we can generate
regular expressions as the output of functions, which lets us add new
constructs to the language of regular expressions without extending the
original definition (like the definition of "xor" in the Prop language in week
1). For example, this function constructs a regex that matches exactly a given
string. (Try calling it in the REPL with some different inputs to get a feel
for how it works.)

> string :: String -> Regex
> string [] = Epsilon
> string (c:cs) = Sing c :.: string cs

If your implementation of the Sing and (:.:) cases of "check" are correct, all
of these tests should evaluate to True in your REPL.

> regex1  = Sing 'a' :.: Plus (Sing 'b') :.: Sing 'c'
> test1_1 = check regex1 "abbbc" == Just ""
> test1_2 = check regex1 "abcd"  == Just "d"
> test1_3 = check regex1 "acb"   == Nothing

> regex2  = string "ab" :|: string "cd"
> test2_1 = check regex2 "abcd" == Just "cd"
> test2_2 = check regex2 "cdab" == Just "ab"
> test2_3 = check regex2 "acd"  == Nothing

We can also write functions to build regular expressions out of other regular
expressions. Our base syntax contains the Plus constructor to match one or
more repetitions of a regex; define the Kleene star operator here to match
zero or more repetitions of the given regex.

> star :: Regex -> Regex
> star r = (Plus (r))

star r = Plus r :|: Epsilon

Below here, write at least three more regexes and at least two tests for each
that demonstrate that "check" and "star" are working correctly.

> regex3 = Plus (string "hello")
> test3_1 = check regex3 "hellohello" == Just ""
> test3_2 = check regex3 "hellobobhello" == Just "bobhello"
> test3_3 = check regex3 "world" == Nothing

> regex4 = string "dogs"
> test4_1 = check regex4 "dogs" == Just ""
> test4_2 = check regex4 "" == Nothing

> regex5 = string "cats"
> test5_1 = check regex5 "cats" == Just ""
> test5_2 = check regex5 "catsdogs" == Just "dogs"
