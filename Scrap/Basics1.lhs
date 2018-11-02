This is a tweaked and commented version of the file produced during the Haskell
lecture on Wednesday 1/25, with a couple extra examples to (hopefully) clarify
some of the concepts.

The Prelude is the big pile of definitions that Haskell imports by default,
which includes Bool, List, Maybe, and a whole host of other stuff. Since we're
going to be defining List and Maybe ourselves in this file and doing very basic
things with them, we only really want to import a few things from the Prelude -
this NoImplicitPrelude compiler directive tells Haskell to import nothing by
default, and then the import line below specifies the small handful of Prelude
imports that we do want.

> {-# OPTIONS -XNoImplicitPrelude #-}

> module Basics where

These are a couple things that we're going to use but can't or don't want to
define for ourselves:

  - Importing Show lets us write "deriving Show", which tells Haskell that we
    want our data types to be printable. We can't define this ourselves because
    it's a special construct in the language - when the REPL goes to print a
    value, it uses this specific Prelude Show type whether or not it's in
    scope, so we couldn't define an exact copy of it ourselves even if we
    wanted to.

  - Int is a primitive machine type, which can't be exactly defined as a
    Haskell datatype. That's a little bit of a lie, really - since Int is
    finite, we could define it with one constructor for each possible int
    value, but this file would be massive and the vast majority of it would be
    extremely uninteresting. We could also define it from mathematical first
    principles, like they do in number theory, but that's a rabbit hole that's
    irrelevant to this course.

  - We can define the Bool type ourselves, as shown below, but the built-in
    (==) operator returns a Prelude Bool, so ours can't quite replace it for
    all purposes and we might as well import the Prelude one. The syntax
    "Bool(..)" means "import the Bool type and all of its constructors".

> import Prelude (Show, Int, (+), (*), (<), mod, (==), Bool(..))

Bool is defined like this:

  data Bool = True | False deriving Show

This says that Bool is a type with two constructors, True and False, and we
want values of Bool to print in the REPL in the obvious way ("True" and
"False"). Type names and constructors always start with an upper-case letter in
Haskell, and variables always start with a lower-case letter; the
compiler/interpreter enforces that. If you use an infix operator as a
constructor, it's required that it starts with a colon, like the (:.:) and
(:|:) operators in HW2.lhs.

Functions in Haskell are usually defined by pattern matching on the arguments.
The first line of this definition gives the type of the function, and the next
two lines define the behavior of the function. All three lines taken together
constitute a single function definition; the two = signs mean that there are
two cases in this definition, not that there are two functions here.

> not :: Bool -> Bool
> not True  = False
> not False = True

That definition actually expands to this one below (not') in the internals of
the compiler.  It's up to you which style you like better; the above style is
more common in Haskell programs and often considered more idiomatic Haskell,
but you're not required to use it. The case expression is kind of like a
generalized if/then/else expression, but since we're working with Bools here
it's acting exactly as an if/then/else.

The apostrophe in the name doesn't mean anything special - apostrophes are just
an identifier character in Haskell and can show up anywhere in an identifier
except the first character. It's common in mathematics to use the notation x'
(pronounced "x prime") for some value derived from something called x; in this
case it's just used to distinguish this function from the one above, since we
can't have two different definitions with the same name in one file.

> not' :: Bool -> Bool
> not' b = case b of
>   True  -> False
>   False -> True

An underscore in a pattern says "I don't care what this argument is". It's a
little special in that it can't show up on the right hand side of a definition,
only in a pattern, and one pattern can have multiple underscores if there are
multiple things it's ignoring.

> and :: Bool -> Bool -> Bool
> and False _     = False
> and _     False = False
> and True  True  = True

> or :: Bool -> Bool -> Bool
> or True  _     = True
> or _     True  = True
> or False False = False

Infix operators are really just regular functions (prefix operators) in
disguise. When you write "x && y", that gets transformed to "(&&) x y" in an
intermediate stage of the compiler/interpreter. Probably the most intuitive way
to define the (&&) operator, then, is like this:

  (&&) :: Bool -> Bool -> Bool
  x && y = and x y

But we can do a little better: notice that the type of (&&) is
Bool -> Bool -> Bool, the same as the type of "and". Functions are values like
any other values in Haskell, so we can just assign the identifier (&&) to the
"and" function itself!
  
> (&&) :: Bool -> Bool -> Bool
> (&&) = and

> (||) :: Bool -> Bool -> Bool
> (||) = or

Haskell has built-in syntax for if/then/else, but this is a good excuse to
introduce generic types. Since concrete types always start with an upper-case
letter, any identifier in a type that starts with a lower-case letter is
treated as a generic type variable.

In Java, this function's signature would look something like:
  A ifThenElse<A>(bool b, A x, A y)

And in C++:
  template <typename A>
  A ifThenElse(bool b, A x, A y)

In other words, the "a" type variable is specialized to some concrete type when
the arguments get passed in.

> ifThenElse :: Bool -> a -> a -> a
> ifThenElse True  x y = x
> ifThenElse False x y = y

Lists in Haskell are singly-linked, so every list is either the empty list or a
head element paired with a tail list. This is a generic type, like ifThenElse
is a generic function - there's a type "List X" for any type "X", like List Int
or List Bool.

> data List a = Nil | Cons a (List a) deriving Show

The Prelude defines special syntax for lists - the Nil constructor looks like
"[]", the Cons constructor is the infix operator (:), and the syntax [a, b, c]
translates to a : (b : (c : [])). We can't get that syntax for ourselves with a
user-defined type, so we'll be using these slightly uglier (but traditional)
names.

> ints :: List Int
> ints = Cons 1 (Cons 2 (Cons 3 Nil)) -- [1,2,3]

We define recursive functions on lists by pattern matching: if we define what a
function does to an empty list and what it does to a nonempty list, it can act
on any possible list. When pattern matching on a nonempty list of type "List a"
with the pattern "Cons x xs", the variable "x" is the head of type "a", and the
variable "xs" is the tail of type "List a" (as specified in the List
definition).

> length :: List a -> Int
> length Nil = 0
> length (Cons x xs) = 1 + length xs

The map function applies a function to each element of a list, so that
"map f [a, b, ..., z]" evaluates to "[f a, f b, ..., f z]". The first argument
is a function, and determines the concrete types that get substituted in for
a/b.

> map :: (a -> b) -> List a -> List b
> map f Nil = Nil
> map f (Cons x xs) = Cons (f x) (map f xs)

> inc :: Int -> Int
> inc x = x + 1

Try out "map inc ints" in the interpreter. This is a good time to note that we
can also apply only some of the arguments to a function - that's why we've been
separating arguments with arrows this whole time. It's subtle, but follow the
types closely:

  map :: (a -> b) -> List a -> List b
  inc :: Int -> Int
  map inc :: List Int -> List Int
  map inc ints :: List Int

Each of these is a valid expression in Haskell that can be used anywhere its
type is valid. For example:

> incList :: List Int -> List Int
> incList = map inc

The filter function takes a function argument, like map, and uses it to decide
which elements to remove from a list. The first argument is a predicate (a
function from some type to Bool) which takes in an element of the list and
returns True if it should be kept in the output list and False if it should be
thrown away.

> filter :: (a -> Bool) -> List a -> List a
> filter f Nil = Nil
> filter f (Cons x xs) =
>   if f x then
>     Cons x (filter f xs)
>   else
>     filter f xs

Try out "filter even ints" for an example. As with map, we can partially apply
"filter" to get a specialized function:

> even :: Int -> Bool
> even x = mod x 2 == 0

> filterEvens :: List Int -> List Int
> filterEvens = filter even

We can also write functions without giving them names, called "anonymous
functions" or sometimes "lambda expressions" or just "lambdas". (They come from
an old logical system called "lambda calculus", which we'll study a bit in the
later weeks of this course, but that's just history and isn't required
knowledge in order to use lambdas in Haskell.) The syntax "\x -> y" means "a
function with an argument named x that returns the expression y", and multiple
arguments are separated with spaces, like "\x y -> z". For example, this is
another way to write the inc function:

> inc' :: Int -> Int
> inc' = \x -> x + 1

The utility of this is that when filtering or mapping over a list, we often use
small single-use functions that don't really deserve names. For example, here's
a function that filters out all numbers less than 10 from a list:

> filterLt10 :: List Int -> List Int
> filterLt10 = filter (\x -> x < 10)

The expression "\x -> f x" is always equivalent to "f" for any function f.
(This is called eta-reduction, and we'll get to it more formally during the
lambda calculus stuff in a few weeks; for now it's just a way to make your code
a little shorter and easier to read sometimes.)

The Maybe type can be seen in a couple different ways: it's either the absence
of a value or the presence of a value, which is effectively equivalent to
saying it's a list with either zero or one elements. It's usually used to add a
"failure" option to a function, since there's no "null" in Haskell.

> data Maybe a = Nothing | Just a deriving Show

Nothing is a Maybe for any type, since the absence of something might be the
absence of anything.

> noInt :: Maybe Int
> noInt = Nothing

Just constructs a Maybe X out of an X, for some type X. Note that "10" isn't
the same as "Just 10" - in order to make a Maybe, we have to explicitly
construct it with either Just or Nothing.

> someInt :: Maybe Int
> someInt = Just 10

The find function works kind of like filter, except it returns just the first
element that matches a predicate. Since there might be no elements in the list
that match, it returns a Maybe: "Just x" if some x is found that matches the
predicate, and "Nothing" otherwise.

> find :: (a -> Bool) -> List a -> Maybe a
> find f Nil = Nothing
> find f (Cons x xs) =
>   if f x then
>     Just x
>   else
>     find f xs

findLt3, for example, finds the first element in a list of Ints that's less
than 3.

> findLt3 :: List Int -> Maybe Int
> findLt3 = find (\x -> x < 3)

We can pattern match on Maybe arguments just like with List arguments, by
giving a case for each possible constructor. Note that the variables introduced
in a pattern expression (the stuff to the left of an "=" in a function
definition or a "->" in a case expression) are only bound in the right hand
side of that definition, so for example there's no variable "x" in scope in the
third case. This function adds two numbers, interpreting Nothing as zero.

> addMaybe :: Maybe Int -> Maybe Int -> Int
> addMaybe Nothing  Nothing  = 0
> addMaybe (Just x) Nothing  = x
> addMaybe Nothing  (Just y) = y
> addMaybe (Just x) (Just y) = x + y

We're not restricted to pattern matching on arguments - the case expression
lets us pattern match on any expression. This findPlus function takes in two
predicates over Ints and returns the first element in the given list that
matches the first predicate added to the first element that matches the second
predicate. If either predicate doesn't match any element, the whole operation
fails; specifically, we test the first predicate and fail immediately if it
fails or move onto the second predicate if it succeeds.

> findPlus :: (Int -> Bool) -> (Int -> Bool) -> List Int -> Maybe Int
> findPlus f g xs =
>   case find f xs of            -- try the first find
>     Just x ->                  -- case where first find succeeded
>       case find g xs of        -- try the second find
>         Just y -> Just (x + y) -- case where both finds succeeded
>         Nothing -> Nothing     -- case where only the first find succeeded
>     Nothing -> Nothing         -- case where the first find failed

> addFirstOddToFirstEven :: List Int -> Maybe Int
> addFirstOddToFirstEven = findPlus even (\x -> not (even x))

There's a similar principle to eta-expansion ("\x -> f x" == "f") with case
expressions: the expression

  case x of
    Just y -> Just y
    Nothing -> Nothing

is always equivalent to "x" for any Maybe value "x", since the case expression
is saying "if this is something of form 'Just y' then return it as-is and if
it's something of the form 'Nothing' then also return it as-is".
