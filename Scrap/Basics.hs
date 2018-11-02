module Basics where

import Prelude(Show, Int, (+), (*), (mod), Eq, (==), Bool(..))

--data Bool = True | False deriving Show

not :: Bool -> Bool
not True = False
not False = True

not' :: Bool -> Bool
not' b = case b of 
  True -> False
  False -> True

and :: Bool -> Bool -> Bool
and False b = False
and b False = False
and True True = True

or :: Bool -> Bool -> Bool
or True b = True
or b True = True
or False False = False

(&&) :: Bool -> Bool -> Bool
False && b = False
b && False = False
True && True = True
--(&&) = and

(||) :: Bool -> Bool -> Bool
(||) = or

ifThenElse :: Bool -> Bool -> Bool -> Bool
ifThenElse True x y = x
ifThenElse False x y = y

data Unit = Unit deriving Show

data List a = Nil | Cons a (List a) deriving Show -- a is head

length :: List a -> Int
length Nil = 0
length (Cons x xs) = 1 + length xs

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

ints :: List Int
ints = Cons 1 (Cons 2 (Cons 3 Nil)) -- [1,2,3] 

inc :: Int -> Int
inc x = x + 1

map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

even :: Int -> Bool
even x = mod x 2 == 0

filter :: (a -> Bool) -> List a -> List a
filter f Nil = Nil
filter f (Cons x xs) = if f x then Conx x (filter f xs)
	else
		filter f xs

data Maybe a = Nothing | Just a deriving Show

noInt :: Maybe Int
noInt = Nothing

someInt :: Maybe Int
someInt = Just 10

