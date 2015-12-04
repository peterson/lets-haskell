module Lets.Go where

{-
  The list is the fundamental 'data structure' in functional programming.
-}

-- Let's define some lists:

intL  = [1, 2, 3]
-- what is the type of intL? >>> intL :: [Integer]

charL = ['a','b','c']       -- what is the type? ... list2 :: [Char]
-- what is the type of intL? >>> charL :: [Char]


strL  = ["hello", "world"]  -- what is the type? ... list3 :: [[Char]]
-- what is the type of intL? >>> strL :: [[Char]] .. surprised it's not [String]?

strL' = strL :: [String]
-- Q: What is the type now? >>> strlL :: [String] !!
-- Q: Are they equivalent? >>> strL == strlL --> True .. Yes.


{-

Most functional languages are strictly evalued. Haskell is lazily evaluated.

-}

-- infinite lists
infIntL = [1..]

-- Prelude provides a function: take :: a -> [b] -> [b]
-- Q: What is the result of taking n items from an infinite list?
-- >>> take 10 infIntL

{-

Constructing lists.

* Empty list => []
* Cons operator => :
* Infix vs. prefix operations, i.e. 'a : b' vs. '(:) a b'


-}


-- constructing a list
empty = []
intL1 = 1 : []
intL2 = 1 : 2 : []
intL3 = 1 : 2 : 3 : []      -- intL3 == intL ? ... True

-- element at position 'n'

-- using the index position operator (!!)
pos0 = intL3 !! 0
pos1 = intL3 !! 1
pos2 = intL3 !! 2

-- or using pattern matches

pos0' (x:_)     = x
pos1' (_:x:_)   = x
pos2' (_:_:x:_) = x

safefirstL :: [a] -> Maybe a
safefirstL [] = Nothing
safefirstL (x:_) = Just x

-- operations on lists
-- head, tail, length, etc ...
-- ... look at the types!


{-

  List operations

-}

-- length
--
-- >>> :t length
-- >>> length intL3

-- append
--
-- >>> :t (++)
-- >>> intL1 ++ intL3


{-
-- Text
-}

-- some multi-line text

text = "Haskell Brooks Curry (September 12, 1900 - September 1, 1982) \
        \ was an American mathematician and logician. Curry is best known for his work in \
        \ combinatory logic; while the initial concept of combinatory logic was based on a \
        \ single paper by Moses Schonfinkel, much of the development was done by Curry. \
        \ Curry is also known for Curry's paradox and the Curry-Howard correspondence. \
        \ There are three programming languages named after him, Haskell, Brooks and Curry, \
        \ as well as the concept of currying, a technique used for transforming functions \
        \ in mathematics and computer science."
        -- from http://en.wikipedia.org/wiki/Haskell_Curry

-- operations on text
-- words and unwords

{-

Example: Wordcount

-}

wc = length . words
