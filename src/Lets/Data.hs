{-

Lets.Data

Build our own list representation using an algebraic data type (ADT).

-}

module Lets.Data where

data List a = Nil | Cons a (List a)
            -- deriving (Eq, Ord, Show, Read)
            deriving (Eq,Ord,Read) -- own show!


--
-- note that Nil and Cons are now defined as functions!
--
-- >>> :t Nil
-- >>> Nil :: List a
--
-- >>> :t Cons
-- >>> Cons :: a -> List a -> List a
--


--
-- builder functions
--

-- empty returns an empty list
empty :: List a
empty = Nil

-- singleton builds a list around a value
singleton :: a -> List a
singleton v = Cons v Nil


-- toList takes an in-built list to our List type
toList :: [a] -> List a
toList []     = Nil
toList (a:as) = Cons a (toList as)


-- fromL takes our List type to an in-built list
fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a as) = a : (fromList as)



--
-- Some sample lists
--

 -- int lists
il1 :: List Integer
il1 = toList [1..10]

il2 :: List Integer
il2 = toList [11..20]

-- string lists
sl1 :: List String
sl1 = toList (words "hello world")

sl2 :: List String
sl2 = toList (words "this is a test")

sl3 :: List String
sl3 = toList (words "to be or not to be")

-- string of string list !
sosl :: List (List String)
sosl = toList [sl1, sl2, sl3]



--
-- instancing a typeclass
--

instance Show a => Show (List a) where
  show Nil = "👍" ++ " "
  show (Cons a as) = show a ++ " : " ++ show as



--
-- higher order functions
--

mapL :: (a -> a) -> List a -> List a
mapL _ (Nil) = Nil
mapL f (Cons a as) = Cons (f a) (mapL f as)


appendL :: List a -> List a -> List a
appendL Nil Nil = Nil
appendL l1 Nil = l1
appendL Nil l2 = l2
appendL (Cons l1 l1s) l2 = Cons l1 (appendL l1s l2)


concatL :: List (List a) -> List a
concatL Nil = Nil
concatL (Cons l ls) = appendL l (concatL ls)


filterL :: (a -> Bool) -> List a -> List a
filterL _ (Nil) = Nil
filterL p (Cons a as) = if (p a) then (Cons a (filterL p as)) else (filterL p as)

foldrL :: (a -> b -> b) -> b -> List a -> b
foldrL _ z (Nil) = z
foldrL f z (Cons a as) = f a (foldrL f z as)

-- sum of list elements
sumL :: List Integer -> Integer
sumL = foldrL (+) 0

-- product of list elements
prodL :: List Integer -> Integer
prodL = foldrL (*) 1

-- identity
idL :: List a -> List a
idL = foldrL Cons Nil -- this is the identity function on List!


--
-- Lots of things can be defined using folds, even map and filter!
--

-- map
mapL' :: (a -> b) -> List a -> List b
mapL' f = foldrL (Cons . f) Nil
--                ^^^^^^^^ => "f, then Cons"

-- alternatively, we can write it this way ...

-- define a "then" operator, |>
(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

-- now, define map using |>
mapL'' :: (a -> b) -> List a -> List b
mapL'' f = foldrL (f |> Cons) Nil
--                 ^^^^^^^^^ => "f then Cons"

-- filter
filterL' :: (a -> Bool) -> List a -> List a
filterL' p = foldrL (\x xs -> if p x then (Cons x xs) else xs) Nil

-- concat
concatL' :: List (List a) -> List a
concatL' = foldrL (appendL) Nil
