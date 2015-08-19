{-

Lets.Data

Build our own list representation using an algebraic data type (ADT)

-}

module Lets.Data where

data List a = Nil | Cons a (List a)
            deriving (Eq, Ord, Show, Read)
            -- deriving (Eq,Ord,Read) -- own show!


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
-- Some sample lists
--

 -- int lists
il1 = toList [1..10]
il2 = toList [11..20]
-- >>> :t il1
-- >>> il1 :: List Integer

-- string lists
sl1 = toList (words "hello world")
sl2 = toList (words "this is a test")
sl3 = toList (words "to be or not to be")
-- >>> :t sl1
-- >>> sl1 :: List String

-- string of string list !
sosl = toList [sl1, sl2, sl3]
-- >>> :t sosl
-- >>> sosl :: List (List String)



--
-- instancing a typeclass
--

-- instance Show a => Show (List a) where
--   show Nil = "👍" ++ " "
--   show (Cons a as) = show a ++ " : " ++ show as
--


--
-- builder functions
--

-- empty returns an empty list
empty :: List a
-- empty = undefined
empty = Nil

-- singleton builds a list around a value
singleton :: a -> List a
-- singleton = undefined
singleton v = Cons v Nil


-- toList takes an in-built list to our List type
toList :: [a] -> List a
-- toList = undefined
toList []     = Nil
toList (a:as) = Cons a (toList as)


-- fromL takes our List type to an in-built list
fromList :: List a -> [a]
-- fromL = undefined
fromList Nil = []
fromList (Cons a as) = a : (fromList as)


--
-- higher order functions
--

mapL :: (a -> a) -> List a -> List a
-- mapL = undefined
mapL f (Nil) = Nil
mapL f (Cons a as) = Cons (f a) (mapL f as)


appendL :: List a -> List a -> List a
-- appendL = undefined
appendL Nil Nil = Nil
appendL l1 Nil = l1
appendL Nil l2 = l2
appendL (Cons l1 l1s) l2 = Cons l1 (appendL l1s l2)


concatL :: List (List a) -> List a
-- concatL = undefined
concatL Nil = Nil
concatL (Cons l ls) = appendL l (concatL ls)


filterL :: (a -> Bool) -> List a -> List a
-- filterL = undefined
filterL p (Nil) = Nil
filterL p (Cons a as) = if (p a) then (Cons a (filterL p as)) else (filterL p as)

foldrL :: (a -> b -> b) -> b -> List a -> b
-- foldrL = undefined
foldrL f z (Nil) = z
foldrL f z (Cons a as) = f a (foldrL f z as)


-- sumL = undefined
sumL = foldrL (+) 0

-- prodL = undefined
prodL = foldrL (*) 1

-- idL = undefined
idL = foldrL Cons Nil -- this is the identity function on List!


--
-- Lots of things can be defined using folds, even map and filter!
--

-- map
mapL' f = foldrL (Cons . f) Nil
--                ^^^^^^^^ => "f, then Cons"

-- alternatively, we can write it this way ...

-- define a "then" operator, |>
(|>) = flip (.)
-- >>> :t (|>)
-- >>> (|>) :: (a -> b) -> (b -> c) -> a -> c

-- now, define map using |>
mapL'' f = foldrL (f |> Cons) Nil
--                 ^^^^^^^^^ => "f then Cons"

-- filter
filterL' p = foldrL (\x xs -> if p x then (Cons x xs) else xs) Nil

-- concat
concatL' = foldrL (appendL) Nil
