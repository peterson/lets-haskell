{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lets.List where

-- import Control.Applicative

{-

Basic implementation of List, taken directly from the one we defined in the
Lets.Data module.

-}

data List a = Nil | Cons a (List a)
            deriving (Eq, Ord, Read, Functor, Foldable, Traversable)

instance Show a => Show (List a) where
  show Nil = "👍" ++ " "
  show (Cons a as) = show a ++ " : " ++ show as


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

-- map for List
mapL :: (a -> b) -> List a -> List b
mapL _ (Nil) = Nil
mapL f (Cons a as) = Cons (f a) (mapL f as)

-- append for List
appendL :: List a -> List a -> List a
appendL Nil Nil = Nil
appendL l1 Nil = l1
appendL Nil l2 = l2
appendL (Cons l1 l1s) l2 = Cons l1 (appendL l1s l2)

-- concat for List
concatL :: List (List a) -> List a
concatL Nil = Nil
concatL (Cons l ls) = appendL l (concatL ls)

--
-- plus one more function we didn't have previously, but now add ...
--

-- instance Functor List where
--   fmap = mapL

-- map then concat
concatMapL :: (a -> List b) -> List a -> List b
concatMapL f = concatL . (fmap f)


instance Monoid (List a) where
  mempty  = Nil
  mappend = appendL
  -- mconcat = concatL . toList
