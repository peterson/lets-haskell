module Lets.Comonad where

import Data.Monoid
import Lets.List

{-
  Typeclasses ...
-}

class Comonad w where
  extend :: (w a -> b) -> w a -> w b
  extract :: w a -> a

instance Comonad List where
  extend = extendL
  extract = extractL

duplicateL :: List a -> List (List a)
duplicateL Nil     = Nil
duplicateL l@(Cons h t) = singleton l <> duplicateL t
-- Q: can i do this with an unfold?

extractL :: List a -> a
extractL (Cons a _) = a

extendL :: (List a -> b) -> List a -> List b
extendL =
  \f -> fmap f . duplicateL

(<<=) :: (List a -> b) -> List a -> List b
(<<=) = extendL
infixr 1 <<=

-- flipped version of extend ... dual of (=<<) (bind) operator
(=>>) :: List a -> (List a -> b) -> List b
(=>>) = flip extendL
infixr 1 =>>

{-

> let l = toList [1..3]
1 : 2 : 3 : 👍

> id <<= l
1 : 2 : 3 : 👍  : 2 : 3 : 👍  : 3 : 👍  : 👍

- note that <<= is simply \f -> fmap f . duplicateL ...
> duplicateL l
1 : 2 : 3 : 👍  : 2 : 3 : 👍  : 3 : 👍  : 👍

> length <<= l
3 : 2 : 1 : 👍

> l =>> length
3 : 2 : 1 : 👍

-}

-- right-to-left co-Kleisli composition
(=<=) :: Comonad w => (w b -> c) -> (w a -> b) -> w a -> c
f =<= g =
  f . extend g
