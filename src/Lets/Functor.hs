module Lets.Functor where

import Control.Applicative

{-

Basic implementation of List, taken directly from the one we defined in the
Lets.Data module.

-}

data List a = Nil | Cons a (List a)
            deriving (Eq,Ord,Read)

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
-- map then concat
concatMapL :: (a -> List b) -> List a -> List b
concatMapL f = concatL . (fmap f)

-----------------------------------------------------------


{-

Functors

-}



instance Functor List where
  fmap = mapL


single = (*1)
double = (*2)
triple = (*3)

numL = toList [1..5]

-- fmap :: Functor f => (a -> b) -> f a -> f b


-- with Functor now defined for list, we can use fmap!

-- >>> fmap double numL
-- >>> 2 : 4 : 6 : 8 : 10 : 👍
--
-- or, using the infix form of fmap ... <$> ...
--
-- >>> double <$> numL
-- >>> 2 : 4 : 6 : 8 : 10 : 👍
--



{-

Applicatives (or, applicative functors)

-}

instance Applicative List where
  pure = singleton

  -- we can define <*> using pattern matching and appendL ...
  (Cons f fs) <*> as = appendL (f <$> as) (fs <*> as)
  (Nil)       <*> as = Nil

  -- alternatively, we can define more compactly, using concatMapL ...
  -- f <*> a = concatMapL (\g -> fmap g a) f


-- what is the type signature for applicatives?
-- ap :: f (a -> b) -> f a -> f b
--
-- The infix operator form of 'ap' is <*> ...
--

-- Let's put our functions (a -> b) into a context ..

timesL :: List (Integer -> Integer)
timesL = toList [single, double, triple]

boolL :: List (Integer -> Bool)
boolL  = toList [odd, even]

--
-- now, we can use the applicative instance we've just defined (and the <*> operator)
-- to apply these functions to the list of numbers ...
--
-- >>> timesL <*> numL
-- >>> 1 : 2 : 3 : 4 : 5 : 2 : 4 : 6 : 8 : 10 : 3 : 6 : 9 : 12 : 15 : 👍
--                   >>> | <<<              >>> | <<<           >>> | <<<
--
-- >>> boolL <*> numL
-- >>> True : False : True : False : True : False : True : False : True : False : 👍
--                                    >>> | <<<                             >>> | <<<



{-

Fuzzy's (... Monads!)

-}


-- First, let's review "list comprehensions" ...

listcomp :: [ (Integer,Integer) ]
listcomp = [(i,j) | i <- [1..3], j <- [i+1..i+3] ]


--
-- Doing this using in-built lists (i.e. [1,2,3] ) ..

sugared :: [ (Integer,Integer) ]
sugared = do
  i <- [1..3]
  j <- [i+1..i+3]
  return (i,j)

desugared :: [ (Integer,Integer) ]
desugared =
  [1..3] >>= \i ->
    [i+1..i+3] >>= \j ->
      return (i,j)


--
-- using Fuzzy's

class Fuzzy m where
  (~~>) :: m a -> (a -> m b) -> m b -- fuzz
  ret   :: a -> m a


instance Fuzzy List where
  (~~>) = bindL
  ret   = singleton


bindL :: List a -> (a -> List b) -> List b
bindL = flip concatMapL

--- doing this using Fuzzy's and our List ...
desugaredFuzzy :: List (Integer, Integer)
desugaredFuzzy =
  toList [1..3] ~~> \i ->
    toList [i+1..i+3] ~~> \j ->
      ret (i,j)


--
-- now, doing this using (ooh, scary!) ... Monads!

instance Monad List where
  (>>=)  = bindL
  return = singleton

-- note the functions used (bindL and singleton) are the SAME as for Fuzzy!


-- doing this using our newly-defined Monad for List
desugaredMonad =
  toList [1..3] >>= \i ->
    toList [i+1..i+3] >>= \j ->
      return (i,j)


-- Conclusions:
--
-- 1. A Monad is just a different name for a Fuzzy!
-- 2. (>>=), or "bind", is exactly the same as (~~>), or "fuzz"
-- 3. (>>=), or "bind", is basically just an "and-then" operator
-- 4. Monads provide a context that allows for greater control over execution, in which
-- values may be 'passed down' the monadic chain, using the (>>=), or "bind", operator.

-- There's a tiny bit more to it, e.g. (>>) for example, but essentially that's it!
