module Lets.TouchyFeelyFuzzyScary where

import Control.Applicative ((<*>))


{-
  Typeclasses ...
-}

class Touchy t where
  (~>) :: (a -> b) -> t a -> t b -- touchy


class Feely f where
  (<~>) :: f (a -> b) -> f a -> f b -- feely
  (~^)  :: a -> f a -- grabby


class Fuzzy z where
  (~~>) :: z a -> (a -> z b) -> z b -- fuzzy
  (^~^) :: a -> z a -- grabby


{-

  Touchy ... a.k.a. Functor

-}

instance Touchy [] where
  (~>) = touchy

touchy :: (a -> b) -> [a] -> [b]
touchy _ [] = []
touchy f (a:as) = (f a) : (touchy f as)
-- touchy is just "map"! ... i.e. recursively apply f to each a in the list!
-- We call it "touchy" because it "touches" each element a in as with f,
-- i.e. f ~> as ... (equivalent to "map f as", or f <$> as)

-- note: ~> is supposed to look like a finger!

-- Examples:
--
-- >>> odd ~> [1..5]
-- >>> [True,False,True,False,True]
--
-- ex: square
-- >>> (\i -> i * i) ~> [1..5]
-- >>> [1,4,9,16,25]


{-

  Feely ... a.k.a. Applicative

-}

instance Feely [] where
  (~^)  = grabby
  (<~>) = feely

grabby :: a -> [a]
grabby a = [a]
-- grabby takes some value v, and puts it into a list, i.e. [v]

feely :: [a -> b] -> [a] -> [b]
feely [] _ = []
feely (f:fs) as = (f ~> as) ++ feely fs as
--
-- Let's break the above line down a bit ...
-- 1. (f ~> as) "touches" the as with f (i.e. map)
-- 2. (++) appends the list produced by step 1. to ...
-- 3. (feely fs as) recurses into feely, i.e. to "touchy" with the remaining fs
--
-- So essentially this recursively builds a flattened list of the results of applying
-- each f in fs to each a in as! (feely handles the recursion over fs, and calls
-- touchy (~>), defined above, which handles the recursion over as!)

-- note: <~> is supposed to look like a "two-way" finger, perhaps (as it's "touching"
-- both sides, i.e. both arguments, not just one side.)

-- Examples:
--
-- >>> [(*2), (*3), (*4)] <~> [1..5]
-- >>> [2,4,6,8,10,3,6,9,12,15,4,8,12,16,20]
--
-- >>> [odd, even] <~> [1..5]
-- >>> [True,False,True,False,True,False,True,False,True,False]


{-

  Fuzzy ... a.k.a. Monad (ooh, scary!)

-}

instance Fuzzy [] where
  (~~>) = fuzzy
  (^~^) = grabby

-- now, we need a couple of helper functions!

squeezy :: [[a]] -> [a] -- a.k.a. flatten
squeezy [] = []
squeezy (l:ls) = l ++ squeezy ls
-- squeezy takes a list of list, and returns a list, i.e. removes one level of nesting.
-- it does this by replacing each cons (:) in the list with an append (++) operation.
-- Ex: >>> squeezy [[1,2,3],[4,5,6]] ==> [1,2,3,4,5,6]
-- Note: Remember [[1,2,3],[4,5,6]] is equivalent to [1,2,3] : [4,5,6] : []
-- so [1,2,3] : [4,5,6] : [] ==> [1,2,3] ++ [4,5,6] ++ [] ==> [1,2,3,4,5,6]


touchysqueezy :: (a -> [b]) -> [a] -> [b]
touchysqueezy f = squeezy . ((~>) f) -- squeezy . (touchy f)
-- touchysqueezy "touches with f, then flattens" ...


fuzzy :: [a] -> (a -> [b]) -> [b]
fuzzy = flip touchysqueezy
-- fuzzy just flips the order of arguments to touchysqueezy ...
-- with fuzzy, we have a function of the form input_list -> function -> output_list
-- ... this is very handy for "threading" the output of one fuzzy to the input of
-- a subsequent fuzzy!

-- note: ~~> is supposed to look like a scary finger! :)

--
-- Examples:
--

--
-- using the in-built "list comprehension" syntax (sugar!)
listily = [ (i,j) | i <- [1..3], j <- [i+2..i+4] ]

-- >>> listily
-- >>> [(1,3),(1,4),(1,5),(2,4),(2,5),(2,6),(3,5),(3,6),(3,7)]


-- using the list monad, with in-built "do-notation" syntax (sugar!)
doily = do
  i <- [1..3]
  j <- [i+2 .. i+4]
  return (i,j)

-- >>> doily
-- >>> [(1,3),(1,4),(1,5),(2,4),(2,5),(2,6),(3,5),(3,6),(3,7)]


-- using our own operator (~~>), or "fuzz" ...
fuzzily =
  [1..3] ~~> \i ->
    [i+2 .. i+4] ~~> \j ->
      (^~^) (i,j)
-- (~~>), or "fuzz", threads a list into a function and gets a list back.
-- (^~^), or "grab / return / pure / wrap / whatever", grabs a value e.g. (i,j)
-- and puts it into a context (in this case a list), e.g. (i,j) ==> [(i,j)]

--
-- >>> fuzzily
-- >>> [(1,3),(1,4),(1,5),(2,4),(2,5),(2,6),(3,5),(3,6),(3,7)]


-- using the in-built list monad with (>>=), or "bind"
scarily =
  [1..3] >>= \i ->
    [i+2 .. i+4] >>= \j ->
      return (i,j)
-- note: an identical definition as fuzzily! ~~> becomes >>=, and ^~^ becomes return!

--
-- >>> scarily
-- >>> [(1,3),(1,4),(1,5),(2,4),(2,5),(2,6),(3,5),(3,6),(3,7)]
