{-

A basic introduction to lenses using tuples, e.g. (1,2)

See: https://www.fpcomplete.com/user/tel/lenses-from-scratch

-}

module Lets.TupleLens where


--
-- Getting and Setting
--

-- given a tuple made up of (x, y) ...
get1 :: (x, y) -> x
get1 (x, _) = x -- return the value x

set1 :: x' -> (x, y) -> (x', y)
set1 x' (_,y) = (x',y) -- overwrite the value x with value x'

-- example
--
-- > get1 (1,2)
-- 1
--
-- > set1 2 (1,2)
-- (2,2)


--
-- First Lens
--

data Lens a b =
  Lens { get :: a -> b
       , set :: b -> a -> a
       }

--
-- Lets make a lens that focuses on the first element in a tuple i.e. a in (a,b)

_1 :: Lens (a,b) a
--         ^^^^  ^   NOTE: 'a' and 'b' are type-level parameters
--          ||   |-  ... this is 'b' in "data Lens a b = .." - i.e. a value of type a
--          ||------ ... and this is 'a' in "data Lens a b = .." - i.e. a tuple of type (a,b)

-- to do this, we need to 'implement' get and set operations that follow the Lens
-- signature defined above. But, we have these functions already - get1 and set1.
-- So we can go ahead and create a "lens", using the Lens type constructor, and
-- our two functions:

_1 = Lens get1 set1


-- Recall that, from record syntax { .. }, the helper functions 'get' and 'set'
-- will now be defined:

-- What is the type of 'get' ?
--
-- > :t get
-- Lens a b -> a -> b
--             ^^^^^^
--                |-- this is the signature of getT
--
-- Breaking this down, 'get' expects a "lens", i.e. a (Lens a b), and an 'a', and returns
-- a 'b'. Alternatively, if you only pass in the "lens", you are given the function,
-- i.e. getT :: a -> b, as expected. This is just partial application!

-- And what is the type of 'set' ?
--
-- > :t set
-- Lens a b -> b -> a -> a
--             ^^^^^^^^^^^
--                  |--  this is the signature of setT
--
-- Similarly, 'set' expects a "lens", 'b' and 'a' and returns an 'a'. If you
-- only pass in the "lens", you are given back the 'setT' function b -> a -> b,
-- as expected. Again, partial application.

--
-- So, given we have the lens '_1' defined:
--
-- > :t get _1
-- get _1 :: (b, b1) -> b
--
-- So, 'get _1' expects a tuple type (b, b1) and extracts the b. That's right.
--
-- Similarly, for the lens '_1' we have:
--
-- > :t set _1
-- set _1 :: b -> (b, b1) -> (b, b1)
--
-- So 'set _1' expects a 'b', a tuple type (b, b1), and returns a tuple
-- type (b, b1). Again, that's exactly what we expect.
--


--
-- Let's try using the lens!
--
-- the old way:
-- > getT (1,2)
-- > 1
--
-- the lens way:
-- > get _1 (1,2)
-- > 1

-- Similarly, for set:
--
-- the old way:
-- setT 2 (1,2)
-- > (2,2)
--
-- the lens way:
-- set _1 2 (1,2)
-- > (2,2)

-- So, the general "lens" works the same way as the getT and setT operations
-- we saw earlier. It's just a "wrappered" version of the set and get inside
-- a type constructor called Lens.



--
-- Let's create a lens for the second element in the tuple.
--
-- What is the type signature?
_2 :: Lens (a,b) b

--
-- We can write our 'get' and 'set' functions directly, as anonomous
-- functions. (These serve the same role as getT and setT in the first
-- example!)

_2 = Lens (\(_, y) -> y) (\y (x, _) -> (x, y))
--        ^^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^
--              |                |- this is 'set' :: b -> (a,b) -> (a,b)
--              |
--              |- this is 'get' :: (a,b) -> b


--
-- Let's try using the _2 lens!
--
-- > get _2 (1,2)
-- > 2
--
-- set _2 1 (1,2)
-- > (1,1)



--
-- Lens composition
--

-- Let's define a composition operator for lenses
(>-) :: Lens a b -> Lens b c -> Lens a c  -- note similarity to (.) !
(>-) l1 l2 = Lens (get l2 . get l1) $
                  (\part whole -> set l1 (set l2 part (get l1 whole)) whole)


-- Let's compose some lenses!
_1_1 = _1 >- _1
_1_2 = _1 >- _2
_2_1 = _2 >- _1
_2_2 = _2 >- _2


--
-- Examples
--
-- Using get
-- > get _1_1 ((1,2),(3,4))
-- 1
-- > get _1_2 ((1,2),(3,4))
-- 2
-- > get _2_1 ((1,2),(3,4))
-- 3
-- > get _2_2 ((1,2),(3,4))
-- 4
--
-- Using set
-- > set _1_1 0 ((1,2),(3,4))
-- ((0,2),(3,4))
-- > set _1_2 0 ((1,2),(3,4))
-- ((1,0),(3,4))
-- > set _2_1 0 ((1,2),(3,4))
-- ((1,2),(0,4))
-- > set _2_2 0 ((1,2),(3,4))
--((1,2),(3,0))
