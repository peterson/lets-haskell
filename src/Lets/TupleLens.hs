{-

A basic introduction to lens operations using tuples.

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
--                |-- this is the signature of get1
--
-- Breaking this down, 'get' expects a "lens", i.e. a (Lens a b), and an 'a', and returns
-- a 'b'. Alternatively, if you only pass in the "lens", you are given the function,
-- i.e. get1 :: a -> b, as expected. This is just partial application!

-- And what is the type of 'set' ?
--
-- > :t set
-- Lens a b -> b -> a -> a
--             ^^^^^^^^^^^
--                  |--  this is the signature of set1
--
-- Similarly, 'set' expects a "lens", 'b' and 'a' and returns an 'a'. If you
-- only pass in the "lens", you are given back the 'set1' function b -> a -> b,
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
-- > get1 (1,2)
-- > 1
--
-- the lens way:
-- > get _1 (1,2)
-- > 1

-- Similarly, for set:
--
-- the old way:
-- set1 2 (1,2)
-- > (2,2)
--
-- the lens way:
-- set _1 2 (1,2)
-- > (2,2)

-- So, the general "lens" works the same way as the get1 and set1 operations
-- we saw earlier. It's just a "wrappered" version of the set and get inside
-- a type constructor called Lens.



--
-- Let's create a lens for the second element in the tuple.
--
-- What is the type signature?
_2 :: Lens (a,b) b

--
-- We can write our 'get' and 'set' functions directly, as anonomous
-- functions. (These serve the same role as get1 and set1 in the first
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
-- Looking at the types is constructive ...
--
-- > :t _1_1
-- _1_1 :: Lens ((c, b1), b) c
--
-- > :t _1_1
-- _1_2 :: Lens ((a, c), b) c
--
-- These signatures show clearly which part of the structure the lens
-- is focused upon!


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


--
-- Shortcut operators
--

(.~) :: Lens a b -> b -> a -> a
(.~) = set
infixr 4 .~

(^.) :: a -> Lens a b -> b
(^.) = flip get
infixl 8 ^.

--
-- Examples
--
-- (^.) i.e. get
--
-- > (1,2) ^. _1
-- 1
--
-- > (1,2) ^. _2
-- 2
--
-- (.~) i.e. set
-- > _1 .~ 4 $ (1,2)
-- (4,2)
--
-- > _2 .~ 3 $ (1,3)
-- > (1,3)


--
-- Mapping over a lens
--

-- over is 'fmap' for a lens
over :: Lens a b -> (b -> b) -> a -> a
over l f a = set l (f (get l a)) a
(%~) = over
infixr 4 %~


--
-- Examples
--
-- > _1 %~ (*2) $ (3,2)
-- > (6,2)
--
-- > _2 %~ (*2) $ (3,2)
-- > (3,4)
--
-- Looking at the types:
--
-- > :t (_1 %~)
-- > (_1 %~) :: (b -> b) -> (b, b1) -> (b, b1)
--              ^^^^^^^^    ^^^^^^^    ^^^^^^^
--                  |          |----------|----- tuples
--                  |-- the function 'f' to be mapped over the lens focus



--
-- Lens laws
--

-- 1. Get-Set law
-- This law says that changing the sub-part (via set) to exactly the original
-- value (via get) is the same thing as doing nothing at all!

get_set_law :: Eq a => Lens a b -> a -> Bool
get_set_law l =
  \a ->
    set l (get l a) a == a

--
-- To test, put in a lens (e.g. _1) and a tuple, e.g. (1,2) and you will
-- be returned a Bool (which should always be True).
--
-- > get_set_law _1 (3,2)
-- True
-- > get_set_law _1 (99,23)
-- True
-- > get_set_law _1 (1,2)
-- True


-- 2. Set-Get law
-- This law says if you perform a set operation on a focus using a lens and then
--  view / get the same location, you will see the value that was set (s).

set_get_law :: Eq b => Lens a b -> b -> a -> Bool
set_get_law l =
  \s a ->
    get l (set l s a) == s

--
-- To test, put in a lens e.g. _1, a new value e.g. 5 and a tuple e.g. (1,2)
-- and you will be returned a Bool (which should always be True).
--
-- > set_get_law _1 5 (1,2)
-- True
-- > set_get_law _1 5 (3,2)
-- True
-- > set_get_law _1 5 (2,1)
-- True


-- 3. Set-Set law
-- This law says if you overwrite the result of one set operation (s1) by
-- a second set operation (s2), only the result of last operation (s2) is
-- preserved.

set_set_law :: Eq a => Lens a b -> b -> b -> a -> Bool
set_set_law l =
  \s1 s2 a ->
    set l s2 (set l s1 a) == set l s2 a
--  ^^^^^^^^ ^^^^^^^^^^^^
--     |          |- first (inner) set operation (s1)
--     |
--     |- second (outer) set operation (s2)


--
-- To test, put in a lens e.g. _1, a first set value (s1) e.g. 12, a second
-- set value (s2) e.g. 24, and a tuple, e.g. (1,2) and you will be returned
-- a Bool (which should always be True).
--
-- > set_set_law _1 12 24 (1,2)
-- True
--
-- What happened here? Let's break it down a bit ...
--
-- The first set operation (s1) produces a result like this:
-- > _1 .~ 12 $ (1,2)
-- (12,2)
--
-- The second set operation (s2) produces a result like this:
-- > _1 .~ 24 $ (1,2)
-- (24,2)
--
-- that is, the update in s1 has now been overwritten by s2.
--
-- Out set_set_law asserts that the end result is always equal to
-- just the second set operation (s2). This is why the RHS of the (==)
-- comparison is 'set l s2 a' (i.e. the tuple resulting from applying
-- the lens l and the value s2 to the the input a!). The value s1 is
-- simply thrown away.


--
-- For a deep dive on the relationship between the lens laws and
-- a "costate comonad coalgebra", see here:
--
-- http://r6research.livejournal.com/23705.html
--
-- And for the last word:
--
-- "Costate Comonad Coalgebra is equivalent of Java's member variable
-- update technology for Haskell"
--   - @PLT_Borat
--   https://twitter.com/PLT_Borat/status/228009057670291456
--
-- So now you know what is Lens ... I like! Hi 5!
