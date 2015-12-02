{-

Inspired by the article "Evaluating Cellular Automata is Comonadic"
by Dan Piponi.

See: http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html

-}

module Lets.Cellular where

-- define a Zipper for type x
data U x = U [x] x [x]
  -- deriving Show

-- move one step left
left :: U x -> U x
left  (U (l:ls) v r) = U ls l (v:r)

-- move one step right
right :: U x -> U x
right (U l v (r:rs)) = U (v:l) r rs

-- implement Functor for U
instance Functor U where
  fmap f (U l v r) = U (fmap f l) (f v) (fmap f r)

-- define Comonad
class Functor w => Comonad w where
  extend :: (w a -> b) -> w a -> w b
  extract :: w a -> a
  duplicate :: w a -> w (w a)

-- implement Comonad for U
instance Comonad U where
  extract = extractU
  extend  = extendU
  duplicate = duplicateU

-- extract returns the current focus value 'v' from the comonad
-- a.k.a. coreturn, or copure
extractU (U _ v _) = v

-- duplicate constructs a "universe" of all possible shifts of a
-- a.k.a. cojoin
duplicateU :: U x -> U (U x)
duplicateU a = U (tail $ iterate left a) a (tail $ iterate right a)

extendU :: (U x -> b) -> U x -> U b
extendU = \f -> fmap f . duplicateU

-- infix versions of extend
(<<=) = extendU
infixr 1 <<=

-- and flipped extend, a.k.a. cobind
(=>>) :: U x -> (U x -> b) -> U b
(=>>) = flip extendU
infixr 1 =>>


--
-- test

-- simple rule from the article
rule :: U Bool -> Bool
rule (U (l:_) v (r:_)) = not (l && v && not r || (l==v))

-- shift by i steps to the left (negative) or right (positive)
shift :: Int -> U x -> U x
shift i u = (iterate (if i<0 then left else right) u) !! abs i

toList i j u = take (j-i) $ half $ shift i u where
  half (U _ b c) = [b] ++ c

rtest rule n =
 let u = U (repeat False) True (repeat False)
 in putStr $
    unlines $
    take n $
    fmap (fmap (\x -> if x then '*' else ' ') . toList (-n) n) $
    iterate (=>> rule) u

test = rtest rule 20

-- trivial initial state
u = U (repeat False) True (repeat False)

-- rule 110:
-- current pattern           111 110 101 100 011 010 001 000
-- new state for center cell  0   1   1   0   1   1   1   0
--
-- See: https://en.wikipedia.org/wiki/Rule_110

rule110 :: U Bool -> Bool
rule110 (U (True:_) True (False:_))   = True
rule110 (U (True:_) False (True:_))   = True
rule110 (U (False:_) True (True:_))   = True
rule110 (U (False:_) True (False:_))  = True
rule110 (U (False:_) False (True:_))  = True
rule110 _                             = False

r110 = rtest rule110

--
-- rule 30:
-- current pattern           111 110 101 100 011 010 001 000
-- new state for center cell  0   0   0   1   1   1   1   0
--
-- See: https://en.wikipedia.org/wiki/Rule_30

rule30 ::  U Bool -> Bool
rule30 (U (True:_) False (False:_)) = True
rule30 (U (False:_) True (True:_))  = True
rule30 (U (False:_) True (False:_)) = True
rule30 (U (False:_) False (True:_)) = True
rule30 _                            = False

r30  = rtest rule30

--
-- rule 90: (XOR)
--
-- current pattern           111 110 101 100 011 010 001 000
-- new state for center cell  0   1   0   1   1   0   1   0
--
-- See: https://en.wikipedia.org/wiki/Rule_90

xor :: Bool -> Bool -> Bool
xor True  True  = True
xor False False = True
xor _     _     = False

rule90 :: U Bool -> Bool
rule90 (U (l:_) v (r:_)) = l `xor` v `xor` r

-- when u = U (F..) T (F..), rule90 produces a Sierpinski triangle
r90 = rtest rule90


--
-- rule 184:
--
-- current pattern           111 110 101 100 011 010 001 000
-- new state for center cell  1   0   1   1   1   0   0   0
--
-- See: https://en.wikipedia.org/wiki/Rule_184

rule184 :: U Bool -> Bool
rule184 (U (True:_) True (True:_))   = True
rule184 (U (True:_) False (True:_))  = True
rule184 (U (True:_) False (False:_)) = True
rule184 (U (False:_) True (True:_))  = True
rule184 _                            = False

r184 = rtest rule184
