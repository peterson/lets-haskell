module Lets.Function where

{-
  Functions

  Function application in Haskell is _quiet_ ...

  - Mathematically, we may say f(x) = x + 3

  - In Haskell, we write this as f x = x + 3

  - f is a function that takes a value (x) and returns a value (x + 3)

  - But what kind of value? An Int? A Float? Something else? This is where
    'type signatures' come in.

  - In Haskell, we can write a type signature for f like this:

    f :: Int -> Int

  - '::' is read as "has type", and '->' is read as 'to', so

    f :: Int -> Int

    is read as "f has type Int to Int" .. takes an Int TO an Int (i.e. returns Int)
-}


-- f(x) = x+3
f :: Int -> Int
f x = x + 3

-- g(x) = x^2
g :: Int -> Int
g x = x * x

-- let's give it a more useful name

sq :: Int -> Int
sq x = x * x

-- but sq can work with any number, not just Int's
sq' :: Num a => a -> a
sq' a = a * a

{-
  A note on 'Num a => a'

  'Num a => ...' is an example of a 'typeclass' definition in Haskell. Typeclasses
  are one of Haskell's most unique features, and allow related types to be 'grouped
  together' in terms of their behaviour ... much like "interfaces" do for object
  classes in languages like Java.
-}



-- In Haskell, pi is defined in the Prelude .. but what we really want is 𝛕 (tau)!
-- See: "No, really, pi is wrong .. The Tau Manifesto" --> http://tauday.com/
tau = 2.0 * pi
-- and we can do this as well ...
𝜏 = tau

-- and then use 𝜏 directly in other functions
circleA :: Double -> Double
circleA r = 1/2 * 𝜏 * r * r



{-

Functions are first-class values! You can pass them to other functions, etc.

* example of passing a fn to another fn ...

-}


double x = x + x

apply f x = f $ x

-- >> apply double 21 -> 42


{-

Most of the 'operators' you're used to seeing in other languages
(e.g. ==, *, /, +, - etc) are simply functions in Haskell.

>>> :t (+)
>>> :t (*)
>>> :t (==)

and so on!

-}

-- In the definition of double (above), "+" is a function. We can make this
-- explicit by moving it to infix position:
double' x = (+) x x
