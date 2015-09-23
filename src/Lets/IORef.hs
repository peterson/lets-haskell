module Lets.IORef where

import Data.IORef
import Control.Concurrent
import Control.Monad (replicateM_)

{-

Important type signatures for IORef's:

newIORef :: a -> IO (IORef a)
readIORef :: IORef a -> IO a
writeIORef :: IORef a -> a -> IO ()
modifyIORef' :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b

-}


double :: Int -> Int
double = (*2)

updateRef :: IORef Int -> IO ()
updateRef ref = do
  val <- readIORef ref
  writeIORef ref (double val)

incrTuple :: Int -> (Int,())
incrTuple v = (succ v,())

modifyRef :: IORef Int -> IO ()
modifyRef ref = do
  modifyIORef ref succ -- increment

modifyRef' :: IORef Int -> IO ()
modifyRef' ref = do
  modifyIORef' ref succ -- strict!

atomicModifyRef' :: IORef Int -> IO ()
atomicModifyRef' ref = do
  atomicModifyIORef' ref incrTuple -- strict!


main1 :: IO ()
main1 = do
  -- initialise IORef with a value of 1
  ref <- newIORef (1 :: Int)
  putStrLn "New IORef initialised"

  -- read and display
  val1 <- readIORef ref
  putStrLn $ "Value is: " ++ show val1

  -- mutate
  updateRef ref

  -- read and display
  val2 <- readIORef ref
  putStrLn $ "Value is: " ++ show val2


main2 :: IO ()
main2 = do
  -- initialise IORef with a value of 1
  ref <- newIORef (1 :: Int)
  putStrLn "New IORef initialised"

  -- read and display
  val1 <- readIORef ref
  putStrLn $ "Value is: " ++ show val1

  -- mutate
  replicateM_ 200000 $ modifyRef ref -- non-strict, use modifyRef' for strict

  -- read and display
  val2 <- readIORef ref -- RACE!
  putStrLn $ "Value is: " ++ show val2


main3 :: IO ()
main3 = do
  -- initialise IORef with a value of 1
  ref <- newIORef (1 :: Int)
  putStrLn "New IORef initialised"

  -- read and display
  val1 <- readIORef ref
  putStrLn $ "Value is: " ++ show val1

  -- mutate
  forkIO(replicateM_ 200000 $ modifyRef' ref) -- new thread!

  -- read and display
  val2 <- readIORef ref -- RACE!
  putStrLn $ "Value is: " ++ show val2


-- run 'main4' repeatedly .. note differing values of val2 !
main4 = do
  -- initialise IORef with a value of 1
  ref <- newIORef (1 :: Int)
  putStrLn "New IORef initialised"

  -- read and display
  val1 <- readIORef ref
  putStrLn $ "Value is: " ++ show val1

  -- mutate
  forkIO(replicateM_ 200000 $ atomicModifyRef' ref) -- new thread!

  -- read and display
  val2 <- readIORef ref -- BUT ... this still gets an incorrect result!
  putStrLn $ "Value is: " ++ show val2

  {-
  From Data.IORef haddocks:

  "The implementation is required to ensure that reordering of memory operations
  cannot cause type-correct code to go wrong. In particular, when inspecting the
  value read from an IORef, the memory writes that created that value must have
  occurred from the point of view of the current thread.

  atomicModifyIORef acts as a barrier to reordering. Multiple atomicModifyIORef
  operations occur in strict program order. An atomicModifyIORef is never observed
  to take place ahead of any earlier (in program order) IORef operations, or
  after any later IORef operations."

  -}

--
-- entry point

main :: IO ()
main = do
  main1
  -- main2
  -- main3
  -- main4
