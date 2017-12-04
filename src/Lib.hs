module Lib where

import Control.Monad (forM_)

-- | `xor` implementation using four operations.
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && (not $ a && b)

-- | Prints a truth table for any boolean function.
truthTable :: (Bool -> Bool -> Bool) -> IO ()
truthTable f = do
  let inputs = [(a, b) | a <- [False, True], b <- [False, True]]
  putStrLn "a b | out"
  putStrLn "---------"
  forM_ inputs $ \(a, b) -> do
    putStrLn $ mconcat [showBool a, " ", showBool b, " | ", showBool (f a b)]


showBool :: Bool -> String
showBool False = "0"
showBool True  = "1"
