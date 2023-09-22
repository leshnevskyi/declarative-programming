module Main (main) where

import Lib (isValidPercentage, stringMul)

main :: IO ()
main = do
  putStrLn $ stringMul (2, 5)
  print (isValidPercentage 50)