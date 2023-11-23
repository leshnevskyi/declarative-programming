module Main (main) where

import Lib.Utils (isValidPercentage, stringMul)

main :: IO ()
main = do
  putStrLn $ stringMul (2, 5)
  print (isValidPercentage 50)