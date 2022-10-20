module Main where

import qualified MyLib (someFunc)
import Gfolds

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
