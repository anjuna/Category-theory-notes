module Main where

import Adjunctions(adjunctionMain)
import ComonadStuff

main :: IO ()
main = adjunctionMain >> putStrLn "sorted"


