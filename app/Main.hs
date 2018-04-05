module Main where

import Adjunctions(adjunctionMain)

main :: IO ()
main = adjunctionMain >> putStrLn "sorted"
