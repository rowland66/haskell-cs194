module Main where

import JoinListBuffer
import Editor
import Sized
import Scrabble
import JoinList

main :: IO ()
main = runEditor editor (Single ((Score 0), (Size 1)) "--EMPTY--")