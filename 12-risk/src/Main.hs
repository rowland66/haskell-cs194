module Main where

import System.IO
import System.Environment(getArgs)
import Risk
import Control.Monad.Random
import Control.Monad.Writer

main :: IO ()
main = do
  debug <- isDebugEnabled
  putStrLn "Welcome to the Risk invasion success calculator."
  if debug then
    putStrLn "(Debug mode enabled)"
  else
    return ()
  putStrLn ""
  putStrLn "How many attacking armies? "
  aCountStr <- getLine
  putStrLn "How many defending armies? "
  dCountStr <- getLine
  (prob, debugOutput) <- runSuccessProb (if debug then 1 else 1000) (read aCountStr) (read dCountStr)
  if debug then do
    putStrLn ("Dice roll data:")
    putStrLn debugOutput
  else
    putStrLn ("Probability of successful invastion: " ++ show prob)

isDebugEnabled :: IO Bool
isDebugEnabled = do
  args <- getArgs
  let debugStr = filter (== "--debug") args
  return (if debugStr /= [] then True else False)