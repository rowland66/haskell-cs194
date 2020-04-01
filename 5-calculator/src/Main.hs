module Main where

import System.Console.Haskeline
import Control.Monad.State
import Data.Either

import Expr
import Parser
import StackVM

main :: IO ()
main = runInputT defaultSettings (evalStateT loop [])

loop :: StateT [(String, Integer)] (InputT IO) ()
loop = do
    maybeInput <- lift (getInputLine "% ")
    case maybeInput of
        Nothing -> return ()
        Just ":q" -> return () -- special handling for quit
        Just input -> do
            if (head input == ':') then
              (processCmd (tail input)) >> loop
            else
              (processExpression input) >> loop

processExpression :: String -> StateT [(String, Integer)] (InputT IO) ()
processExpression input = do
  map <- get
  lift $ outputStrLn $ decodeStackVal $ fromRight Void (stackVM $ compile map input)
                    where
                      decodeStackVal :: StackVal -> [Char]
                      decodeStackVal (IVal n) = show n
                      decodeStackVal (BVal b) = show b
                      decodeStackVal (Void) = "Error"

processCmd :: String -> StateT [(String, Integer)] (InputT IO) ()
processCmd cmd = do
  variableMap <- get
  case (operation) of
    's' -> put $ (parseAssignment argument) : variableMap
    'u' -> put $ filter (\varMapping -> (fst varMapping) /= argument) variableMap
    'd' -> displayVariableMap
    otherwise -> return ()
  where
    operation = head cmd
    argument = drop 2 cmd

parseAssignment :: String -> (String, Integer)
parseAssignment s = let key = fst $ break (== '=') s
                        value = read $ tail $ snd (break (== '=') s)
                    in (key, value)

displayVariableMap :: StateT [(String, Integer)] (InputT IO) ()
displayVariableMap = do
  alist <- get
  lift $ mapM_ (\varMapping -> outputStrLn $ fst varMapping ++ "=" ++ show (snd varMapping)) alist

compile :: [(String, Integer)] -> String -> Program
compile map expStr = case maybeExp of
                        Nothing -> []
                        Just exp -> Expr.eval exp
                    where
                        maybeExp = parseExp lit (var map) add mul expStr :: Maybe (ExprT Program)


