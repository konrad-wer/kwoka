module Main where

import Parser
import AST
import Preliminary
import TypeInference
import System.Environment
import Text.Megaparsec.Error (errorBundlePretty)

import qualified Data.Map as Map
import Data.List

readArgs :: [a] -> Maybe a
readArgs [] = Nothing
readArgs xs = return $ head xs

main :: IO ()
main = do
  args <- readArgs <$> getArgs
  case args of
    Nothing -> putStrLn "Please provide filename!"
    Just filename -> do
      sourceCode <- readFile filename
      case parseProgram filename sourceCode of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast ->
          case buildProgram ast of
            Left err -> print err
            Right (funs, effectEnv, typeEnv) ->
              case checkProgram effectEnv typeEnv funs of
                Left err -> print err
                Right c -> do
                  putStrLn $ showProgram ast
                  putStrLn "\n\n"
                  putStrLn (intercalate "\n". map show . Map.toList $ c)