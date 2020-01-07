module Main where

import Parser
import AST
import Preliminary
import TypeInference
import System.Environment
import Text.Megaparsec.Error (errorBundlePretty)

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
        Right ast -> case checkProgram (buildEffectEnv ast) (buildTypeEnv ast) (getFuns ast) of
          Left err -> print err
          _ -> putStrLn $ showProgram ast