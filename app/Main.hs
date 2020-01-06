module Main where

import Parser
import ASTBuilder
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
        Right ast -> print $ checkProgram (buildEffectEnv ast) (buildTypeEnv ast) (getFuns ast)