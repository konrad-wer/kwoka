module Main where

import Parser
import ASTBuilder
import System.Environment
import Text.Megaparsec.Error (errorBundlePretty)

import qualified Data.Map as Map

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
        Right ast -> mapM_ print $ Map.toList $ buildTypeEnv ast