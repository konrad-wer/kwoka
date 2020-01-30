module Main where

import Parser
import AST
import Preliminary
import TypeInference
import Translate
import Machine
import Data.List
import Data.Maybe
import Control.Monad
import System.Environment
import Text.Megaparsec.Error (errorBundlePretty)
import System.Exit

printHelp :: IO ()
printHelp = putStrLn $ "Usage: kwoka filename [options]\n" ++
  "Options:\n" ++
  "-debug - Pretty print program with inferred types to file\n" ++
  "-help - Print this message\n"

isArg :: String -> Bool
isArg ('-' : _) = True
isArg _ = False

parseArgs :: [String] -> IO (String, [String])
parseArgs xs = do
  let filenames = filter (not . isArg) xs
  let args = filter isArg xs
  if length filenames /= 1 || any (not . flip elem ["-debug", "-help"]) args then do
    printHelp
    exitWith $ ExitFailure 1
  else
    return (head filenames, args)


main :: IO ()
main = do
  (filename, args) <- parseArgs . snd .fromMaybe ("", []) . uncons =<< getArgs
  if "-help" `elem` args then
    printHelp
  else do
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
                when("-debug" `elem` args) $ writeFile (filename ++ ".typed") $ showTypedProgram c ast
                case translateProgram typeEnv funs of
                  (Nothing, _) -> return ()
                  (Just prog, env) -> void $ eval prog env [] []