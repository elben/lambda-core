module Main where

import LambdaCore.Data
import LambdaCore.Eval
import LambdaCore.Utils
import LambdaCore.Parser
import System.Console.Haskeline
import Data.List

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "LambdaCore> "
    case minput of
      Nothing -> outputStrLn "Exiting..."
      Just input -> do
        let typeCheck = ":t " `isPrefixOf` input
        let input' = if typeCheck then input \\ ":t " else input
        let answer = parseAndEval input'
        case answer of
          Left e -> outputStrLn e
          Right (e, t) ->
            if typeCheck
            then outputStrLn (input' ++ " : " ++ show t)
            else outputStrLn (toLisp e ++ " : " ++ show t)
        loop

-- | Evaluates program.
parseAndEval :: Program -> Either String Answer
parseAndEval p =
  case parseProgram p of
    Left err -> Left $ show err
    Right expr ->
      case eval expr of
        Left err -> Left $ show err
        Right answer -> Right answer

