module Main where

import Language.Lys.Parser
import Language.Lys.Desugar
import Language.Lys.TypeChecking
import Language.Lys.Pretty

import System.Environment

main :: IO ()
main = head <$> getArgs >>= parseFile >>= \case
    Right p -> do
        print p
        putStrLn ""
        case runCheck (check p) mempty of
            Left errs -> mapM_ prettyPrint errs
            Right (_, e) -> print e
    Left e -> putStrLn e
