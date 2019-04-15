module Main where

import Language.Lys.TypeChecking
import Language.Lys.Parser
import Language.Lys.Pretty

import System.Environment

main :: IO ()
main = head <$> getArgs >>= parseFile >>= \case
    Right p -> print p
    Left e -> putStrLn e

