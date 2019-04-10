module Main where

import Language.Lys.TypeChecking
import Language.Lys.Types

import Control.Monad (forM_)

import qualified Data.Map as Map

import Lens.Micro
import Text.PrettyPrint.ANSI.Leijen

bcIntBody :: Process
bcIntBody = NewP "tea" (VarT "N") (OutputP (VarN "s") (VarN "tea") (NewP "cc" (VarT "I") (OutputP (VarN "s") (VarN "cc") (InputP (VarN "s") "r" NilP))))

p :: String -> Process
p v = NewP "u" (VarT "G") (OutputP (VarN v) (VarN "u") NilP)

s :: String -> Process
s x = InputP (VarN "x") "y" (SourceP "x" "u" (ReplicateP (VarN "y") "z" (AppP "p" (VarN "u"))))

q :: String -> Process
q x = ParP (NewP "k1" (VarT "K1") (OutputP (VarN x) (VarN "k1") (VarP "q1")))
           (NewP "k2" (VarT "K2") (OutputP (VarN x) (VarN "k2") (VarP "q2")))

r :: String -> Process
r x = NewP "k1" (VarT "K1") (OutputP (VarN x) (VarN "k1") (VarP "q1"))

defaultGamma :: Map.Map String Scheme
defaultGamma = Map.fromList
    [ ("p", Scheme ["A", "G"] ["v"] (Context (Map.fromList [("z", VarT "A")]) (Map.fromList [("v", VarT "G")])))
    , ("q1", Scheme ["A"] [] (Context (Map.fromList [("y", VarT "A")]) (Map.fromList [("u", VarT "A")])))
    , ("q2", Scheme [] [] mempty)
    ]

main :: IO ()
main = case runInfer (infer (r "q" :⊢ mempty & theta %~ introduce "q" (VarT "Q"))) defaultGamma of
    Left (InferError err) -> putDoc (err <> char '\n')
    Right ctx -> print ctx
