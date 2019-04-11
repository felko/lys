module Main where

import Language.Lys.TypeChecking
import Language.Lys.Types

import Control.Monad (forM_)

import qualified Data.Map as Map

import Control.Lens hiding (Context)

import Text.PrettyPrint.ANSI.Leijen

bcIntBody :: Process
bcIntBody = NewP "tea" (VarT "N") (OutputP (VarN "s") (VarN "tea") (NewP "cc" (VarT "I") (OutputP (VarN "s") (VarN "cc") (InputP (VarN "s") "r" NilP))))

p :: String -> Process
p v = NewP "u" (VarT "G") (OutputP (VarN v) (VarN "u") NilP)

s :: String -> Process
s x = InputP (VarN x) "y" (SourceP x "u" (ReplicateP (VarN "y") "z" (AppP "p" (VarN "u"))))

t :: String -> Process
t x = NewP "q" (VarT "Q") (OutputP (VarN x) (VarN "q") (ParP (AppP "q" (VarN "q")) (AppP "r" (VarN x))))

q :: String -> Process
q x = ParP (NewP "k1" (VarT "K1") (OutputP (VarN x) (VarN "k1") (VarP "q1")))
           (NewP "k2" (VarT "K2") (OutputP (VarN x) (VarN "k2") (VarP "q2")))

c :: Process
c = NewP "x" (VarT "A") (ParP (s "x") (t "x"))

defaultGamma :: Env Scheme
defaultGamma = Env $ Map.fromList
    [ ("p", Scheme ["A", "G"] ["v"] (mempty & (delta %~ introduce "z" (VarT "A")) . (theta %~ introduce "v" (VarT "G"))))
    , ("q1", Scheme ["A"] [] (mempty & (delta %~ introduce "y" (VarT "A")) . (theta %~ introduce "u" (VarT "A"))))
    , ("q2", Scheme ["A"] [] (mempty & (delta %~ introduce "y" (VarT "A")) . (theta %~ introduce "u" (VarT "A"))))
    , ("q", Scheme ["Q"] ["q"] (mempty & delta %~ introduce "q" (VarT "Q")))
    , ("r", Scheme ["A"] ["v"] (mempty & delta %~ introduce "v" (VarT "a")))
    ]

main :: IO ()
main = case runInfer (infer (c :⊢ mempty)) defaultGamma of
    Left (InferError err) -> putDoc (err <> char '\n')
    Right ctx -> print ctx
