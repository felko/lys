module Main where

import Language.Lys.TypeChecking
import Language.Lys.Types

import Control.Monad (forM_)

import qualified Data.Map as Map

import Lens.Micro.Mtl
import Text.PrettyPrint.ANSI.Leijen

bcIntBody :: Process
bcIntBody = NewP "tea" (VarT "Tea") (OutputP (VarN "s") (VarN "tea") (NewP "cc" (VarT "CC") (OutputP (VarN "s") (VarN "cc") (InputP (VarN "s") "r" NilP))))

p :: String -> Process
p v = NewP "u" (VarT "G") (OutputP (VarN v) (VarN "u") NilP)

s :: String -> Process
s x = InputP (VarN x) "y" (ReplicateP (VarN "y") "z" (VarP "p"))

defaultCtx :: Context
defaultCtx = Context
    { _delta = Map.fromList
        []
    , _theta = Map.fromList
        []
    }

defaultGamma :: Map.Map String Scheme
defaultGamma = Map.fromList
    [ ("p", Scheme ["A", "G"] ["v"] (Context (Map.fromList [("z", VarT "A")]) (Map.fromList [("v", VarT "G")])))
    ]

main :: IO ()
main = case runInfer (infer (InputP (VarN "x") "y" (SourceP "x" "u" (ReplicateP (VarN "y") "z" (AppP "p" (VarN "u")))) :⊢ defaultCtx)) defaultGamma of
    Left (InferError err) -> putDoc (err <> char '\n')
    Right ctx -> print ctx
