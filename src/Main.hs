module Main where

import Language.Lys.TypeChecking
import Language.Lys.Types

import Control.Monad (forM_)

import qualified Data.Map as Map

import Control.Lens hiding (Context)

import Text.PrettyPrint.ANSI.Leijen

bcIntBody :: Process
bcIntBody = NewP "tea" (Just $ IdentT "N") (OutputP (VarN "s") (VarN "tea") (NewP "cc" (Just $ IdentT "I") (OutputP (VarN "s") (VarN "cc") (InputP (VarN "s") "r" NilP))))

p :: String -> Process
p v = NewP "u" (Just $ VarT "G") (OutputP (VarN v) (VarN "u") NilP)

s :: String -> Process
s x = InputP (VarN x) "y" (SourceP x "u" (ReplicateP (VarN "y") "z" (CallP "p" [VarN "u"])))

t :: String -> Process
t x = NewP "q" Nothing (OutputP (VarN x) (VarN "q") (ParP (CallP "q" [VarN "q"]) (CallP "r" [VarN x])))

q :: String -> Process
q x = ParP (NewP "k1" Nothing (OutputP (VarN x) (VarN "k1") (CallP "q1" [])))
           (NewP "k2" Nothing (OutputP (VarN x) (VarN "k2") (CallP "q2" [])))

c :: Process
c = NewP "x" Nothing (ParP (s "x") (t "x"))

cell :: Process
cell = NewP "s" Nothing (ParP (InputP (VarN "s") "v" (SourceP "s" "v" NilP)) (OutputP (VarN "s") (LitN (IntL 18)) NilP))

defaultGamma :: Gamma
defaultGamma = Env $ Map.fromList
    [ ("p", Scheme ["A", "G"] $ Scheme ["v"] (mempty & (delta %~ introduce "z" (VarT "A")) . (theta %~ introduce "v" (VarT "G"))))
    , ("q1", Scheme ["A"] $ Scheme [] (mempty & (delta %~ introduce "y" (VarT "A")) . (theta %~ introduce "u" (VarT "A"))))
    , ("q2", Scheme ["A"] $ Scheme [] (mempty & (delta %~ introduce "y" (VarT "A")) . (theta %~ introduce "u" (VarT "A"))))
    , ("q", Scheme [] $ Scheme ["q"] (mempty & delta %~ introduce "q" (IdentT "Q")))
    , ("r", Scheme ["A"] $ Scheme ["v"] (mempty & delta %~ introduce "v" (VarT "a")))
    ]

defaultTau :: Env (Scheme Type Type)
defaultTau = Env $ Map.fromList
    [ ("Maybe", Scheme ["T"] (PlusT $ Map.fromList [("just", VarT "T"), ("nothing", OneT)]))
    , ("Int", Scheme [] OneT)
    , ("Q", Scheme [] OneT)
    ]

defaultEnv :: InferEnv
defaultEnv = InferEnv defaultGamma defaultTau

main :: IO ()
main = case runInfer (infer (c :⊢ mempty)) defaultEnv of
    Left (InferError err) -> putDoc (err <> char '\n')
    Right ctx -> print ctx
