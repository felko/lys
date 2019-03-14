module Main where

import Language.Lys.Inference
import Language.Lys.Pretty
import Language.Lys.Types

import qualified Data.Map as Map

import Text.PrettyPrint.ANSI.Leijen hiding ((<>))

ctx :: Context
ctx = Context
    { _typeNames = Map.fromList
        [ ("Bool", Scheme mempty (RecordT (SumRT (Field "true" (RecordT EmptyRT)) (SumRT (Field "false" (RecordT EmptyRT)) EmptyRT))))
        , ("Maybe", Scheme (singletonTV "a") (RecordT (SumRT (Field "just" (VarT "a")) (SumRT (Field "nothing" (RecordT EmptyRT)) EmptyRT))))
        , ("Player", Scheme mempty (RecordT (ProdRT (Field "id" IntT) (ProdRT (Field "name" StringT) (ProdRT (Field "health" IntT) EmptyRT)))))
        ]
    , _typeEnv = Map.fromList
        [ ("player", Scheme mempty (IdentT "Player"))
        , ("print", Scheme (singletonTV "a") (VarT "a"))
        ]
    , _sessionEnv = Map.fromList []
    }

n1 :: Name
n1 = FieldN (VarN "player") "name"

test :: Process
test = NewP "player" (IdentT "Player")
        $ InputP (FieldN (VarN "player") "health") "h"
        $ OutputP (VarN "print") (VarN "h")

write100 = ParP
    (OutputP (FieldN (VarN "player") "health") (LitN (IntL 100)))
    (InputP (FieldN (VarN "player") "name") "n" (OutputP (VarN "print") (VarN "n")))

proc = ProcP "player" (IdentT "Player") write100

mapMaybe = ProcP "f" (QuoteT (ProcS "x" (VarT "a") (ProcS "y" (VarT "b") (ReadS (VarN "x") (WriteS (VarN "y")))))) (ProcP "mx" (RecordT (SumRT (Field "just" (VarT "a")) (SumRT (Field "nothing" (RecordT EmptyRT)) EmptyRT))) (ProcP "my" (RecordT (SumRT (Field "just" (VarT "b")) (SumRT (Field "nothing" (RecordT EmptyRT)) EmptyRT))) p))
    where p = ParP
                (CallP (CallP (DropP (VarN "f")) (FieldN (VarN "mx") "just")) (FieldN (VarN "my") "just"))
                (InputP (FieldN (VarN "mx") "nothing") "_" (OutputP (FieldN (VarN "my") "nothing") (RecN (ProdR []))))

mapMaybe' = ProcPI "f" (ProcPI "mx" (ProcPI "my" p))
    where p = ParP
                (InputP (FieldN (VarN "mx") "nothing") "_" (OutputP (FieldN (VarN "my") "nothing") (RecN (ProdR []))))
                (CallP (CallP (DropP (VarN "f")) (FieldN (VarN "mx") "just")) (FieldN (VarN "my") "just"))

main :: IO ()
main = case runInfer (inferProcess' mapMaybe') ctx of
    Left (InferError err) -> putDoc (err <> char '\n')
    Right s               -> prettyPrint s
