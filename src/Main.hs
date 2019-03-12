module Main where

import Language.Lys.Core
import Language.Lys.Pretty
import Language.Lys.Types

import qualified Data.Map as Map

import Text.PrettyPrint.ANSI.Leijen hiding ((<>))

ctx :: Context
ctx = Context
    { _typeNames = Map.fromList
        [ ("Bool", Scheme mempty (VariantT (ExtendT "true" (RecordT EmptyT) (ExtendT "false" (RecordT EmptyT) EmptyT))))
        , ("Maybe", Scheme (singletonTV "a") (VariantT (ExtendT "just" (VarT "a") (ExtendT "nothing" (RecordT EmptyT) EmptyT))))
        , ("Player", Scheme mempty (RecordT (ExtendT "id" IntT (ExtendT "name" StringT (ExtendT "health" IntT EmptyT)))))
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

mapMaybe = ProcP "f" (QuoteT (ProcS "x" (VarT "a") (ProcS "y" (VarT "b") (ReadS (VarN "x") (WriteS (VarN "y")))))) (ProcP "mx" (VariantT (ExtendT "just" (VarT "a") (ExtendT "nothing" (RecordT EmptyT) EmptyT))) (ProcP "my" (VariantT (ExtendT "just" (VarT "b") (ExtendT "nothing" (RecordT EmptyT) EmptyT))) p))
    where p = ParP
                (CallP (CallP (DropP (VarN "f")) (FieldN (VarN "mx") "just")) (FieldN (VarN "my") "just"))
                (InputP (FieldN (VarN "mx") "nothing") "_" (OutputP (FieldN (VarN "my") "nothing") (RecN [])))

mapMaybe' = ProcPI "f" (ProcPI "mx" (ProcPI "my" p))
    where p = ParP
                (CallP (CallP (DropP (VarN "f")) (FieldN (VarN "mx") "just")) (FieldN (VarN "my") "just"))
                (InputP (FieldN (VarN "mx") "nothing") "_" (OutputP (FieldN (VarN "my") "nothing") (RecN [])))

main :: IO ()
main = case runInfer (inferProcess' mapMaybe') ctx of
    Left (InferError err) -> putDoc (err <> char '\n')
    Right s               -> prettyPrint s
