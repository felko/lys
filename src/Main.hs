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

-- mx: ~(Maybe A), my: Maybe A
fmapIdMaybe :: String -> String -> Process
fmapIdMaybe mx my = CaseP (VarN mx)
    [ Branch "just" (VarPat "x") (InjectP (VarN my) "just" (VarN "x") NilP)
    , Branch "nothing" WildcardPat (InjectP (VarN my) "nothing" (LitN OneL) NilP)
    ]

{-}
A -o B = ~A, B

type Maybe<A> = +{
    just: A
    nothing
}

process collapseMaybe<A>: (~Maybe)<Maybe<A>> -o Maybe<A>
                        : ~(&{ just: +{ just: A, nothing }, nothing }), Maybe<A>
                        : +{ just: &{ just: A, nothing }, nothing }, Maybe<A>
                        : Maybe<(~Maybe)<A>>, Maybe<A>
process collapseMaybe<A>(mmCh, m) {
    mmCh(mm), match mm {
        just: mm
    }
}


process mapId<A>(mx: ~Maybe<A>, my: Maybe<A>) {
    match mx {
        just(x): my.just!(x)
        nothing: my.nothing!
    }
}

⊢
⊢
⊢
⊢⊢⊢⊢⊢⊢⊢⊢⊢

type EitherChurch a b = forall r. (a -> r) -> (b -> r) -> r

left :: a -> EitherChurch a b
left x injl _ = injl x

right :: b -> EitherChurch a b
right y _ injr = injr y

bimap :: (a -> a') -> (b -> b') -> EitherChurch a b -> EitherChurch a' b'
bimap l r e injl injr = e (injl . l) (injr . r)

mapLeft :: (a -> a') -> EitherChurch a b -> EitherChurch a' b
mapLeft f = bimap f id

mapLeft :: (b -> b') -> EitherChurch a b -> EitherChurch a b'
mapLeft g = bimap id g

fromEither :: (a -> c) -> (b -> c) -> EitherChurch a b -> c
fromEither l r e = e l r

--}

-- testMapMaybe :: Process
-- testMapMaybe = NewP "mx" Nothing (ParP (CallP "fmapIdMaybe" [VarN "mx", VarN "my"]) (InjectP (VarN "mx") "nothing" NilP))

defaultGamma :: Gamma
defaultGamma = Env $ Map.fromList
    [ ("p", Scheme ["A", "G"] $ Scheme ["v"] (mempty & (delta %~ introduce "z" (VarT "A")) . (theta %~ introduce "v" (VarT "G"))))
    , ("q1", Scheme ["A"] $ Scheme [] (mempty & (delta %~ introduce "y" (VarT "A")) . (theta %~ introduce "u" (VarT "A"))))
    , ("q2", Scheme ["A"] $ Scheme [] (mempty & (delta %~ introduce "y" (VarT "A")) . (theta %~ introduce "u" (VarT "A"))))
    , ("q", Scheme [] $ Scheme ["q"] (mempty & delta %~ introduce "q" (IdentT "Q")))
    , ("r", Scheme ["A"] $ Scheme ["v"] (mempty & delta %~ introduce "v" (VarT "a")))
    , ("fmapIdMaybe", Scheme ["A"] $ Scheme ["mx", "my"] (mempty & (delta %~ introduce "mx" (DualT (AppT (IdentT "Maybe") [VarT "A"])))
                                                                 . (delta %~ introduce "my" (AppT (IdentT "Maybe") [VarT "A"]))))
    ]

defaultTau :: Env (Scheme Type Type)
defaultTau = Env $ Map.fromList
    [ ("Maybe", Scheme ["T"] (PlusT $ Map.fromList [("just", VarT "T"), ("nothing", OneT)]))
    , ("Int", Scheme [] OneT)
    , ("Q", Scheme [] OneT)
    ]

defaultEnv :: InferEnv
defaultEnv = InferEnv defaultGamma defaultTau

defaultCtx :: Context
defaultCtx = mempty
    & delta %~ introduce "mx" (VarT "X")  -- (DualT (AppT (IdentT "Maybe") [VarT "A"]))
             . introduce "my" (VarT "Y")  -- (AppT (IdentT "Maybe") [VarT "A"]) -- (AppT (IdentT "Maybe") [PrimT IntT])

main :: IO ()
main = case runInfer (infer (fmapIdMaybe "mx" "my" :⊢ defaultCtx)) defaultEnv of
    Left (InferError err) -> putDoc (err <> char '\n')
    Right ctx -> print ctx
