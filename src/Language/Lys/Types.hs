module Language.Lys.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Lens.Micro

type Params = [(String, Type)]
type Args   = [Name]
type Label  = String

data Type
    = IntT | FloatT | CharT | StringT
    | RecordT RecordType
    | QuoteT Session
    | IdentT String
    | VarT String
    deriving (Eq, Ord, Show)

data RecordType
    = SumRT Field RecordType
    | ProdRT Field RecordType
    | EmptyRT
    | VarRT String
    deriving (Eq, Ord, Show)
    
data Field = Field Label Type
    deriving (Eq, Ord, Show)

accumulateFields :: RecordType -> ([Field], Maybe String)
accumulateFields (SumRT f r)  = accumulateFields r & _1 %~ (f:)
accumulateFields (ProdRT f r) = accumulateFields r & _1 %~ (f:)
accumulateFields EmptyRT      = ([], Nothing)
accumulateFields (VarRT n)    = ([], Just n)

mapFromRecordType :: RecordType -> (Map.Map Label Type, Maybe String)
mapFromRecordType = (_1 %~ Map.fromList . map (\ (Field l t) -> (l, t))) . accumulateFields

mapToRecordType :: (Field -> RecordType -> RecordType) -> Map.Map Label Type -> Maybe String -> RecordType
mapToRecordType k fs ext = foldr (k . uncurry Field) (maybe EmptyRT VarRT ext) (Map.assocs fs)

data Session
    = ReadS Name Session
    | WriteS Name
    | ParS Session Session
    | ProcS String Type Session
    | NilS
    | VarS String
    deriving (Eq, Ord, Show)

data Process
    = InputP  Name String Process
    | OutputP Name Name
    | NewP String Type Process
    | NewPI String Process
    | ParP Process Process
    | ProcP String Type Process
    | ProcPI String Process
    | CallP Process Name
    | VarP String
    | DropP Name
    | NilP
    deriving (Eq, Ord, Show)

data Name
    = FieldN Name String
    | LitN Literal
    | RecN Record
    | CaseN Name String
    | QuoteN Process
    | VarN String
    deriving (Eq, Ord, Show)

data Record
    = SumR Label Name
    | ProdR [(Label, Name)]
    deriving (Eq, Ord, Show)

data Literal
    = IntL Integer
    | FloatL Float
    | CharL Char
    | StringL String
    deriving (Eq, Ord, Show)
