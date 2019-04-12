module Language.Lys.CodeGen where

data Instr
    = 
{-}
def Cell( self, u ) =
    self ? {
    read( r ) = r![u] | Cell[self, u],
    write( v ) = Cell[self, v]
    }
    in  new x Cell[x,9] | new y Cell[y,true]

============>

main = {
    def Cell = {
        objf 2
        put p0
        put p1
        trobj p0 = {
            { read, write }
            read = {
                msgf 1, 0
                put f1
                trmsg 2, Cell
                put f0
                put f1
            }
            write = {
                instof 2, Cell
                put f0
                put p0
            }
        }
    }
    newc c0
    instof 2, Cell
    put c0
    put 9
    instof 2, Cell
    put c1
    put true
}

type Reader<S> = !S

process reader<S>(x: S, c: Cell<S>) -> 

-}


= NilP
| ParP Process Process
| NewP String Type Process
| OutputP Name Name Process
| InputP Name String Process
| ReplicateP Name String Process
| InjectP Name String Process
| CaseP Name [(String,Process)]
| VarP String
| AppP String Name
| SourceP String String Process
| ContractP String String Process Process

newtype Gen a = Gen
    { unGen :: RWST () GenState [Instr] a }
