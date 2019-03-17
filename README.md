Lys
===

A concurrent programming language based on asynchronous π-calculus.

### Syntax

#### Names

| Name 	| Meaning 	| Conditions 	| Type 	|
|-------------------------	|---------------------------------------	|----------------------------------------	|-------------------------	|
| `{f: x}` 	| Construct variant-object 	| `x : T` 	| `{f: T \| ...}` 	|
| `{f1: x1, ..., fn: xn}` 	| Construct record-object 	| `xi : Ti` 	| `{f1: T1, ..., fn: Tn}` 	|
| `x.f` 	| Get the channel of `x` at field `f` 	| `x : {f : T, ...}` or `x : {f: T \| ...}` 	| `T` 	|
| ``` `p` ``` 	| Quotes process `p` 	| `p : s` 	| ``` `s` ``` 	|

#### Processes

|Process| Meaning 	| Conditions 	| Session 	|
|------------------------------	|----------------------------------------------------------------------------------	|--------------------------	|-------------------	|
| `x?y, p` 	| Receive value on channel `x`, bind it to `y` then executes `p` 	| `x : T, p : s` 	| `x?, hide(y, s)` 	|
| `x!y` 	| Send channel `y` over channel `x` 	| `x : T, y : T` 	| `x!` 	|
| `p \| q` 	| Executes `p` and `q` concurrently 	| `p : s1, q : s2`  	| `s1 \| s2` 	|
| `0` 	| The null process, does nothing 	|  	| `0` 	|
| `new x: T in p` 	| Defines a new channel `x` of type `T` then runs `p` 	| `p : s` 	| `hide(x, s)` 	|
| `select x { p1 \| ... \| pn }` 	| Pattern match on `x` by selecting a process among `p1`, ..., `pn` (must be exhaustive)	| `pi : x.fi?, s`	| `x?, s` 	|
| `init x { p1 \| ... \| pn }` 	| Initializes `x` by writing on all of its fields in processes `p1`, ..., `pn` 	| `pi : x.fi!` | `x!` 	|
| `match x y { p1 \| ... \| pn }` 	| Syntactic sugar for simultaneous `select x` and `init y` | `pi : x.fi?, y.gi!` | `x?, y!` 	|
| `proc(x: T) -> s { p }` 	| A process `p` of session `s` parametrized by a channel `x` of type `T` 	| `p : s` 	| `proc(x: T) -> s` 	|
| `p(a)` 	| Calls a process `p` with argument `x` 	| `a : T, proc(x: T) -> s` 	| `s` 	|
| `$x` 	| Executes a quoted process `x` 	| ``` x: `s` ``` 	| `s` 	|

### Type system

The type system is divided into two kinds: types (types of channels) and sessions (types of processes)

#### Types

A lys type can be either:
- a type identifier `Int`, `Float`, `Bool`, `String`
- a type variable `t`
- a record
    - a product type `{ f1: t1, ..., fn: tn }`
    - a sum type `{ f1: t1 | ... | fn: tn }`
- a quoted session (acts like a closure)

It features (for now) row-polymorphism and polymorphic variants so records can be extended.

#### Sessions

A session the type a processes, it can depend on channels:
- `proc(x: T) -> s`: expect an argument `x` of type `T` then behave like a process of session `s`
- `x?, s`: read channel `x` then behave like `s`
- `x!`: write on `x`
- `0`: the unit session
- `s | s'`: act as `s` and `s'` executed simultaneously
- `s where t1, ..., tn: Type, s1, ..., sn: Session`: Session scheme

### Roadmap

- [x] Type checking
- [ ] Type inference
- [ ] Parser
- [ ] Compiler
    - [ ] VM
    - [ ] Code generation
    - [ ] (non tracing) garbage collection?
- Type system extensions:
    - [ ] Inductive and coinductive sessions
    - [ ] Higher-rank polymorphism
    - [ ] Effects
