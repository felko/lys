Lys
===

A concurrent programming language based on asynchronous π-calculus.

### Syntax

#### Names

| Name 	| Meaning 	| Conditions 	| Type 	|
|-------------------------	|---------------------------------------	|----------------------------------------	|-------------------------	|
| `{f: x}` 	| Construct variant-object 	| `x : T` 	| `{f: T | ...}` 	|
| `{f1: x1, ..., fn: xn}` 	| Construct record-object 	| `xi : Ti` 	| `{f1: T1, ..., fn: Tn}` 	|
| `x.f` 	| Get the channel of `x` at field `f` 	| `x : {f : T, ...}` or `x : {f: T \| ...}` 	| `T` 	|
| ``` `p` ``` 	| Quotes process `p` 	| `p : s` 	| ``` `s` ``` 	|

#### Processes

|Process| Meaning 	| Conditions 	| Session 	|
|------------------------------	|----------------------------------------------------------------------------------	|--------------------------	|-------------------	|
| `x?y, p` 	| Receive value on channel `x`, bind it to `y` then executes `p` 	| `x : T, p : s` 	| `x?, hide(y, s)` 	|
| `x!y` 	| Send channel `y` over channel `x` 	| `x : T, y : T` 	| `x!` 	|
| `p | q` 	| Executes `p` and `q` concurrently 	| `p : s1, q : s2`  	| `s1 | s2` 	|
| `0` 	| The null process, does nothing 	|  	| `0 : 0` 	|
| `new x: T in p` 	| Defines a new channel `x` of type `T` then runs `p` 	| `p : s` 	| `hide(x, s)` 	|
| `select x { p1 | ... | pn }` 	| Choses a suitable process reading on `x` or a field of `x` among `p1`, ..., `pn` 	| `pi : s` 	| `x?, s` 	|
| `proc(x: T) -> s { p }` 	| A process `p` of session `s` parametrized by a channel `x` of type `T` 	| `p : s` 	| `proc(x: T) -> s` 	|
| `p(a)` 	| Calls a process `p` with argument `x` 	| `a : T, proc(x: T) -> s` 	| `s` 	|

### Roadmap

- [x] Type checking
- [ ] Type inference
- [ ] Parser
- [ ] Compiler
    - [ ] VM
    - [ ] Code generation

