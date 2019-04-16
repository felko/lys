Lys
===

A concurrent programming language based on typed π-calculus.

### Syntax

#### Processes

The language is an extension of synchronous π-calculus:
- `end`: terminated session/inaction
- `P | Q`: execute `P` and `Q` in parallel
- `new x: t { P }`: create new channel `x` of type `t` in the process `P`
- `x?(y), P`: read channel `x`, bind the received value to `y` and continue with `P`
- `x!(y), P`: write on channel `x` the value to `y` and continue with `P`
- `repeat x?(y), P`: persistent read
- `repeat x!(y), P`: persistent write
- `x.f!(y), P`: construct `x` with field `f` provided the channel `y`, and continue with `P`
- `match x { f1(x): P1 | … | fn(x): Pn }`: pattern patch over `+`-type
- `p(x1, …, xn)`: spawn top-level process `p` on channels `x1`, …, `xn`

### Type system

The type system used is called πCLL, defined in "Linear logic propositions as session types" — Luis Caires,
Franck Pfenning and Bernardo Toninho, and corresponds to full intuitionistic linear logic.

The logical connectives and truth values have been modified to be unicode characters

A type in Lys can be either:
- a primitive type like `Int`, `Float`, etc...
- `A | B`: read a value of type `A` and continue with `B` (_par_ or ⅋)
- `A; B`: write a value of type `A` and continue with `B` (_tensor_ or ⊗)
- `&{ fi: Ti }`: additive conjunction (_with_ or &)
- `+{ fi: Ti }`: additive disjunction (_plus_ or ⊕)
- `!A`: can read multiple times a channel of type `A`s (_of course_)
- `?A`: can write multiple times on a channel of type `A`
- `~A`: dual of type `A`
- `1`: multiplicative truth
- `⊥`: multiplicative falsity
- `0`: additive falsity
- `⊤`: additive truth

### Roadmap

- [x] Type checking
- [x] Type inference
- [x] Parser
- [ ] Compiler
    - [ ] VM
    - [ ] Code generation
    - [ ] (non tracing) garbage collection?
- Type system extensions:
    - [ ] Inductive and coinductive sessions
    - [ ] Higher-rank polymorphism
    - [ ] Effects
