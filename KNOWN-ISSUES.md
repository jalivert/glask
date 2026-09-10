# Known issues

Reproduced against the test suite (`cabal test glask-test`, GHC 9.12.4).
None of these fail the suite; each is either covered by a negative test
or left out of the positive tables deliberately.

1. `double#+` primitive not implemented.
   `double'd + double'd` over `examples/positive/evaluate/ev-arith.glask`
   returns `Left "Unexpected: Uh oh ... ('double#+') is not implemented yet"`.
   Covered by NegativeSpec ("unimplemented double primitive").

2. Kind inference rejects partially applied higher-kinded constructors.
   `:k "Test Maybe"` and `:k "Test Maybe Bool"` over
   `examples/positive/higher-kinded/higherkinded.glask` return
   `Left "INTERNAL Semantic Error: ... While assigning a Kind to a type
   constant ..."`. Fully applied `Test Maybe Int` kinds to `*` (KindSpec 013).

3. Parenthesised operator application rejected by the reader.
   `(+) 1 2` over `examples/positive/operators/infix.glask` returns
   `Left "Semantic Error: Missing Operand ..."`. Bare `(+)` infers fine.

4. Overloaded integer literal keeps its representation under annotation.
   `expl :: Double; expl = 23` evaluates to `Literal (Lit'Int 23)`, not a
   `Double`. Covered by ClassesSpec ("int literal under Double annotation
   stays Int").

5. Fixity names are comma-separated.
   `infixl 5 +, *` declares two fixities; `infixl 5 + *` is a parse error.
   Bare `-` and `:` are not accepted as fixity names. Covered by ParserSpec.
