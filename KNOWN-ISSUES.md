# Known issues

Reproduced against the test suite (`cabal test glask-test`, GHC 9.12.4).
None of these fail the suite; each is either covered by a negative test
or left out of the positive tables deliberately.

1. Overloaded integer literal keeps its representation under annotation.
   `expl :: Double; expl = 23` evaluates to `Literal (Lit'Int 23)`, not a
   `Double`. Covered by ClassesSpec ("int literal under Double annotation
   stays Int").

2. Fixity names are comma-separated.
   `infixl 5 +, *` declares two fixities; `infixl 5 + *` is a parse error.
   Bare `-` and `:` are not accepted as fixity names. Covered by ParserSpec.
