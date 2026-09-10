# Known issues

Reproduced against the test suite (`cabal test glask-test`, GHC 9.12.4).
None of these fail the suite; each is either covered by a negative test
or left out of the positive tables deliberately.

No open issues. Last item (fixity declarations are comma-separated;
`infixl 6 -` and `infixr 5 :` both accepted, Haskell-compatible) verified
against the implementation and covered by ParserSpec.
