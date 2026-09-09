# Glask

[![Haskell CI](https://github.com/jalivert/glask/actions/workflows/haskell.yml/badge.svg)](https://github.com/jalivert/glask/actions/workflows/haskell.yml)

I am currently rebuilding the whole project from the ground up in a different repository. This repo hosts a source code of the implementation for my master's thesis project.

## Master's thesis implementation

This repository is the implementation accompanying my master's thesis:

**Implementation of a statically typed, lazy, pure functional programming language**
Bc. Jan Sliacký, Faculty of Information Technology, Czech Technical University in Prague, June 2022.
Supervisor: Ryan Michael Culpepper, Ph.D.

The full text is in the root of this repository: [Master's Thesis.pdf](Master's%20Thesis.pdf).

From the abstract: *"This thesis presents an implementation of a functional, statically typed programming
language inspired by Haskell. It mainly focuses on the challenges of implementing the type system
for such a language. The implementation is based on multiple resources covering implementations of
different type system features. The thesis also covers other aspects of the language
implementation — lexical and syntactic analysis, translation into a smaller functional core language,
and non-strict evaluation."*

The thesis assignment explored three areas: parsing (user-defined infix operators), the type system
(first-class polymorphism, higher-ranked types, user-definable type classes, higher-kinded data
types, type synonyms, explicit type annotations, typed holes), and evaluation (an interactive
interpreter with a REPL offering type and kind checking; no machine-code compiler, elaboration to a
small core language instead).

Since the defense, the code has been revived as a portfolio piece: the build was switched from
Stack to Cabal, licensed MIT, the stale CI was rebuilt around GHC 9.10.1, and a few long-standing
type-system bugs (prenex conversion of qualified types, dictionary elaboration of local bindings)
were fixed and covered with tests.

## What Glask is

Glask is a small, lazy, pure, statically typed functional language in the spirit of Haskell. A
program is a module of declarations: fixity declarations, type classes with instances, data types
(including higher-kinded ones), type synonyms, and (possibly annotated) function bindings. There is
no compiler backend; programs are elaborated into a small core language and run by a non-strict
interpreter with an interactive REPL.

## A tour

Type classes with dictionary passing (`examples/positive/evaluate/arith.glask`):

```haskell
; class Num a where
  { (+) :: a -> a -> a }

; instance Num Int where
  { (+) x y = int#+ (x, y) }
```

The expression `2 + 3` then evaluates to `5` (covered by the test suite).

Higher-rank polymorphism (`examples/positive/prenex/nested.glask`): a constrained polymorphic
function passed as an argument, and a locally-bound annotated binding used at higher rank:

```haskell
; apply :: (forall a . Num a => a -> a) -> Int -> Int
; apply h x = h x

; inc :: Num a => a -> a
; inc x = x + x

; t'local'rank = let { loc :: Num a => a -> a
                     ; loc = \ x -> x + x }
                 in apply loc 3
```

Data types and pattern matching (`examples/positive/evaluate/matching.glask`):

```haskell
; tail :: [a] -> Maybe [a]
; tail [] = Nothing
; tail (a : as) = Just as
```

User-defined operators with custom fixity (`examples/positive/operators/infix.glask`):

```haskell
; infixl 5 +
; infixl 6 *

; program = 1 + 2 * 3
```

Polymorphic local bindings get per-use dictionaries (`examples/positive/prenex/local.glask`):

```haskell
; t'over'poly = let { f :: Ident a => a -> a
                    ; f = \x -> ident x }
                in pick (f 5, f True)
```

## The REPL

```
$ cabal run glask-exe -- examples/positive/prenex/nested.glask
Glamorous Glask REPL.

Successfully loaded the file.

glask λ > :t apply
          apply :: (forall a . Num a => a -> a) -> Int -> Int
glask λ > :t inc
          inc :: (forall a . Num a => a -> a)
glask λ > apply inc 3
          a
```

(The last answer is `show` of `6`: the example `Show Int` instance prints every `Int` as `'a'`.)
REPL commands: `:t` infers a type scheme, `:k` infers a kind, `:c` shows the elaborated core,
`:d` shows the desugared expression, `:p` shows the parse, `:q` quits. With no file argument the
REPL loads `prelude.glask`.

## Building and testing

Requires GHC (CI uses 9.10.1) and Cabal:

```
$ cabal build all
$ cabal test all
```

The test suite (`test/ExamplesSpec.hs`) typechecks every file in `examples/positive`, evaluates
selected terms, and checks inferred schemes — currently 82 examples, 0 failures.

## Project structure

```
src/Compiler/Lexer     lexical analysis
src/Compiler/Parser    syntactic analysis (user-defined operators and fixities)
src/Compiler/Syntax    surface syntax AST and translation to it
src/Compiler/Analysis  semantic analyses (binding groups, methods, instances, synonyms)
src/Compiler/TypeSystem
  Kind                 kind inference
  Type/Infer           bidirectional type inference, constraint solving,
                       dictionary elaboration to placeholders
  Solver               unification and constraint solving
src/Interpreter        translation to the core language and the lazy evaluator
src/REPL               file loading, the interactive loop, expression queries
examples/positive      tested example programs, grouped by feature
app                    the `glask-exe` entry point
test                   Hspec suite driving the examples
```

## Status and future work

This snapshot preserves the thesis implementation with the maintenance described above. The
language is being re-designed from scratch in a separate repository; one concrete item already
planned is porting that rewrite's off-side-rule parser back here.
