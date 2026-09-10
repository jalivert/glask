# Glask

[![Haskell CI](https://github.com/jalivert/glask/actions/workflows/haskell.yml/badge.svg)](https://github.com/jalivert/glask/actions/workflows/haskell.yml)

## Background

This repository contains the implementation accompanying the master's thesis *Implementation of a
statically typed, lazy, pure functional programming language* (Jan Liam Verter, Faculty of
Information Technology, Czech Technical University in Prague, 2022). The [thesis text](https://dspace.cvut.cz/server/api/core/bitstreams/de31dcfd-386b-4860-a095-26b0588c091c/content) is available
online.

The implementation follows three papers in particular: Mark P. Jones's [*Typing Haskell in
Haskell*](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf) for the constraint-based type
inference core, Simon Peyton Jones, Vytiniotis, Weirich and Shields's [*Practical Type Inference for
Arbitrary-Rank Types*](https://doi.org/10.1017/S0956796806006034) for higher-rank types, and
Peterson and Jones's [*Implementing Type Classes*](https://doi.org/10.1145/173262.155112) for the
dictionary-passing elaboration of type classes.

## What Glask is

Glask is a small, lazy, pure, statically typed functional language in the spirit of Haskell. A
program is a module of declarations: fixity declarations, type classes with instances, data types
(including higher-kinded ones), type synonyms, and (possibly annotated) function bindings. There is
no compiler backend; programs are elaborated into a small core language and run by a non-strict
interpreter with an interactive REPL.

## Syntax

A program is a module: a header (`module Main where`) followed by declarations.
There are seven kinds: fixity signatures, type classes, instances, data types,
type synonyms, type signatures, and bindings. Blocks use implicit layout —
braces and semicolons are accepted but never required:

```haskell
module Main where
class Num a where
  (+) :: a -> a -> a

instance Num Int where
  (+) x y = int#+ (x, y)
```

Every program in `examples/positive` exists in both notations (the layout
variant lives next to the original as `*.layout.glask`).

### Operators with any fixity

This is where Glask departs from Haskell. Operators come in three fixities —
prefix, infix, postfix — each with an associativity (left, right, none) and a
precedence (0–9). That includes unary operators: a prefix operator can be left
associative, a postfix one right associative, and an implicit rule picks the
only sensible reading. Any ordinary function doubles as an operator when
wrapped in backticks — including prefix and postfix ones, which Haskell does
not allow. Application chains are resolved by the author's own variation of
Dijkstra's shunting-yard algorithm (thesis §1.8 and §2.3.3), extended with
unary operators and same-precedence mixing. One rule to know: an operator used
in an expression needs a fixity declaration in scope.

The thesis introduces the triangle `|>` — the author's pizza operator, named for
the slice it resembles. Combined with a postfix `!` over an ordinary infix `+`,
precedence alone disambiguates the chain:

```haskell
prefix 9 |>
postfix 9 !
infixl 5 +

(|>) :: Int -> Int
(|>) x = x

(!) :: Int -> Int
(!) x = x

expr = |> 1 + 2 !
```

`expr` parses as `(|> 1) + (2 !)` and evaluates to `3`.

And a backticked function as a weak prefix operator (thesis §1.8, live in
`examples/prelude/protolude.glask` as ``prefixr 0 `print` ``):

```haskell
infixl 5 +

prefix 0 `print`

print :: Int -> Int
print x = x

shout = `print` 1 + 2
```

`shout` parses as `` `print` (1 + 2) `` and evaluates to `3`.

## A tour

Declarations use implicit layout instead of explicit braces (every example below
also exists in brace form in the test suite; layout twins live next to the
originals as `*.layout.glask`).

Type classes with dictionary passing (`examples/positive/evaluate/arith.glask`):

```haskell
class Num a where
  (+) :: a -> a -> a

instance Num Int where
  (+) x y = int#+ (x, y)

infixl 5 +
```

The expression `2 + 3` then evaluates to `5` (covered by the test suite).

Higher-rank polymorphism (`examples/positive/prenex/nested.glask`): a constrained polymorphic
function passed as an argument, and a locally-bound annotated binding used at higher rank:

```haskell
class Num a where
  (+) :: a -> a -> a

instance Num Int where
  (+) x y = int#+ (x, y)

infixl 5 +

apply :: (forall a . Num a => a -> a) -> Int -> Int
apply h x = h x

inc :: Num a => a -> a
inc x = x + x

t'local'rank = let loc :: Num a => a -> a
                   loc = \ x -> x + x
               in apply loc 3
```

Data types and pattern matching (`examples/positive/evaluate/matching.glask`):

```haskell
data Maybe a = Nothing | Just a

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (a : as) = Just as
```

User-defined operators with custom fixity (`examples/positive/operators/infix.glask`):

```haskell
infixl 5 +
infixl 6 *

program = 1 + 2 * 3
```

Polymorphic local bindings get per-use dictionaries (`examples/positive/prenex/local.glask`):

```haskell
t'over'poly = let f :: Ident a => a -> a
                  f = \x -> ident x
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
REPL loads `examples/prelude/prelude.glask`.

## Building and testing

Requires GHC (CI uses 9.10.1) and Cabal:

```
$ cabal build all
$ cabal test all
```

The test suite typechecks every program in `examples/positive`, evaluates selected
terms, and checks inferred schemes — currently 1052 examples, 0 failures. Each
`*.layout.glask` twin additionally has a parse golden snapshot.

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
                       (`*.layout.glask` twins cover implicit layout)
examples/prelude       the REPL preludes (`prelude`, `protolude`, …)
examples/scratch       untried example drafts
examples/negative      programs that must fail (typecheck or parse)
app                    the `glask-exe` entry point
test                   Hspec suite driving the examples
```
