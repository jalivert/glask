# Glask

[![Haskell CI](https://github.com/jalivert/glask/actions/workflows/haskell.yml/badge.svg)](https://github.com/jalivert/glask/actions/workflows/haskell.yml)

## Background

This repository contains the implementation accompanying the master's thesis *Implementation of a
statically typed, lazy, pure functional programming language* (Jan Liam Verter, Faculty of
Information Technology, Czech Technical University in Prague, 2022). The [thesis text](https://dspace.cvut.cz/server/api/core/bitstreams/de31dcfd-386b-4860-a095-26b0588c091c/content) is available
online. The implementation has been revived and maintained since the defense;
post-defense fixes are recorded in the commit history.

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

The thesis introduces the triangle `|>`. Combined with a postfix `!` over an
ordinary infix `+`, precedence alone disambiguates the chain:

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

## Showcase

Three larger programs in `examples/positive/showcase/` serve as the language's
benchmarks. All are typechecked, evaluated, and REPL-transcripted by the
test suite, in brace and layout notation alike.

A lazy prime sieve (`showcase/sieve.glask`) — an infinite Eratosthenes sieve
over lazy lists. Integer division truncates, so `mod` is built from `/`, `*`,
and `-`:

```haskell
infixl 6 +
infixl 6 -
infixl 7 *
infixl 7 /
infix 4 ==
infixr 5 :

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a

instance Num Int where
  (+) x y = int#+ (x, y)
  (-) x y = int#- (x, y)
  (*) x y = int#* (x, y)

class Div a where
  (/) :: a -> a -> a

instance Div Int where
  (/) x y = int#/ (x, y)

class Eq a where
  (==) :: a -> a -> Bool

instance Eq Int where
  (==) x y = int#== (x, y)

data Bool = True | False

not True = False
not False = True

mod :: Int -> Int -> Int
mod x y = x - (x / y) * y

divides :: Int -> Int -> Bool
divides d n = mod n d == 0

from :: Int -> [Int]
from n = n : from (n + 1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs) = if p x then x : filter p xs else filter p xs

sieve :: [Int] -> [Int]
sieve (p : xs) = p : sieve (filter (\ x -> not (divides p x)) xs)

primes :: [Int]
primes = sieve (from 2)
```

`filter` stays fully polymorphic; the numeric spine is pinned to `Int` by
annotation. Left floating, its `(Num a, Div a, Eq a)` constraints have no use
site to resolve against, and the compiler rejects the program as ambiguous —
the same as Haskell. In the REPL:

```
glask λ > at 9 first'primes
          29
glask λ > :t sieve
          sieve :: [Int] -> [Int]
glask λ > :t divides
          divides :: Int -> Int -> Bool
```

An overloaded arithmetic DSL (`showcase/expr.glask`) — one program evaluated
at two types, the dictionary-passing benchmark:

```haskell
infixl 6 +
infixl 7 *
infixl 6 <+>

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a

instance Num Int where
  (+) x y = int#+ (x, y)
  (-) x y = int#- (x, y)
  (*) x y = int#* (x, y)

class Arith a where
  lit :: Int -> a
  add :: a -> a -> a
  mul :: a -> a -> a

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr

instance Arith Expr where
  lit n = Lit n
  add x y = Add x y
  mul x y = Mul x y

instance Arith Int where
  lit n = n
  add x y = x + y
  mul x y = x * y

(<+>) :: Expr -> Expr -> Expr
(<+>) x y = Add x y

prog :: Arith a => a
prog = add (lit 2) (mul (lit 3) (lit 4))

answer'int :: Int
answer'int = prog

answer'expr :: Expr
answer'expr = prog

sugared :: Expr
sugared = Lit 1 <+> Lit 2

eval'expr :: Expr -> Int
eval'expr (Lit n) = n
eval'expr (Add x y) = eval'expr x + eval'expr y
eval'expr (Mul x y) = eval'expr x * eval'expr y

check = eval'expr answer'expr
```

The same `prog` elaborates with the `Int` dictionary to `14` and with the
`Expr` dictionary to the syntax tree, and a tree evaluator agrees with the
direct computation:

```
glask λ > answer'int
          14
glask λ > check
          14
glask λ > :t answer'expr
          answer'expr :: Expr
```

A bare `prog` query is ambiguous — no use site selects a dictionary — and the
compiler reports it instead of guessing.

A rank-2 applicator (`showcase/rankn.glask`) — one polymorphic argument used
at two different types in a single body, which a prenex rank-1 signature
cannot express:

```haskell
infixl 6 +

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a

instance Num Int where
  (+) x y = int#+ (x, y)
  (-) x y = int#- (x, y)
  (*) x y = int#* (x, y)

instance Num Double where
  (+) x y = double#+ (x, y)
  (-) x y = double#- (x, y)
  (*) x y = double#* (x, y)

class Fractional a

instance Fractional Double

inc :: Num a => a -> a
inc x = x + x

apply'both :: (forall a . Num a => a -> a) -> (Int, Double)
apply'both h = (h 1, h 2.5)

both = apply'both inc
```

```
glask λ > first'both
          2
glask λ > :t apply'both
          apply'both :: (forall a . Num a => a -> a) -> (Int, Double)
```

(The transcript also checks `:t both`.) The rank-1 spelling of the same
program — `apply'mono :: Num a => (a -> a) -> (Int, Double)` — is rejected:
once `a` is fixed, it cannot be both `Int` and `Double`
(`examples/negative/rank/one.glask`, covered by the test suite).

## The type system

Type classes elaborate by dictionary passing (thesis §1.9 and §2.5):
overloaded identifiers are collected as constraints during inference and
rewritten to dictionary selections once the constraints are solved (thesis
§2.5.3). Instances can themselves be qualified
(`examples/prelude/prelude.glask`):

```haskell
class Num a

instance Num Int

data Wrap a = Wrap a

class Constant a where
  vaval :: a

instance Num a => Constant (Wrap a) where
  vaval = Wrap 23
```

Inference is bidirectional and constraint-based, in the style of *Typing
Haskell in Haskell* (thesis §1.4 and §2.4): literals and lambdas are checked
against expected types, predicates are gathered, reduced against the instance
environment, and whatever remains is generalized or kept as a qualified
scheme. Higher-rank types work in annotations and in locally-bound
polymorphic bindings alike (thesis §1.7 and §2.4.8 — the tour above shows
both).

Unsatisfiable constraints are errors, not warnings. A fractional literal
checked against `Int` fails for want of a `Fractional Int` instance:

`didn't find any instances of a class 'Fractional' for a type 'Int'`

and an ambiguous program is rejected with `cannot resolve ambiguity`.

## Data, matching, laziness

Data declarations take parameters of any kind, with kinds inferred by the
kind checker — including higher-kinded ones (thesis §1.6):

```haskell
data Maybe a = Nothing | Just a
data Record m a = Rec { a :: Int, b :: Maybe a, c :: m a }
```

(`examples/positive/data/records.glask`, where `m` gets kind `* -> *`).
Constructors are matched with first-match patterns over literals, variables,
wildcards, constructors, and tuples (thesis §1.6.4); `tail` above infers a
polymorphic list-to-`Maybe` scheme.

Evaluation is non-strict (thesis §1.12): infinite structures are ordinary
values. `ones = 1 : ones` (`examples/positive/evaluate/ev-lazy.glask`) would
diverge if fully forced and never needs to be — `take`, `head`, and the
sieve's `filter` only force the spine they consume, so `primes` is a genuine
infinite list of which the suite observes the first ten: 2, 3, 5, 7, 11, 13,
17, 19, 23, 29.

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
terms, and checks inferred schemes — currently 1112 examples, 0 failures. Each
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
examples/positive/showcase
                       flagship programs: a lazy prime sieve, an
                       overloaded arithmetic DSL, and a rank-2 applicator
examples/prelude       the REPL preludes (`prelude`, `protolude`, …)
examples/scratch       untried example drafts
examples/negative      programs that must fail (typecheck or parse)
app                    the `glask-exe` entry point
test                   Hspec suite driving the examples
```
