module EvalSpec where


import Test.Hspec

import System.IO

import qualified Data.Map.Strict as Map
import Control.Monad.State ( runState )

import Compiler.Syntax.Term
import Compiler.Syntax.Literal
import Compiler.Syntax.Type ( Type )

import Compiler.TypeSystem.InferenceEnv ( kind'env )
import Compiler.TypeSystem.InferenceEnv as I'Env
import qualified Compiler.TypeSystem.InferenceState as I'State
import Compiler.TypeSystem.InferenceState ( Infer'State )

import qualified Interpreter.Core as Core
import qualified Compiler.Syntax.ToAST.TranslateEnv as TE
import Compiler.Syntax.ToAST.TranslateEnv

import Interpreter.Evaluate ( eval )
import Interpreter.ToCore ( section'to'core, data'to'core, to'core )

import Compiler.Counter

import REPL.Expression ( read'expr, infer'expr'type )
import REPL.Load ( load'declarations, process'declarations, build'env'store )
import Interpreter.Value ( Value(..) )
import Compiler.Syntax ( Expression(..), Literal(..) )

import Compiler.TypeSystem.Program ( Program( Program, data'declarations, bind'section, b'sec'core, environment, store) )


-- | A single evaluator case: display name, fixture file, expression,
--   and the exact expected 'Value'. Every expectation below was derived
--   by running the language's own evaluator (via the prebuilt REPL).
type Eval'Case = (String, String, String, Value)

eval'cases :: [Eval'Case]
eval'cases =
  [ ("int addition", ev'arith, "2 + 3", Literal (Lit'Int 5))
  , ("int subtraction", ev'arith, "10 - 4", Literal (Lit'Int 6))
  , ("int multiplication", ev'arith, "6 * 7", Literal (Lit'Int 42))
  , ("precedence: * binds tighter than +", ev'arith, "2 + 3 * 4", Literal (Lit'Int 14))
  , ("parens override precedence", ev'arith, "(2 + 3) * 4", Literal (Lit'Int 20))
  , ("double negation", ev'arith, "neg (neg 8)", Literal (Lit'Int 8))
  , ("user function on compound argument", ev'arith, "square (2 + 1)", Literal (Lit'Int 9))
  , ("equality holds", ev'arith, "(2 + 2) == 4", Data "True" [])
  , ("less-than holds", ev'arith, "3 < 4", Data "True" [])
  , ("greater-than fails", ev'arith, "4 > 9", Data "False" [])
  , ("not of a false equality", ev'arith, "not (1 == 2)", Data "True" [])
  , ("conjunction with negation", ev'arith, "and True (not False)", Data "True" [])
  , ("disjunction of falses", ev'arith, "or False False", Data "False" [])
  , ("integer division truncates", ev'arith, "div 7 2", Literal (Lit'Int 3))
  , ("integer division remainder dropped", ev'arith, "div 7 3", Literal (Lit'Int 2))
  , ("char literal pattern match", ev'arith, "case 'a' of { 'a' -> 65; _ -> 0 }", Literal (Lit'Int 65))
  , ("double binding evaluates", ev'arith, "double'd", Literal (Lit'Double 2.5))
  , ("char literal evaluates", ev'arith, "'a'", Literal (Lit'Char 'a'))
  , ("length of finite prefix of endless list", ev'lazy, "length (take 4 endless'list)", Literal (Lit'Int 4))
  , ("head of endless list", ev'lazy, "head endless'list", Literal (Lit'Int 23))
  , ("sum of finite prefix of endless list", ev'lazy, "sum'list (take 3 endless'list)", Literal (Lit'Int 69))
  , ("head after take on infinite ones", ev'lazy, "head (take 5 ones)", Literal (Lit'Int 1))
  , ("length of concrete list", ev'lazy, "length normal'list", Literal (Lit'Int 4))
  , ("sum of concrete list", ev'lazy, "sum'list normal'list", Literal (Lit'Int 10))
  , ("laziness: unused diverging argument", ev'lazy, "kestrel 5 loop", Literal (Lit'Int 5))
  , ("head of infinite ones", ev'lazy, "head ones", Literal (Lit'Int 1))
  , ("arithmetic on taken head", ev'lazy, "1 + head (take 1 endless'list)", Literal (Lit'Int 24))
  , ("longer finite prefix", ev'lazy, "length (take 10 endless'list)", Literal (Lit'Int 10))
  , ("sum of taken ones", ev'lazy, "sum'list (take 4 ones)", Literal (Lit'Int 4))
  , ("sum of empty list", ev'lazy, "sum'list []", Literal (Lit'Int 0))
  , ("head of constructed cons", ev'lazy, "head (2 : [])", Literal (Lit'Int 2))
  , ("empty take matches nil", ev'lazy, "case take 0 endless'list of { [] -> 1; _ -> 0 }", Literal (Lit'Int 1))
  , ("fromMaybe on Just", ev'data, "fromMaybe 0 (Just 5)", Literal (Lit'Int 5))
  , ("fromMaybe on Nothing", ev'data, "fromMaybe 0 Nothing", Literal (Lit'Int 0))
  , ("isJustNum on Just", ev'data, "isJustNum (Just 5)", Literal (Lit'Int 1))
  , ("isJustNum on Nothing", ev'data, "isJustNum Nothing", Literal (Lit'Int 0))
  , ("case on Just", ev'data, "caseDemo (Just 42)", Literal (Lit'Int 42))
  , ("case on Left", ev'data, "eitherDemo (Left 3)", Literal (Lit'Int 3))
  , ("first of pair", ev'data, "fst (5, 6)", Literal (Lit'Int 5))
  , ("record field projection", ev'data, "getx origin", Literal (Lit'Int 3))
  , ("record update then projection", ev'data, "getx (bump origin)", Literal (Lit'Int 9))
  , ("length after map with lambda", ev'data, "length (map (\\x -> x + 1) [1,2,3])", Literal (Lit'Int 3))
  , ("head after map with lambda", ev'data, "head (map (\\x -> x + 1) [10,20])", Literal (Lit'Int 11))
  , ("case on swapped pair", ev'data, "case swapPair (Pair 1 2) of { Pair a b -> a }", Literal (Lit'Int 2))
  , ("unit pattern match", ev'data, "case () of { () -> 9 }", Literal (Lit'Int 9))
  , ("nullary constructor True", ev'data, "True", Data "True" [])
  , ("nullary constructor Nothing", ev'data, "Nothing", Data "Nothing" [])
  , ("nullary constructor unit", ev'data, "()", Data "()" [])
  , ("infix precedence * over +", ev'ops, "1 + 2 * 3", Literal (Lit'Int 7))
  , ("parens over infix precedence", ev'ops, "(1 + 2) * 3", Literal (Lit'Int 9))
  , ("left-associated chain of -", ev'ops, "10 - 3 - 2", Literal (Lit'Int 5))
  , ("mixed * and +", ev'ops, "2 * 3 + 4 * 5", Literal (Lit'Int 26))
  , ("custom operator +++", ev'ops, "2 +++ 3", Literal (Lit'Int 6))
  , ("custom operator ***", ev'ops, "2 *** 3", Literal (Lit'Int 11))
  , ("prefix operator", ev'ops, "!!! 5", Literal (Lit'Int (-5)))
  , ("prefix then postfix", ev'ops, "!!! 5 ???", Literal (Lit'Int 95))
  , ("prefix with custom infix", ev'ops, "!!! 3 +++ 4", Literal (Lit'Int 2))
  , ("postfix with custom infix", ev'ops, "100 ??? +++ 1", Literal (Lit'Int 202))
  , ("higher-order twice", ev'ops, "twice inc 5", Literal (Lit'Int 7))
  , ("nested application", ev'ops, "inc (twice inc 5)", Literal (Lit'Int 8))
  , ("subtraction of product", ev'ops, "10 - 2 * 3", Literal (Lit'Int 4))
  , ("explicitly nested custom operator", ev'ops, "1 +++ (2 +++ 3)", Literal (Lit'Int 8))
  , ("product of sums", ev'ops, "(2 + 3) * (4 - 1)", Literal (Lit'Int 15))
  , ("chain of multiplications", ev'ops, "2 * 2 * 2", Literal (Lit'Int 8))
  , ("nested let block", ev'nest, "double'loc", Literal (Lit'Int 3))
  , ("let shadowing", ev'nest, "shadow", Literal (Lit'Int 2))
  , ("shadowing across sibling lets", ev'nest, "shadow'outer", Literal (Lit'Int 3))
  , ("case true branch", ev'nest, "choose True", Literal (Lit'Int 1))
  , ("case false branch", ev'nest, "choose False", Literal (Lit'Int 0))
  , ("nested case first", ev'nest, "nested'case 0", Literal (Lit'Int 10))
  , ("nested case second", ev'nest, "nested'case 1", Literal (Lit'Int 11))
  , ("nested case fallback", ev'nest, "nested'case 9", Literal (Lit'Int 12))
  , ("nested if first", ev'nest, "if'nest 0", Literal (Lit'Int 100))
  , ("nested if second", ev'nest, "if'nest 1", Literal (Lit'Int 101))
  , ("nested if fallback", ev'nest, "if'nest 5", Literal (Lit'Int 102))
  , ("recursion: factorial", ev'nest, "fact 5", Literal (Lit'Int 120))
  , ("recursion: fibonacci", ev'nest, "fib 10", Literal (Lit'Int 55))
  , ("closure over let binding", ev'nest, "closure'test", Literal (Lit'Int 15))
  , ("immediate beta reduction", ev'nest, "beta", Literal (Lit'Int 42))
  , ("local lambda binding", ev'nest, "let'fun", Literal (Lit'Int 42))
  , ("multiplication", ev'arith, "3 * 4", Literal (Lit'Int 12))
  , ("square applied", ev'arith, "square 5", Literal (Lit'Int 25))
  , ("negation", ev'arith, "neg 3", Literal (Lit'Int (-3)))
  , ("conjunction true", ev'arith, "and True True", Data "True" [])
  , ("disjunction true", ev'arith, "or True False", Data "True" [])
  , ("negation of true", ev'arith, "not True", Data "False" [])
  , ("exact division", ev'arith, "div 9 3", Literal (Lit'Int 3))
  , ("square of sum", ev'arith, "square (1 + 2)", Literal (Lit'Int 9))
  , ("char literal", ev'arith, "'b'", Literal (Lit'Char 'b'))
  , ("negation of false", ev'arith, "not False", Data "True" [])
  , ("conjunction false", ev'arith, "and False True", Data "False" [])
  , ("disjunction with false", ev'arith, "or False True", Data "True" [])
  , ("negative result", ev'arith, "10 - 20", Literal (Lit'Int (-10)))
  , ("division truncates", ev'arith, "div 10 3", Literal (Lit'Int 3))
  , ("negation of zero", ev'arith, "neg 0", Literal (Lit'Int 0))
  , ("negated equality", ev'arith, "not (2 == 2)", Data "False" [])
  , ("equality holds", ev'arith, "(1 + 1) == 2", Data "True" [])
  , ("less-than fails", ev'arith, "3 < 2", Data "False" [])
  , ("greater-than holds", ev'arith, "5 > 4", Data "True" [])
  , ("fractional binding", ev'arith, "double'd", Literal (Lit'Double 2.5))
  , ("matching require", matching, "require 10", Literal (Lit'Int 10))
  , ("matching projection", matching, "the'what 9", Literal (Lit'Int 3))
  , ("double negation", ev'arith, "neg (neg 5)", Literal (Lit'Int 5))
  , ("square composed", ev'arith, "square (square 2)", Literal (Lit'Int 16))
  , ("division of sum", ev'arith, "div (7 + 5) 4", Literal (Lit'Int 3))
  , ("conjunction of equality", ev'arith, "and (1 == 1) True", Data "True" [])
  , ("disjunction of comparisons", ev'arith, "or (2 > 3) (3 > 2)", Data "True" [])
  , ("mixed precedence chain", ev'arith, "1 + 2 * 3 - 4", Literal (Lit'Int 3))
  , ("matching nested require", matching, "require (require 2)", Literal (Lit'Int 2))
  , ("negated comparison", ev'arith, "not (3 > 4)", Data "True" [])
  , ("double addition", ev'arith, "double'd + double'd", Literal (Lit'Double 5.0))
  , ("double subtraction", ev'arith, "double'd - double'd", Literal (Lit'Double 0.0))
  , ("double multiplication", ev'arith, "double'd * double'd", Literal (Lit'Double 6.25))
  , ("applied plus operator", ev'arith, "(+) 1 2", Literal (Lit'Int 3))
  , ("parenthesised operator application", ev'ops, "(+) 2 3", Literal (Lit'Int 5))
  ]
  where
    ev'arith = "./examples/positive/evaluate/ev-arith.glask"
    ev'lazy  = "./examples/positive/evaluate/ev-lazy.glask"
    ev'data  = "./examples/positive/evaluate/ev-data.glask"
    ev'ops   = "./examples/positive/evaluate/ev-ops.glask"
    ev'nest  = "./examples/positive/evaluate/ev-nest.glask"
    matching = "./examples/positive/evaluate/matching.glask"


spec :: Spec
spec = describe "Evaluator cases over ev- fixtures" $ mapM_ run'case eval'cases
  where
    run'case (name, fixture, expr, expected) =
      it (name ++ "  [" ++ expr ++ "]") $ do
        r <- eval'within expr fixture
        r `shouldBe` Right expected


eval'within :: String -> String -> IO (Either String Value)
eval'within expr file'name = load file'name counter expr
  where counter   = Counter { counter = 0 }


load :: String -> Counter -> String -> IO (Either String Value)
load file'name counter expr = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle

  case load'declarations contents counter of
    Left sem'err -> do
      return $ Left $ show sem'err

    Right (decls, trans'env, counter') -> do

      case process'declarations decls trans'env counter' of
        Left err -> do
          return $ Left $ show err

        Right (program, infer'env, trans'env', counter'', infer'state) -> do
          let k'e = kind'env infer'env

          let b'section = bind'section program
          let b'sec'core = section'to'core b'section

          let data'decls = data'declarations program

          let unit'constr = Core.Binding "()" (Core.Intro "()" [])
          let cons'constr = Core.Binding ":" (Core.Abs "a" (Core.Abs "as" (Core.Intro ":" [Core.Var "a", Core.Var "as"])))
          let nil'constr  = Core.Binding "[]" (Core.Intro "[]" [])

          let constructor'core = unit'constr : cons'constr : nil'constr : data'to'core data'decls

          let (env, stor) = build'env'store $ b'sec'core ++ constructor'core

          repl'expr expr (program{ b'sec'core = b'sec'core ++ constructor'core, environment = env, store = stor }, infer'env, trans'env'{ TE.kind'context = k'e `Map.union` (TE.kind'context trans'env')}, counter'', infer'state)


repl'expr :: String -> (Program, Infer'Env, Translate'Env, Counter, Infer'State Type) -> IO (Either String Value)
repl'expr expr (program@Program{ environment = environment, store = store }, i'env@Infer'Env{ kind'env = k'env, type'env = type'env, class'env = class'env }, trans'env, counter, infer'state) = do
  case read'expr expr trans'env counter of
    Left trans'err -> do
      return $ Left $ show trans'err

    Right (expr', counter''') -> do
      let inf'env = i'env{ I'Env.instances = I'State.instances infer'state, I'Env.overloaded = I'State.overloaded infer'state }

      let error'or'scheme = infer'expr'type expr' inf'env counter'''

      case error'or'scheme of
        Left err -> do
          return $ Left $ show err

        Right (_, expr'', _) -> do
          let core'expr = to'core expr''
          let (res, _) = runState (eval core'expr environment) store
          case res of
            Left eval'err ->
              return $ Left $ show eval'err

            Right value ->
              return $ Right value
