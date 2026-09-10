module NegativeSpec where


import Test.Hspec

import System.IO
import Control.Exception ( evaluate )
import Control.Monad.State ( runState )
import Data.Either ( isLeft )
import Data.Maybe ( isJust )

import qualified Data.Map.Strict as Map

import Compiler.Syntax.Term
import Compiler.Syntax.Literal
import Compiler.Syntax.Type ( Type )

import Compiler.Parser.Parser ( parse'module, parse'expr )

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


-- Every case in this file MUST fail. Each expectation was derived by
-- running the language itself (via the prebuilt REPL binary):
--   * malformed sources raise 'error' ("Parse/Lexical error ...") while
--     parsing, so 'evaluate (parse'expr src)' throws;
--   * ill-typed files make 'type'check' return 'Just err' (or throw, when
--     the file does not even parse);
--   * ill-typed expressions make 'eval'within' return 'Left err'.


spec :: Spec
spec = do
  describe "malformed sources are parse errors" $
    mapM_ run'parse parse'cases

  describe "ill-typed files fail type checking" $ do
    let one = "./examples/negative/annotations/one.glask"
    it "annotations/one is rejected" $
      type'check one `shouldSatisfyIO` isJust
    it "annotations/one mentions the kind context" $
      type'check one `shouldReturnSubstring` "kind context"
    it "annotations/one mentions Maybe" $
      type'check one `shouldReturnSubstring` "Maybe"
    it "annotations/one mentions a type constant" $
      type'check one `shouldReturnSubstring` "type constant"

    let two = "./examples/negative/annotations/two.glask"
    it "annotations/two is rejected" $
      type'check two `shouldSatisfyIO` isJust
    it "annotations/two mentions the kind context" $
      type'check two `shouldReturnSubstring` "kind context"
    it "annotations/two mentions Maybe" $
      type'check two `shouldReturnSubstring` "Maybe"
    it "annotations/two is an internal error" $
      type'check two `shouldReturnSubstring` "INTERNAL"

    let cone = "./examples/negative/classes/one.glask"
    it "classes/one is rejected" $
      type'check cone `shouldSatisfyIO` isJust
    it "classes/one is a shape error" $
      type'check cone `shouldReturnSubstring` "[Shape]"
    it "classes/one reports a kind mismatch" $
      type'check cone `shouldReturnSubstring` "Couldn't match kind"
    it "classes/one mentions * -> *" $
      type'check cone `shouldReturnSubstring` "* -> *"

    let ctwo = "./examples/negative/classes/two.glask"
    it "classes/two does not even parse" $
      type'check ctwo `shouldThrow` anyErrorCall
    it "classes/two fails in parse'module" $ do
      src <- readFile ctwo
      (evaluate (parse'module src)) `shouldThrow` anyErrorCall

    let super = "./examples/negative/classes/super.glask"
    it "classes/super is rejected" $
      type'check super `shouldSatisfyIO` isJust
    it "classes/super reports an unsatisfiable predicate" $
      type'check super `shouldReturnSubstring` "unsatisfiable"
    it "classes/super mentions class Super" $
      type'check super `shouldReturnSubstring` "'Super'"
    it "classes/super mentions type Int" $
      type'check super `shouldReturnSubstring` "'Int'"

    let hexpl = "./examples/negative/holes/explicit.glask"
    it "holes/explicit is rejected" $
      type'check hexpl `shouldSatisfyIO` isJust
    it "holes/explicit reports found holes" $
      type'check hexpl `shouldReturnSubstring` "Found holes"

    let himpl = "./examples/negative/holes/implicit.glask"
    it "holes/implicit is rejected" $
      type'check himpl `shouldSatisfyIO` isJust
    it "holes/implicit reports found holes" $
      type'check himpl `shouldReturnSubstring` "Found holes"

  describe "ill-typed expressions fail evaluation" $
    mapM_ run'expr expr'cases

-- NOTE: there is deliberately no division-by-zero test. The evaluator is
-- lazy and the harness only forces results to weak head normal form, so a
-- `div 1 0` exception inside an unforced thunk is not observable here
-- (the query returns `Right <thunk>` instead of throwing).


-- | 'type'check' must return 'Just err' ('isJust' over 'IO').
shouldSatisfyIO :: IO (Maybe String) -> (Maybe String -> Bool) -> Expectation
shouldSatisfyIO action p = do
  r <- action
  r `shouldSatisfy` p


-- | 'type'check' must return 'Just err'; the message must contain the fragment.
shouldReturnSubstring :: IO (Maybe String) -> String -> Expectation
shouldReturnSubstring action fragment = do
  r <- action
  case r of
    Nothing -> expectationFailure "expected a type error, but type checking succeeded"
    Just msg -> msg `shouldContain` fragment


-- | Malformed sources. Each crashes parsing with an 'error' call, so
-- | 'evaluate (parse'expr src)' must throw.
parse'cases :: [String]
parse'cases =
  [ "(2 + 3"
  , "[1, 2"
  , "\\ -> 1"
  , "let x = 1 in"
  , "case x of"
  , "if True then 1"
  , "\\x -> "
  , "let { x = } in x"
  , "2 `plus"
  , "(,"
  , "5 ="
  , "@foo"
  , "(1 + 2))"
  , "((("
  , "2 `+++` 3"
  , "let { } in 1"
  , "_x @"
  , "case 1 of { -> 2 }"
  , "{"
  , "->"
  , "="
  , "if True then 1 else"
  , "\\x y ->"
  , "'a"
  , "{"
  , "}"
  , "let x = in x"
  , "case x of { -> 1 }"
  , "f :: "
  , "data"
  , "class"
  , "instance"
  , "(("
  , "))"
  , "((1)"
  , "[1,"
  , "[1 2"
  , "x = "
  , "= 1"
  , "let { x = 1 in x"
  , "\\ -> x"
  , "if True then"
  , "{ True -> }"
  , "`"
  ]

run'parse :: String -> Spec
run'parse src =
  it ("rejects " ++ show src) $
    (evaluate (parse'expr src)) `shouldThrow` anyErrorCall


-- | Ill-typed expressions: fixture, expression, and message fragments that
-- | the resulting 'Left' must contain.
type Expr'Case = (String, String, String, [String])

expr'cases :: [Expr'Case]
expr'cases =
  [ ("adding int and char", ev'arith, "2 + 'a'", ["didn't find any instances", "'Num'", "'Char'"])
  , ("adding bool and int", ev'arith, "True + 1", ["didn't find any instances", "'Num'", "'Bool'"])
  , ("equality of int and char", ev'arith, "5 == 'a'", ["didn't find any instances", "'Eq'", "'Char'"])
  , ("non-bool if condition", ev'arith, "if 1 then 2 else 3", ["didn't find any instances", "'Num'", "'Bool'"])
  , ("over-applying the identity", ev'arith, "(\\x -> x) 1 2", ["didn't find any instances", "'Num'"])
  , ("equality of tuples", ev'arith, "(1, 2) == (1, 2)", ["didn't find any instances", "'Eq'"])
  , ("division operator not in scope", ev'arith, "10 / 2", ["Data Not In Scope /"])
  , ("cons onto non-list", ev'lazy, "length (1 : 2)", ["didn't find any instances", "'Num'"])
  , ("take of non-list", ev'lazy, "length (take 1 2)", ["didn't find any instances", "'Num'"])
  , ("adding bool", ev'lazy, "1 + True", ["didn't find any instances", "'Num'", "'Bool'"])
  , ("case bool pattern on int", ev'data, "case 1 of { True -> 1 }", ["didn't find any instances", "'Num'", "'Bool'"])
  , ("uninitialised record field", ev'data, "getx (Point { px = 1 })", ["Unitialized Fields", "Point"])
  , ("unknown record field", ev'data, "origin { pz = 1 }", ["Not In Scope Field pz"])
  , ("projection of non-record", ev'data, "getx 5", ["didn't find any instances", "'Num'", "'Point'"])
  , ("mapping a non-function", ev'data, "length (map 5 [1,2])", ["didn't find any instances", "'Num'"])
  , ("case on non-bool", ev'nest, "choose 1", ["didn't find any instances", "'Num'", "'Bool'"])
  , ("unknown variable", ev'nest, "frobnicate 1", ["Unknown variable frobnicate"])
  , ("plus not in scope in matching", matching, "int + 1", ["Data Not In Scope +"])
  , ("times not in scope in matching", matching, "int * 2", ["Data Not In Scope *"])
  , ("times not in scope in arith", arith, "3 * 4", ["Data Not In Scope *"])
  , ("minus not in scope in arith", arith, "2 - 1", ["Data Not In Scope -"])
  , ("tuple length mismatch", matching, "((1, 2, 3) :: (Int, Bool))", ["[Shape]", "Couldn't match type"])
  , ("bool equality test", ev'nest, "if'nest True", ["didn't find any instances", "'Num'", "'Bool'"])
  , ("fibonacci of char", ev'nest, "fib 'a'", ["didn't find any instances", "'Num'", "'Char'"])
  , ("postfix applied to literal", ev'ops, "5 ???", ["didn't find any instances", "'Num'"])
  , ("backticked plain function", ev'ops, "2 `plus` 3", ["Data Not In Scope plus"])
  , ("parenthesised operator", ev'ops, "(+) 2 3", ["Missing Operand"])
  , ("mixing operator families", ev'ops, "1 +++ 2 *** 3", ["Mixing two operators"])
  , ("dangling infix operator", ev'ops, "1 + * 2", ["Missing Operand"])
  , ("trailing operator", ev'ops, "2 3 +", ["Missing Operand"])
  , ("method on char", matching, "case require 'a' of { _ -> 1 }", ["didn't find any instances", "'Num'", "'Char'"])
  , ("ambiguous method", matching, "vaval", ["cannot resolve ambiguity"])
  , ("bool for maybe", matching, "test' True", ["Couldn't match type", "`Bool`", "Maybe"])
  , ("bool for list", matching, "case tail True of { _ -> 1 }", ["Couldn't match type", "`Bool`"])
  , ("unsatisfiable show on char", nested, "f 1 'a'", ["didn't find any instances", "'Show'", "'Char'"])
  , ("char for int", nested, "apply inc 'a'", ["Couldn't match type", "`Char", "`Int`"])
  , ("show not in scope", arith, "show 1", ["Unknown variable show"])
  ]
  where
    ev'arith  = "./examples/positive/evaluate/ev-arith.glask"
    ev'lazy   = "./examples/positive/evaluate/ev-lazy.glask"
    ev'data   = "./examples/positive/evaluate/ev-data.glask"
    ev'nest   = "./examples/positive/evaluate/ev-nest.glask"
    ev'ops    = "./examples/positive/evaluate/ev-ops.glask"
    matching  = "./examples/positive/evaluate/matching.glask"
    nested    = "./examples/positive/prenex/nested.glask"
    arith     = "./examples/positive/evaluate/arith.glask"


run'expr :: Expr'Case -> Spec
run'expr (name, fixture, expr, fragments) =
  it (name ++ "  [" ++ expr ++ "]") $ do
    r <- eval'within expr fixture
    r `shouldSatisfy` isLeft
    case r of
      Left msg -> mapM_ (msg `shouldContain`) fragments
      Right _ -> expectationFailure "expected a failure, but evaluation succeeded"


type'check :: String -> IO (Maybe String)
type'check file'name = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle

  case load'declarations contents counter of
    Left sem'err -> return $ Just $ show sem'err

    Right (decls, trans'env, counter') -> do
      case process'declarations decls trans'env counter' of
        Left err -> return $ Just $ show err

        Right (program, infer'env, trans'env, counter'', infer'state) -> do
          return Nothing
  where counter   = Counter { counter = 0 }


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
