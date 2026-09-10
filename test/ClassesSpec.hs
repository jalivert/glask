module ClassesSpec where


import Test.Hspec

import System.IO

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe ( isNothing )
import Data.Either ( isLeft, isRight )

import Control.Monad ( forM_ )
import Control.Monad.Extra ( allM )
import Control.Monad.State ( runState )


import Compiler.Syntax.Term
import Compiler.Syntax.Literal
import Compiler.Syntax.Type ( Sigma'Type, Type, T'V' )

import Compiler.Parser.Parser ( parse'module, parse'decls, parse'expr, parse'type )

import Compiler.TypeSystem.Error ( Error )

import Compiler.TypeSystem.InferenceEnv ( init'k'env, class'env )
import Compiler.TypeSystem.InferenceEnv as I'Env
import qualified Compiler.TypeSystem.InferenceState as I'State
import Compiler.TypeSystem.InferenceState ( Infer'State )

import qualified Interpreter.Core as Core
import qualified Compiler.Syntax.ToAST.TranslateEnv as TE

import Compiler.Syntax.ToAST.TranslateEnv

import Control.Monad.State ( runState )

import Interpreter.Evaluate ( eval, data'to'string )
import Interpreter.ToCore ( decls'to'core, section'to'core, data'to'core, to'core )


import Compiler.Counter

import REPL.Expression ( read'expr, infer'type, infer'expr'type )
import REPL.Load ( load'declarations, process'declarations, build'env'store )
import Interpreter.Value (Value (Literal))
import Compiler.Syntax (Expression(..), Literal (Lit'Int))



import Compiler.TypeSystem.Solver.Substitutable ( Term(free'vars) )

import Compiler.TypeSystem.Program ( Program(Program, b'sec'core, environment, store) )
import Compiler.TypeSystem.Program ( Program( Program, data'declarations, bind'section, methods, method'annotations, b'sec'core, environment, store) )


-- Fixture paths (specs run from the repo root).
file'local, file'nested, file'branch :: String
file'local  = "./examples/positive/prenex/local.glask"
file'nested = "./examples/positive/prenex/nested.glask"
file'branch = "./examples/positive/prenex/branch.glask"

file'class, file'super, file'qualified, file'higherkinded, file'tcsuper :: String
file'class       = "./examples/positive/classes/class.glask"
file'super       = "./examples/positive/classes/super.glask"
file'qualified   = "./examples/positive/classes/qualified.glask"
file'higherkinded = "./examples/positive/classes/higherkinded.glask"
file'tcsuper     = "./examples/positive/classes/tc-super.glask"

file'matching, file'arith :: String
file'matching = "./examples/positive/evaluate/matching.glask"
file'arith    = "./examples/positive/evaluate/arith.glask"


-- 10 positive typecheck cases (expect Nothing).
tc'positive :: [String]
tc'positive =
  [ file'class
  , file'super
  , file'qualified
  , file'higherkinded
  , file'matching
  , file'local
  , file'nested
  , file'branch
  , file'arith
  , file'tcsuper
  , "./examples/positive/operators/infix.glask"
  , "./examples/positive/operators/prefix.glask"
  , "./examples/positive/operators/prefix-postfix.glask"
  , "./examples/positive/operators/implicit.glask"
  , "./examples/positive/layout/basic.glask"
  , "./examples/positive/data/bool.glask"
  , "./examples/positive/data/records.glask"
  , "./examples/positive/data/phantom.glask"
  , "./examples/positive/evaluate/lazy.glask"
  , "./examples/positive/higher-rank/basic.glask"
  , "./examples/positive/higher-rank/case.glask"
  , "./examples/positive/higher-rank/data.glask"
  , "./examples/positive/higher-kinded/lists.glask"
  , "./examples/positive/higher-kinded/higherkinded.glask"
  , "./examples/positive/annotations/forall.glask"
  , "./examples/positive/evaluate/ev-arith.glask"
  , "./examples/positive/evaluate/ev-data.glask"
  , "./examples/positive/evaluate/ev-lazy.glask"
  , "./examples/positive/evaluate/ev-nest.glask"
  , "./examples/positive/evaluate/ev-ops.glask"
  ]

-- 2 negative typecheck cases (expect failure).
tc'negative :: [String]
tc'negative =
  [ "./examples/negative/classes/one.glask"
  , "./examples/negative/classes/super.glask"
  ]

-- 1 negative case that does not even parse (expect a throw).
tc'unparseable :: [String]
tc'unparseable =
  [ "./examples/negative/classes/two.glask"
  ]

-- 70 eval cases with exact Int results: (label, fixture, expr, expected).
-- Values of data type (Wrap/Bool/Maybe constructors) are deliberately
-- avoided: they elaborate to store promises whose numbers are unstable.
-- Every Bool-typed dictionary use is observed through `pick :: (Int, Bool) -> Int`.
eval'int :: [(String, String, String, Int)]
eval'int =
  -- local.glask: Ident Int maps everything to 0, Ident Bool to False (19)
  [ ("pure local at two types", file'local, "t'pure'poly", 1)
  , ("overloaded local at one type", file'local, "t'over'mono", 6)
  , ("overloaded local at two types gets per-use dictionaries", file'local, "t'over'poly", 99)
  , ("Int dict selected under pick with True", file'local, "pick (ident 5, True)", 0)
  , ("Int dict selected under pick with False", file'local, "pick (ident 5, False)", 99)
  , ("Bool dict maps True to False so pick takes else", file'local, "pick (5, ident True)", 99)
  , ("pick passes through", file'local, "pick (7, True)", 7)
  , ("both dictionaries at once", file'local, "pick (ident 3, ident True)", 99)
  , ("Num Int dictionary for (+)", file'local, "1 + 2", 3)
  , ("pick else-branch constant", file'local, "pick (9, False)", 99)
  , ("pick then-branch constant", file'local, "pick (0, True)", 0)
  , ("two constrained locals, per-use dictionaries, True", file'local, "let { f :: Ident a => a -> a ; f = \\x -> ident x } in let { g :: Num a => a -> a ; g = \\x -> x + x } in pick (f (g 3), True)", 0)
  , ("two constrained locals, per-use dictionaries, False", file'local, "let { f :: Ident a => a -> a ; f = \\x -> ident x } in let { g :: Num a => a -> a ; g = \\x -> x + x } in pick (f (g 3), False)", 99)
  , ("one overloaded local used twice", file'local, "let { d :: Num a => a -> a ; d = \\x -> x + x } in d (d 1)", 4)
  , ("one overloaded local used once", file'local, "let { d :: Num a => a -> a ; d = \\x -> x + x } in d 5", 10)
  , ("Int dict under pick", file'local, "pick (ident 9, True)", 0)
  , ("pick identity on then-branch", file'local, "pick (1, True)", 1)
  , ("pick identity on then-branch 2", file'local, "pick (2, True)", 2)
  , ("Int dict under pick with 1", file'local, "pick (ident 1, True)", 0)
  -- nested.glask: prenex dictionaries across arrows and ranks (19)
  , ("nested qualified result", file'nested, "f 1 2", 1)
  , ("constraint under arrow", file'nested, "g 0 5", 5)
  , ("constrained higher-rank argument", file'nested, "apply inc 3", 6)
  , ("nested annotation argument", file'nested, "t'nested'ann", 6)
  , ("lambda with method use", file'nested, "t'lambda'method", 6)
  , ("local qualified binding", file'nested, "t'local'let", 6)
  , ("local higher-rank use", file'nested, "t'local'rank", 6)
  , ("doubly-nested annotation", file'nested, "t'double'ann", 6)
  , ("increment of five", file'nested, "inc 5", 10)
  , ("first arg wins", file'nested, "f 7 8", 7)
  , ("arrow result wins", file'nested, "g 9 4", 4)
  , ("inline lambda as rank-2 argument", file'nested, "apply (\\ a -> a + a) 5", 10)
  , ("increment of zero", file'nested, "inc 0", 0)
  , ("computed first arg", file'nested, "f (1 + 2) 9", 3)
  , ("computed second arg", file'nested, "g 1 (inc 2)", 4)
  , ("fresh local double", file'nested, "let { h :: Num a => a -> a ; h = \\x -> x + x } in h 7", 14)
  , ("increment twice", file'nested, "inc (inc 1)", 4)
  , ("zero through", file'nested, "f 0 0", 0)
  , ("increment of two", file'nested, "inc 2", 4)
  -- branch.glask: higher-rank branches join (9)
  , ("then-branch selected", file'branch, "t'if'then", 23)
  , ("else-branch selected", file'branch, "t'if'else", 23)
  , ("first branch applied", file'branch, "br1 'a' 23 'z'", 23)
  , ("second branch applied", file'branch, "br2 'q' 41 'w'", 41)
  , ("first branch at Bool", file'branch, "br1 True 5 'z'", 5)
  , ("second branch at Bool", file'branch, "br2 False 6 'w'", 6)
  , ("inline then-join applied", file'branch, "(if True then br1 else br2) 'a' 23 'z'", 23)
  , ("inline else-join applied", file'branch, "(if False then br1 else br2) 'a' 23 'z'", 23)
  , ("first branch zeros", file'branch, "br1 'x' 0 'y'", 0)
  -- tc-super.glask: superclass dictionaries discharge at Int (6)
  , ("bottom through middle and super", file'tcsuper, "apply'bottom 5", 5)
  , ("bottom binding", file'tcsuper, "t'bottom", 5)
  , ("middle identity", file'tcsuper, "apply'middle 7", 7)
  , ("middle binding", file'tcsuper, "t'middle'use", 7)
  , ("chained dictionaries", file'tcsuper, "apply'both 4", 4)
  , ("chained binding", file'tcsuper, "t'both", 4)
  -- class.glask: nullary instance selection (1)
  , ("Constant Int instance", file'class, "(vaval :: Int)", 23)
  -- matching.glask: defaulting and nullary instances (11)
  , ("ambiguous literal defaults to Int", file'matching, "int", 23)
  , ("constrained application defaults", file'matching, "ok", 23)
  , ("plain testing binding", file'matching, "testing", 23)
  , ("expression defaults to Int", file'matching, "require 5", 5)
  , ("projection of constant lambda", file'matching, "the'what 0", 3)
  , ("third projection", file'matching, "fn 1 2 3", 3)
  , ("Constant Int through qualified file", file'matching, "(vaval :: Int)", 23)
  , ("require of zero", file'matching, "require 0", 0)
  , ("nested require", file'matching, "require (require 4)", 4)
  , ("require of 23", file'matching, "require 23", 23)
  , ("require of 7", file'matching, "require 7", 7)
  -- arith.glask: Num Int dictionary for (+) (5)
  , ("two plus three", file'arith, "2 + 3", 5)
  , ("zero plus zero", file'arith, "0 + 0", 0)
  , ("ten plus twenty", file'arith, "10 + 20", 30)
  , ("three plus four", file'arith, "3 + 4", 7)
  , ("one plus one", file'arith, "1 + 1", 2)
  , ("middle dictionary applies", file'tcsuper, "apply'middle 0", 0)
  , ("chained dictionaries apply", file'tcsuper, "apply'both 10", 10)
  , ("projection of applied lambda", file'matching, "the'what 9", 3)
  , ("require of ten", file'matching, "require 10", 10)
  , ("increment of ten", file'nested, "inc 10", 20)
  , ("first branch at chars", file'branch, "br1 'x' 1 'y'", 1)
  , ("pick then-branch constant", file'local, "pick (3, True)", 3)
  , ("pick else-branch constant", file'local, "pick (5, False)", 99)
  ]

-- 1 eval case with an exact Double result.
eval'double :: [(String, String, String, Double)]
eval'double =
  [ ("fractional literal", file'matching, "double", 23.7)
  , ("int literal under Double annotation becomes Double", file'matching, "expl", 23.0)
  ]

-- 8 scheme cases that must succeed (Right): annotations and concrete
-- goals resolve, including through the qualified `Constant (Wrap a)`
-- instance in matching.
scheme'right :: [(String, String, String)]
scheme'right =
  [ ("annotated Constant Int", file'class, "(vaval :: Int)")
  , ("annotated Constant Int in matching", file'matching, "(vaval :: Int)")
  , ("qualified instance fires in matching", file'matching, "(vaval :: Wrap Int)")
  , ("qualified instance fires", file'qualified, "(vaval :: Wrap Int)")
  , ("defaulted numeric literal", file'matching, "23")
  , ("nested selection", file'nested, "f 1 2")
  , ("poly local use", file'local, "t'over'poly")
  , ("applied require", file'matching, "require 5")
  , ("tail over literal", file'matching, "tail [1]")
  , ("const lambda", file'matching, "lambda")
  , ("increment applied", file'nested, "inc 5")
  , ("branch applied", file'branch, "br1 'a' 1 'b'")
  , ("local pick applied", file'local, "pick (1, True)")
  , ("arith overloaded addition", file'arith, "2 + 3")
  , ("double annotation", file'matching, "double :: Double")
  , ("Wrap constructor", file'qualified, "Wrap")
  , ("applied projection", file'matching, "the'what 9")
  , ("Maybe binding", file'matching, "broken")
  ]

-- 7 scheme cases that must fail (Left): bare overloaded identifiers
-- are genuinely ambiguous outside the defaultable standard classes.
scheme'left :: [(String, String, String)]
scheme'left =
  [ ("bare vaval is ambiguous", file'class, "vaval")
  , ("bare higher-kinded method is ambiguous", file'higherkinded, "pure")
  , ("ambiguous higher-kinded variable", file'higherkinded, "pure 5")
  , ("Ident outside defaultable classes", file'local, "ident 5")
  , ("Ident zero outside defaultable classes", file'local, "ident 0")
  , ("Bottom outside defaultable classes", file'super, "bottom'm 5")
  , ("Middle outside defaultable classes", file'super, "middle'm 7")
  , ("unsatisfiable qualified method", file'matching, "many'eqs True 1")
  , ("bare Ident is ambiguous", file'local, "ident")
  , ("bare Middle method is ambiguous", file'super, "middle'm")
  , ("bare Super method is ambiguous", file'super, "super'm")
  , ("bare Center method is ambiguous", file'super, "center'm")
  , ("bare Bottom method is ambiguous", file'super, "bottom'm")
  , ("bare qualified method is ambiguous", file'qualified, "vaval")
  ]


spec :: Spec
spec = do
  describe "class fixtures typecheck" $ do
    forM_ tc'positive $ \file ->
      it ("typechecks: " ++ file) $ do
        r <- type'check file
        r `shouldBe` Nothing

  describe "ill-kinded and unsatisfiable class programs fail" $ do
    forM_ tc'negative $ \file ->
      it ("fails to typecheck: " ++ file) $ do
        r <- type'check file
        r `shouldSatisfy` (not . isNothing)

    forM_ tc'unparseable $ \file ->
      it ("does not parse: " ++ file) $ do
        type'check file `shouldThrow` anyErrorCall

  describe "instance selection and per-use dictionaries evaluate" $ do
    forM_ eval'int $ \(label, file, expr, expected) ->
      it (label ++ ": " ++ expr ++ " == " ++ show expected) $ do
        r <- eval'within expr file
        r `shouldBe` Right (Literal (Lit'Int expected))

  describe "fractional results evaluate" $ do
    forM_ eval'double $ \(label, file, expr, expected) ->
      it (label ++ ": " ++ expr) $ do
        r <- eval'within expr file
        r `shouldBe` Right (Literal (Lit'Double expected))

  describe "schemes resolve or report ambiguity" $ do
    forM_ scheme'right $ \(label, file, expr) ->
      it ("resolves: " ++ label ++ ": " ++ expr) $ do
        r <- scheme'within expr file
        r `shouldSatisfy` isRight

    forM_ scheme'left $ \(label, file, expr) ->
      it ("rejected: " ++ label ++ ": " ++ expr) $ do
        r <- scheme'within expr file
        r `shouldSatisfy` isLeft

    it "branch join leaks no rigid skolems" $ do
      r <- scheme'within "(if True then br1 else br2)" file'branch
      case r of
        Left err -> expectationFailure err
        Right scheme -> (free'vars scheme :: Set.Set T'V') `shouldBe` Set.empty


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


scheme'within :: String -> String -> IO (Either String Sigma'Type)
scheme'within expr file'name = do
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
          case read'expr expr trans'env'{ TE.kind'context = k'e `Map.union` (TE.kind'context trans'env') } counter'' of
            Left trans'err -> do
              return $ Left $ show trans'err

            Right (expr', counter''') -> do
              let inf'env = infer'env{ I'Env.instances = I'State.instances infer'state, I'Env.overloaded = I'State.overloaded infer'state }
              case infer'expr'type expr' inf'env counter''' of
                Left err -> do
                  return $ Left $ show err

                Right (scheme, _, _) -> do
                  return $ Right scheme

  where counter   = Counter { counter = 0 }


serialise'within :: String -> String -> IO (Either String String)
serialise'within expr file'name = do
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

          case read'expr expr trans'env'{ TE.kind'context = k'e `Map.union` (TE.kind'context trans'env') } counter'' of
            Left trans'err -> do
              return $ Left $ show trans'err

            Right (expr', counter''') -> do
              -- the REPL shows every expression by applying `show` to it
              let shown = App (Var "show") expr'
              let inf'env = infer'env{ I'Env.instances = I'State.instances infer'state, I'Env.overloaded = I'State.overloaded infer'state }
              case infer'expr'type shown inf'env counter''' of
                Left err -> do
                  return $ Left $ show err

                Right (_, expr'', _) -> do
                  let core = to'core expr''
                      (res, store') = runState (eval core env) stor
                  case res of
                    Left eval'err -> do
                      return $ Left $ show eval'err

                    Right value -> do
                      case runState (data'to'string value env) store' of
                        (Left err, _) -> do
                          return $ Left $ show err

                        (Right serialised, _) -> do
                          return $ Right serialised

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

        Right (program, infer'env, trans'env, counter'', infer'state) -> do
          let k'e = kind'env infer'env

          let b'section = bind'section program
          let b'sec'core = section'to'core b'section

          let data'decls = data'declarations program


          let unit'constr = Core.Binding "()" (Core.Intro "()" [])
          let cons'constr = Core.Binding ":" (Core.Abs "a" (Core.Abs "as" (Core.Intro ":" [Core.Var "a", Core.Var "as"])))
          let nil'constr  = Core.Binding "[]" (Core.Intro "[]" [])
          let constructor'core = unit'constr : cons'constr : nil'constr : data'to'core data'decls

          let (env, stor) = build'env'store $ b'sec'core ++ constructor'core

          repl'expr expr (program{ b'sec'core = b'sec'core ++ constructor'core, environment = env, store = stor }, infer'env, trans'env{ TE.kind'context = k'e `Map.union` (TE.kind'context trans'env)}, counter'', infer'state)


repl'expr :: String -> (Program, Infer'Env, Translate'Env, Counter, Infer'State Type) -> IO (Either String Value)
repl'expr expr (program@Program{ environment = environment, store = store }, i'env@Infer'Env{ kind'env = k'env, type'env = type'env, class'env =  class'env }, trans'env, counter, infer'state) = do
  case read'expr expr trans'env counter of
    Left trans'err -> do
      return $ Left $ show trans'err

    Right (expr, counter''') -> do
      let inf'env = i'env{ I'Env.instances = I'State.instances infer'state, I'Env.overloaded = I'State.overloaded infer'state }

      let error'or'scheme = infer'expr'type expr inf'env counter'''

      case error'or'scheme of
        Left err -> do
          return $ Left $ show err

        Right (_, expr', counter'''') -> do
          let core'expr = to'core expr'
          let (res, store') = runState (eval core'expr environment) store
          case res of
            Left eval'err ->
              return $ Left $ show eval'err

            Right value ->
              return $ Right value
