-- | Golden regression suite for the Glask implementation.
--
-- Every test compares freshly computed implementation output against a
-- checked-in @test/golden/@ snapshot file:
--
-- * @parse@  - @show@ of @parse'module@ over each
--              @examples/positive/**\/*.glask@ file, headed by a declaration
--              count and a per-declaration constructor tag line (most
--              @Term'Decl@ constructors have only a stub 'Show' instance, so
--              the tag line is what distinguishes the files; the body below
--              it is the literal @show@ output).
-- * @eval@   - @show@ of the in-process evaluator result for key terms
--              (same pipeline as 'ExamplesSpec.eval'within').
-- * @repl@   - fixed query scripts run through 'run'script', which mirrors
--              the @glask-exe@ REPL loop ('REPL.Repl.repl') state-for-state
--              minus the @glask \@> @ \/ continuation prompts.  The snapshots
--              were captured from the real binary and stripped of prompts.
--
-- The harness itself only needs packages the test suite already depends on
-- (base, containers, directory, filepath, hspec, extra) plus the library.
--
-- Run with @BLESS=1@ to (re)write snapshot files instead of comparing.
-- Tests assume the working directory is the repository root.
module GoldenSpec where


import Test.Hspec

import System.Environment (lookupEnv)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import System.IO (openFile, IOMode (ReadMode), hGetContents)

import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Control.Monad.State (runState)
import Data.List.Extra (trim)

import qualified Data.Map.Strict as Map

import Compiler.Counter (Counter (..))
import Compiler.Parser (parse'module)
import Compiler.Syntax (Expression (..))
import Compiler.Syntax.Term (Term'Decl (..))
import Compiler.Syntax.Type (Type)
import Compiler.Syntax.ToAST.TranslateEnv (Translate'Env)
import qualified Compiler.Syntax.ToAST.TranslateEnv as TE
import Compiler.TypeSystem.InferenceEnv (Infer'Env (..))
import qualified Compiler.TypeSystem.InferenceEnv as I'Env
import qualified Compiler.TypeSystem.InferenceState as I'State
import Compiler.TypeSystem.InferenceState (Infer'State)
import Compiler.TypeSystem.Program (Program (..))
import qualified Interpreter.Core as Core
import Interpreter.Evaluate (eval, data'to'string)
import Interpreter.ToCore (section'to'core, data'to'core, to'core)
import REPL.Expression (read'expr, infer'type, infer'expr'type)
import REPL.Load (load'declarations, process'declarations, build'env'store)
import REPL.Type (read'type, infer'kind)

import ExamplesSpec (eval'within)


-- | Compare @compute@ output against a snapshot file.
-- With @BLESS=1@ in the environment the file is rewritten instead.
-- On mismatch the actual output is written to @<file>.actual@ and the
-- test fails showing a small line diff.
golden :: String -> IO String -> FilePath -> Spec
golden name compute expected'file = it name $ do
  actual <- compute
  _ <- evaluate (length actual)
  bless <- lookupEnv "BLESS"
  createDirectoryIfMissing True (takeDirectory expected'file)
  if bless == Just "1"
    then writeFile expected'file actual
    else do
      exists <- doesFileExist expected'file
      unless exists $
        expectationFailure
          ( "golden file does not exist: " ++ expected'file
          ++ "\nRun with BLESS=1 to create it.\nActual output was:\n" ++ actual
          )
      expected <- readFile expected'file
      _ <- evaluate (length expected)
      unless (expected == actual) $ do
        writeFile (expected'file ++ ".actual") actual
        expectationFailure
          ( "golden mismatch: " ++ expected'file
          ++ "\n" ++ line'diff expected actual
          ++ "\nWrote actual output to: " ++ expected'file ++ ".actual"
          )


-- | First 20 differing lines of two texts, with line numbers.
line'diff :: String -> String -> String
line'diff expected actual =
  "expected lines: " ++ show (length (lines expected))
  ++ ", actual lines: " ++ show (length (lines actual))
  ++ "\n" ++ unlines (take 20 diffs)
  where
    diffs =
      [ "line " ++ show n ++ "\n  expected: " ++ e ++ "\n  actual:   " ++ a
      | (n, (e, a)) <- zip [1 :: Int ..] (zip'long (lines expected) (lines actual))
      , e /= a
      ]
    zip'long [] [] = []
    zip'long (x : xs) [] = (x, "") : zip'long xs []
    zip'long [] (y : ys) = ("", y) : zip'long [] ys
    zip'long (x : xs) (y : ys) = (x, y) : zip'long xs ys


-- | Constructor tag of a parsed declaration.  Most 'Term'Decl'
-- constructors have only a stub 'Show' instance, so this tag line is the
-- part of a parse dump that distinguishes files structurally.
decl'tag :: Term'Decl -> String
decl'tag (Binding _ _) = "Binding"
decl'tag (Signature _ _) = "Signature"
decl'tag (Data'Decl _ _ _) = "Data'Decl"
decl'tag (Type'Alias _ _ _) = "Type'Alias"
decl'tag (Fixity _ _ _ _) = "Fixity"
decl'tag (Class'Decl _ _ _ _) = "Class'Decl"
decl'tag (Instance _ _) = "Instance"


-- | Dump @show@ of @parse'module@ over a source file.
parse'dump :: FilePath -> IO String
parse'dump file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  let decls = parse'module contents
  _ <- evaluate (length decls)
  _ <- evaluate (length (show decls))
  return $ unlines
    ( ("-- parse dump: " ++ file)
    : ("-- decls: " ++ show (length decls) ++ " " ++ show (map decl'tag decls))
    : map show decls
    )


-- | Dump @show@ of the evaluated value of a term within a file's context.
eval'dump :: FilePath -> String -> IO String
eval'dump file expr = do
  result <- eval'within expr file
  let out = unlines ["-- eval: " ++ expr, "-- in: " ++ file, show result]
  _ <- evaluate (length out)
  return out


-- | REPL session state, threaded exactly as 'REPL.Repl.repl' threads it.
type Session = (Program, Infer'Env, Translate'Env, Counter, Infer'State Type)


-- | Load a file the way 'REPL.Load.load' does, returning the initial REPL
-- session instead of entering the interactive loop.
load'session :: FilePath -> IO (Either String Session)
load'session file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  _ <- evaluate (length contents)
  case load'declarations contents (Counter 0) of
    Left sem'err -> return (Left (show sem'err))
    Right (decls, trans'env, counter') ->
      case process'declarations decls trans'env counter' of
        Left err -> return (Left (show err))
        Right (program, infer'env, trans'env', counter'', infer'state) -> do
          let k'e = kind'env infer'env
              b'sec'core = section'to'core (bind'section program)
              data'decls = data'declarations program
              unit'constr = Core.Binding "()" (Core.Intro "()" [])
              cons'constr = Core.Binding ":" (Core.Abs "a" (Core.Abs "as" (Core.Intro ":" [Core.Var "a", Core.Var "as"])))
              nil'constr = Core.Binding "[]" (Core.Intro "[]" [])
              constructor'core = unit'constr : cons'constr : nil'constr : data'to'core data'decls
              (env, stor) = build'env'store (b'sec'core ++ constructor'core)
          return (Right
            ( program { b'sec'core = b'sec'core ++ constructor'core, environment = env, store = stor }
            , infer'env
            , trans'env' { TE.kind'context = k'e `Map.union` TE.kind'context trans'env' }
            , counter''
            , infer'state
            ))


-- | Run fixed single-line queries through an in-process mirror of the
-- @glask-exe@ REPL loop ('REPL.Repl.repl').
--
-- The result is the concatenation of the per-query response chunks with
-- the @"glask \@> "@ and 9-space continuation prompts stripped, i.e. the
-- same bytes as real binary output with banner and prompts removed.
-- Queries must be syntactically valid single lines: a parse error inside
-- @read'expr@\/@read'type@ raises an uncaught 'error' in the real REPL
-- (and here), exactly as observed against the binary.
run'script :: FilePath -> [String] -> IO String
run'script file queries = do
  e'session <- load'session file
  case e'session of
    Left err -> return ("load failed: " ++ err ++ "\n")
    Right session -> do
      let transcript = go session queries
      _ <- evaluate (length transcript)
      return transcript
  where
    go :: Session -> [String] -> String
    go _ [] = ""
    go session@(program@Program { environment = environment, store = store }, i'env, trans'env, counter, infer'state) (query : rest) =
      case query of
        [] -> "\n" ++ go session rest
        ":exit" -> "Bye!\n"
        ':' : 'l' : 'o' : 'a' : 'd' : ' ' : _ -> "<loading arbitrary files not implemented yet>"
        ':' : 'q' : _ -> "Bye!\n"
        ':' : 'Q' : _ -> "Bye!\n"
        ':' : 't' : line ->
          case read'expr line trans'env counter of
            Left _ ->
              "Incorrect Format! The :t command must be followed by an expression.\n"
              ++ go session rest
            Right (expression, counter') ->
              case infer'type expression i'env counter' of
                Left err ->
                  "Type Error: " ++ show err ++ "\n" ++ go session rest
                Right (scheme, _, counter'') ->
                  ("          " ++ trim line ++ " :: " ++ show scheme ++ "\n")
                  ++ go (program, i'env, trans'env, counter'', infer'state) rest
        ':' : 'p' : line ->
          case read'expr line trans'env counter of
            Left _ ->
              "Incorrect Format! The :p command must be followed by an expression.\n"
              ++ go session rest
            Right (expression, counter') ->
              ("Parsed: " ++ show expression ++ "\n")
              ++ go (program, i'env, trans'env, counter', infer'state) rest
        ':' : 'k' : line ->
          case read'type line trans'env counter of
            Left err -> "Error: " ++ show err ++ "\n"
            Right (type', counter') ->
              case infer'kind type' i'env counter' of
                Left err ->
                  "Kind Error: " ++ show err ++ "\n"
                  ++ go (program, i'env, trans'env, counter', infer'state) rest
                Right (kind, counter'') ->
                  ("          " ++ trim line ++ " :: " ++ show kind ++ "\n")
                  ++ go (program, i'env, trans'env, counter'', infer'state) rest
        ':' : 'c' : line ->
          case read'expr line trans'env counter of
            Left trans'err -> "Error: " ++ show trans'err ++ "\n"
            Right (expr, counter''') ->
              let inf'env = i'env { I'Env.instances = I'State.instances infer'state, I'Env.overloaded = I'State.overloaded infer'state }
              in case infer'expr'type expr inf'env counter''' of
                Left err ->
                  "Type Error: " ++ show err ++ "\n"
                  ++ go (program, i'env, trans'env, counter''', infer'state) rest
                Right (_, expr', counter'''') ->
                  ("Core:\n" ++ show (to'core expr') ++ "\n")
                  ++ go (program { store = store }, i'env, trans'env, counter'''', infer'state) rest
        ':' : 'd' : line ->
          case read'expr line trans'env counter of
            Left trans'err -> "Error: " ++ show trans'err ++ "\n"
            Right (expr, counter''') ->
              let inf'env = i'env { I'Env.instances = I'State.instances infer'state, I'Env.overloaded = I'State.overloaded infer'state }
              in case infer'expr'type expr inf'env counter''' of
                Left err ->
                  "Type Error: " ++ show err ++ "\n"
                  ++ go (program, i'env, trans'env, counter''', infer'state) rest
                Right (_, expr', counter'''') ->
                  ("Desugared:\n" ++ show expr' ++ "\n")
                  ++ go (program { store = store }, i'env, trans'env, counter'''', infer'state) rest
        ':' : 'f' : digits ->
          ("          " ++ show (store Map.! (read digits :: Int)) ++ "\n")
          ++ go session rest
        line ->
          case read'expr ("show (" ++ line ++ "\n)") trans'env counter of
            Left trans'err -> "Error: " ++ show trans'err ++ "\n"
            Right (expr, counter''') ->
              let inf'env = i'env { I'Env.instances = I'State.instances infer'state, I'Env.overloaded = I'State.overloaded infer'state }
              in case infer'expr'type expr inf'env counter''' of
                Left err ->
                  "Type Error: " ++ show err ++ "\n"
                  ++ go (program, inf'env, trans'env, counter''', infer'state) rest
                Right (_, expr', counter'''') ->
                  let core'expr = to'core expr'
                      (res, store') = runState (eval core'expr environment) store
                  in case res of
                    Left eval'err -> "Evaluation Error: " ++ show eval'err ++ "\n"
                    Right value ->
                      case runState (data'to'string value environment) store' of
                        (Left err, _) -> show err ++ "\n"
                        (Right serialized, store'') ->
                          ("          " ++ serialized ++ "\n")
                          ++ go (program { store = store'' }, inf'env, trans'env, counter'''', infer'state) rest


-- | (test name, example file, golden file) for parse dumps: one per
-- @examples/positive/**\/*.glask@ file.
parse'cases :: [(String, FilePath, FilePath)]
parse'cases =
  [ ("annotations-forall", ex "annotations/forall.glask", pg "annotations-forall")
  , ("classes-class", ex "classes/class.glask", pg "classes-class")
  , ("classes-higherkinded", ex "classes/higherkinded.glask", pg "classes-higherkinded")
  , ("classes-qualified", ex "classes/qualified.glask", pg "classes-qualified")
  , ("classes-super", ex "classes/super.glask", pg "classes-super")
  , ("data-bool", ex "data/bool.glask", pg "data-bool")
  , ("data-phantom", ex "data/phantom.glask", pg "data-phantom")
  , ("data-records", ex "data/records.glask", pg "data-records")
  , ("evaluate-arith", ex "evaluate/arith.glask", pg "evaluate-arith")
  , ("evaluate-lazy", ex "evaluate/lazy.glask", pg "evaluate-lazy")
  , ("evaluate-matching", ex "evaluate/matching.glask", pg "evaluate-matching")
  , ("higherkinded-higherkinded", ex "higher-kinded/higherkinded.glask", pg "higherkinded-higherkinded")
  , ("higherkinded-lists", ex "higher-kinded/lists.glask", pg "higherkinded-lists")
  , ("higherrank-basic", ex "higher-rank/basic.glask", pg "higherrank-basic")
  , ("higherrank-case", ex "higher-rank/case.glask", pg "higherrank-case")
  , ("higherrank-data", ex "higher-rank/data.glask", pg "higherrank-data")
  , ("layout-basic", ex "layout/basic.glask", pg "layout-basic")
  , ("operators-implicit", ex "operators/implicit.glask", pg "operators-implicit")
  , ("operators-infix", ex "operators/infix.glask", pg "operators-infix")
  , ("operators-prefix", ex "operators/prefix.glask", pg "operators-prefix")
  , ("operators-prefix-postfix", ex "operators/prefix-postfix.glask", pg "operators-prefix-postfix")
  , ("prenex-branch", ex "prenex/branch.glask", pg "prenex-branch")
  , ("prenex-local", ex "prenex/local.glask", pg "prenex-local")
  , ("prenex-nested", ex "prenex/nested.glask", pg "prenex-nested")
  ]
  where
    ex rel = "./examples/positive/" ++ rel
    pg slug = "./test/golden/parse/" ++ slug ++ ".golden"


-- | (test name, example file, query expression, golden file) for eval dumps.
eval'cases :: [(String, FilePath, String, FilePath)]
eval'cases =
  [ ("arith-plus", ex "evaluate/arith.glask", "2 + 3", eg "arith-plus")
  , ("lazy-take-endless", ex "evaluate/lazy.glask", "length (take 4 endless'list)", eg "lazy-take-endless")
  , ("lazy-length", ex "evaluate/lazy.glask", "length normal'list", eg "lazy-length")
  , ("lazy-head", ex "evaluate/lazy.glask", "head normal'list", eg "lazy-head")
  , ("lazy-take-two", ex "evaluate/lazy.glask", "program1", eg "lazy-take-two")
  , ("matching-int", ex "evaluate/matching.glask", "int", eg "matching-int")
  , ("matching-ok", ex "evaluate/matching.glask", "ok", eg "matching-ok")
  , ("matching-testing", ex "evaluate/matching.glask", "testing", eg "matching-testing")
  , ("matching-broken", ex "evaluate/matching.glask", "broken", eg "matching-broken")
  , ("matching-the-what", ex "evaluate/matching.glask", "the'what 0", eg "matching-the-what")
  , ("matching-test-let", ex "evaluate/matching.glask", "test''", eg "matching-test-let")
  , ("matching-unwrapped", ex "evaluate/matching.glask", "unwrapped", eg "matching-unwrapped")
  , ("forall-works", ex "annotations/forall.glask", "works", eg "forall-works")
  , ("lists-tail-cons", ex "higher-kinded/lists.glask", "tail [1,2]", eg "lists-tail-cons")
  , ("lists-tail-nil", ex "higher-kinded/lists.glask", "tail []", eg "lists-tail-nil")
  , ("infix-program", ex "operators/infix.glask", "program", eg "infix-program")
  , ("prefix-program", ex "operators/prefix.glask", "program", eg "prefix-program")
  , ("prefix-postfix-program", ex "operators/prefix-postfix.glask", "program", eg "prefix-postfix-program")
  , ("bool-true", ex "data/bool.glask", "True", eg "bool-true")
  , ("nested-f", ex "prenex/nested.glask", "f 1 2", eg "nested-f")
  , ("nested-g", ex "prenex/nested.glask", "g 0 5", eg "nested-g")
  , ("nested-apply", ex "prenex/nested.glask", "apply inc 3", eg "nested-apply")
  , ("nested-ann", ex "prenex/nested.glask", "t'nested'ann", eg "nested-ann")
  , ("nested-lambda-method", ex "prenex/nested.glask", "t'lambda'method", eg "nested-lambda-method")
  , ("nested-local-let", ex "prenex/nested.glask", "t'local'let", eg "nested-local-let")
  , ("nested-local-rank", ex "prenex/nested.glask", "t'local'rank", eg "nested-local-rank")
  , ("nested-double-ann", ex "prenex/nested.glask", "t'double'ann", eg "nested-double-ann")
  , ("branch-if-then", ex "prenex/branch.glask", "t'if'then", eg "branch-if-then")
  , ("branch-if-else", ex "prenex/branch.glask", "t'if'else", eg "branch-if-else")
  , ("layout-double", ex "layout/basic.glask", "double'loc", eg "layout-double")
  , ("layout-choose-true", ex "layout/basic.glask", "choose True", eg "layout-choose-true")
  , ("layout-choose-false", ex "layout/basic.glask", "choose False", eg "layout-choose-false")
  , ("layout-nested-let", ex "layout/basic.glask", "nested'one'line", eg "layout-nested-let")
  , ("layout-case-in-let", ex "layout/basic.glask", "case'in'let", eg "layout-case-in-let")
  , ("local-pure-poly", ex "prenex/local.glask", "t'pure'poly", eg "local-pure-poly")
  , ("local-over-mono", ex "prenex/local.glask", "t'over'mono", eg "local-over-mono")
  , ("local-over-poly", ex "prenex/local.glask", "t'over'poly", eg "local-over-poly")
  ]
  where
    ex rel = "./examples/positive/" ++ rel
    eg slug = "./test/golden/eval/" ++ slug ++ ".golden"


-- | (test name, file to load, single-line queries, golden file) for REPL
-- transcripts.  Snapshots hold the binary's response chunks with prompts
-- stripped; 'run'script' reproduces them in process.
repl'cases :: [(String, FilePath, [String], FilePath)]
repl'cases =
  [ ( "arith-introspect"
    , ex "evaluate/arith.glask"
    , [":t 2 + 3", ":p 2 + 3", ":k Int", ":c 2 + 3", ":d 2 + 3", ":q"]
    , rg "arith-introspect"
    )
  , ( "nested-eval"
    , ex "prenex/nested.glask"
    , ["f 1 2", "apply inc 3", "g 0 5", "t'double'ann", ":q"]
    , rg "nested-eval"
    )
  , ( "nested-introspect"
    , ex "prenex/nested.glask"
    , [":t inc", ":p apply inc 3", ":k Int", ":q"]
    , rg "nested-introspect"
    )
  , ( "branch-introspect"
    , ex "prenex/branch.glask"
    , [":t br1", ":t t'if'then", ":p if True then 1 else 2", ":q"]
    , rg "branch-introspect"
    )
  , ( "arith-errors"
    , ex "evaluate/arith.glask"
    , [":t unknown'var", "2 + 3", ":t True", ":q"]
    , rg "arith-errors"
    )
  , ( "lazy-introspect"
    , ex "evaluate/lazy.glask"
    , [":t length", ":t take", ":p take 2 [1]", ":k Bool", ":q"]
    , rg "lazy-introspect"
    )
  ]
  where
    ex rel = "./examples/positive/" ++ rel
    rg slug = "./test/golden/repl/" ++ slug ++ ".golden"


spec :: Spec
spec = do
  describe "parse dumps (show of parse'module per positive example)" $
    forM_ parse'cases $ \(name, file, golden'file) ->
      golden ("parse: " ++ name) (parse'dump file) golden'file
  describe "eval dumps (key terms per example)" $
    forM_ eval'cases $ \(name, file, expr, golden'file) ->
      golden ("eval: " ++ name) (eval'dump file expr) golden'file
  describe "REPL transcripts (fixed query scripts)" $
    forM_ repl'cases $ \(name, file, queries, golden'file) ->
      golden ("repl: " ++ name) (run'script file queries) golden'file

