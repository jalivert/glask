module InferSpec where


import Test.Hspec

import System.IO
import Data.Either ( isLeft )

import qualified Data.Map.Strict as Map

import Compiler.Counter
import Compiler.Syntax.Type ( Sigma'Type )
import Compiler.TypeSystem.InferenceEnv ( kind'env )
import qualified Compiler.TypeSystem.InferenceEnv as I'Env
import qualified Compiler.TypeSystem.InferenceState as I'State
import qualified Compiler.Syntax.ToAST.TranslateEnv as TE

import REPL.Expression ( read'expr, infer'expr'type )
import REPL.Load ( load'declarations, process'declarations )


spec :: Spec
spec = do
  describe "Type inference (scheme'within over positive fixtures)" $ do
    mapM_ mk'case cases
  describe "Bare overloaded queries report ambiguity" $ do
    mapM_ mk'amb amb'cases
  where
    mk'case (name, file, expr, expected) = do
      it name $ do
        r <- scheme'within expr file
        fmap show r `shouldBe` Right expected
    -- A bare overloaded identifier is genuinely ambiguous: its variable
    -- occurs only under a non-defaultable class, and a query has no
    -- enclosing use to pin it down. GHC rejects the corresponding
    -- programs the same way; the REPL reports it instead of crashing.
    mk'amb (name, file, expr) = do
      it name $ do
        r <- scheme'within expr file
        r `shouldSatisfy` isLeft
        case r of
          Left msg -> msg `shouldContain` "cannot resolve ambiguity"
          Right _ -> expectationFailure "expected ambiguity, but inference succeeded"


-- (test'name, fixture, expression): must fail with an ambiguity error.
amb'cases :: [(String, String, String)]
amb'cases =
  [ ("019 class overloaded method", class', "vaval")
  , ("022 class method under lambda", class', "\\ x -> vaval")
  , ("023 qualified overloaded method", qualified, "vaval")
  , ("026 qualified method under constructor", qualified, "Wrap vaval")
  , ("084 hk class method", hk'class, "pure")
  , ("094 prenex nested constraint", nested, "f")
  ]
  where
    class'    = "./examples/positive/classes/class.glask"
    qualified = "./examples/positive/classes/qualified.glask"
    hk'class  = "./examples/positive/classes/higherkinded.glask"
    nested    = "./examples/positive/prenex/nested.glask"


-- (test'name, fixture, expression, expected scheme string via `show`)
-- Expected strings were obtained by running `:t <expr>` queries through the
-- already-built REPL binary (ghc-9.12.4 build) against the real fixture files.
cases :: [(String, String, String, String)]
cases =
  -- data/bool.glask
  [ ("001 bool literal True", bool, "True", "Bool")
  , ("002 bool literal False", bool, "False", "Bool")
  , ("003 bool char literal", bool, "'a'", "Char")
  , ("004 bool overloaded int literal", bool, "23", "(forall a . Num a => a)")
  , ("005 bool overloaded fractional literal", bool, "23.7", "(forall a . Fractional a => a)")
  , ("006 bool identity lambda", bool, "\\ x -> x", "(forall a . a -> a)")
  , ("007 bool const lambda", bool, "\\ x y -> x", "(forall a b . a -> b -> a)")
  , ("008 bool const-flip lambda", bool, "\\ x y -> y", "(forall a b . a -> b -> b)")
  , ("009 bool if overloaded branches", bool, "if True then 1 else 2", "(forall a . Num a => a)")
  , ("010 bool if bool branches", bool, "if True then True else False", "Bool")
  , ("011 bool applied identity at Bool", bool, "(\\ x -> x) True", "Bool")
  , ("012 bool if under lambda", bool, "\\ b -> if b then 1 else 0", "(forall a . Num a => Bool -> a)")
  -- annotations/forall.glask
  , ("013 forall Nothing", forall', "Nothing", "(forall a . Maybe a)")
  , ("014 forall Just constructor", forall', "Just", "(forall a . a -> Maybe a)")
  , ("015 forall Just applied to literal", forall', "Just 1", "(forall a . Num a => Maybe a)")
  , ("016 forall nested Maybe", forall', "Just Nothing", "(forall a . Maybe (Maybe a))")
  , ("017 forall annotated binding", forall', "works", "Maybe Int")
  , ("018 forall constructor under lambda", forall', "\\ x -> Just x", "(forall a . a -> Maybe a)")
  -- classes/class.glask
  , ("020 class method at Int", class', "vaval :: Int", "Int")
  , ("021 class literal at Int", class', "1 :: Int", "Int")
  -- classes/qualified.glask
  , ("024 qualified Wrap constructor", qualified, "Wrap", "(forall a . a -> Wrap a)")
  , ("025 qualified Wrap applied", qualified, "Wrap 1", "(forall a . Num a => Wrap a)")
  -- higher-kinded/lists.glask
  , ("027 lists nil", lists, "[]", "(forall a . [a])")
  , ("028 lists singleton", lists, "[1]", "(forall a . Num a => [a])")
  , ("029 lists two elements", lists, "[1, 2]", "(forall a . Num a => [a])")
  , ("030 lists cons operator", lists, "1 : []", "(forall a . Num a => [a])")
  , ("031 lists tuple of literals", lists, "(1, 2)", "(forall a b . (Num b, Num a) => (a, b))")
  , ("032 lists tail", lists, "tail", "(forall a . [a] -> Maybe ([a]))")
  , ("033 lists tail applied", lists, "tail [1]", "(forall a . Num a => Maybe ([a]))")
  , ("034 lists higher-order lambda", lists, "\\ f x y -> (f x, f y)", "(forall a b . (a -> b) -> a -> a -> (b, b))")
  -- higher-kinded/higherkinded.glask
  , ("035 hk Wrap constructor", hk'data, "Wrap", "(forall a . a -> Wrap a)")
  , ("036 hk Wrap applied", hk'data, "Wrap 1", "(forall a . Num a => Wrap a)")
  -- higher-rank/basic.glask
  , ("037 hr rank-2 apply", hr'basic, "apply", "(forall a b . (forall x . x -> x) -> a -> b -> (a, b))")
  , ("038 hr apply of identity", hr'basic, "apply (\\ x -> x)", "(forall a b . a -> b -> (a, b))")
  , ("039 hr apply at two types", hr'basic, "apply (\\ x -> x) 1 'a'", "(forall a . Num a => (a, Char))")
  -- higher-rank/data.glask
  , ("040 hr data binding", hr'data, "higher", "Higher Int")
  , ("041 hr unannotated data binding", hr'data, "higher'", "Higher Int")
  , ("042 hr annotated function", hr'data, "fn", "(forall a . a -> Int -> a)")
  , ("043 hr constructor applied", hr'data, "H fn", "Higher Int")
  -- higher-rank/case.glask
  , ("044 hr case match", hr'case, "higher'match", "(forall a b c . Higher a -> a -> b -> c -> (b, c))")
  , ("045 hr rank-2 constructor", hr'case, "H", "(forall a . (forall x . x -> a -> x) -> Higher a)")
  , ("046 hr matched function", hr'case, "fn", "(forall a . a -> Int -> a)")
  -- evaluate/lazy.glask
  , ("047 lazy overloaded addition", lazy, "2 + 3", "(forall a . Num a => a)")
  , ("048 lazy plus under lambda", lazy, "\\ x y -> x + y", "(forall a . Num a => a -> a -> a)")
  , ("049 lazy plus method", lazy, "(+)", "(forall a . Num a => a -> a -> a)")
  , ("050 lazy take", lazy, "take", "(forall a . Int -> [a] -> [a])")
  , ("051 lazy length", lazy, "length", "(forall a . [a] -> Int)")
  , ("052 lazy head", lazy, "head", "(forall a . [a] -> a)")
  , ("053 lazy infinite list", lazy, "endless'list", "[Int]")
  , ("054 lazy Cons applied", lazy, "Cons 1 Nil", "(forall a . Num a => List a)")
  , ("055 lazy take of infinite list", lazy, "take 2 endless'list", "[Int]")
  , ("056 lazy self-referential list", lazy, "inf", "List Int")
  -- evaluate/arith.glask
  , ("057 arith overloaded addition", arith, "2 + 3", "(forall a . Num a => a)")
  , ("058 arith plus method", arith, "(+)", "(forall a . Num a => a -> a -> a)")
  , ("059 arith doubling lambda", arith, "\\ x -> x + x", "(forall a . Num a => a -> a)")
  -- operators/infix.glask
  , ("060 infix monomorphic addition", infix', "2 + 3", "Int")
  , ("061 infix plus operator", infix', "(+)", "Int -> Int -> Int")
  , ("062 infix times operator", infix', "(*)", "Int -> Int -> Int")
  , ("063 infix program binding", infix', "program", "Int")
  , ("064 infix precedence", infix', "1 + 2 * 3", "Int")
  -- operators/prefix.glask
  , ("065 prefix program binding", prefix, "program", "Char")
  , ("066 prefix operator", prefix, "(|>)", "Int -> Bool")
  , ("067 prefix second operator", prefix, "(||>)", "Bool -> Char")
  , ("068 prefix operator applied", prefix, "(|>) 23", "Bool")
  -- operators/prefix-postfix.glask
  , ("069 prepost program binding", prepost, "program", "Char")
  , ("070 prepost prefix operator", prepost, "(|>)", "Int -> Bool")
  , ("071 prepost postfix operator", prepost, "(?)", "Bool -> Char")
  , ("072 prepost combined application", prepost, "(|>) 23 ?", "Char")
  -- operators/implicit.glask
  , ("073 implicit program binding", implicit, "program", "Int")
  , ("074 implicit int binding", implicit, "x", "Int")
  , ("075 implicit addition", implicit, "1 + 2", "Int")
  -- layout/basic.glask
  , ("076 layout let block", layout, "double'loc", "Int")
  , ("077 layout case function", layout, "choose", "(forall a . Num a => Bool -> a)")
  , ("078 layout nested lets", layout, "nested'one'line", "Int")
  , ("079 layout case in let", layout, "case'in'let", "Int")
  -- data/records.glask
  , ("080 records constructor", records, "Rec", "(forall a (b :: * -> *) . Int -> Maybe a -> b a -> Record b a)")
  , ("081 records partially applied", records, "Rec 1", "(forall a (b :: * -> *) . Maybe a -> b a -> Record b a)")
  -- data/phantom.glask
  , ("082 phantom constructor", phantom, "Ph", "(forall a . Phantom a)")
  , ("083 phantom under lambda", phantom, "\\ x -> Ph", "(forall a b . a -> Phantom b)")
  -- classes/higherkinded.glask
  , ("085 hk data constructor", hk'class, "Constr", "(forall a (b :: * -> *) . b a -> Test b a)")
  -- evaluate/matching.glask
  , ("086 matching int binding", matching, "int", "Int")
  , ("087 matching double binding", matching, "double", "Double")
  , ("088 matching const lambda", matching, "lambda", "(forall a b c . a -> b -> c -> c)")
  , ("089 matching constrained identity", matching, "require", "(forall a . Num a => a -> a)")
  , ("090 matching applied require", matching, "ok", "Int")
  , ("091 matching higher-kinded annotation", matching, "expl'forall", "(forall (a :: * -> *) b c . (b -> a c) -> b -> a c)")
  , ("092 matching Maybe binding", matching, "broken", "Maybe Int")
  , ("093 matching merged equations", matching, "many'eqs", "(forall a . a -> a -> a)")
  -- prenex/nested.glask
  , ("095 prenex constraint under arrow", nested, "g", "(forall a . Num a => Int -> a -> a)")
  , ("096 prenex rank-2 apply", nested, "apply", "(forall a . Num a => a -> a) -> Int -> Int")
  , ("097 prenex applied inc", nested, "apply inc 3", "Int")
  -- prenex/local.glask
  , ("098 prenex pure local use", local, "t'pure'poly", "Int")
  , ("099 prenex pair selector", local, "pick", "(Int, Bool) -> Int")
  -- prenex/branch.glask
  , ("100 prenex higher-rank branch", branch, "br1", "(forall a b . a -> Int -> b -> Int)")
  , ("101 bool if-else on literals", bool, "if False then 1 else 2", "(forall a . Num a => a)")
  , ("102 bool list of bools", bool, "[True, False]", "[Bool]")
  , ("103 bool tuple mixed", bool, "(True, 1)", "(forall a . Num a => (Bool, a))")
  , ("104 bool applied identity at Int", bool, "(\\x -> x) 5", "(forall a . Num a => a)")
  , ("105 bool const at Bool and Int", bool, "(\\x y -> x) True 7", "(forall a . Num a => Bool)")
  , ("106 bool if over chars", bool, "if True then 'a' else 'b'", "Char")
  , ("107 bool empty list", bool, "[]", "(forall a . [a])")
  , ("108 bool numeric list", bool, "[1, 2, 3]", "(forall a . Num a => [a])")
  , ("109 bool twice combinator", bool, "\\f x -> f (f x)", "(forall a . (a -> a) -> a -> a)")
  , ("110 bool singleton lambda", bool, "\\x -> [x]", "(forall a . a -> [a])")
  , ("112 bool nested numeric lists", bool, "[[1], [2]]", "(forall a . Num a => [[a]])")
  , ("113 bool if under binder", bool, "\\b -> if b then 0 else 1", "(forall a . Num a => Bool -> a)")
  , ("114 bool applied identity to identity", bool, "(\\x -> x) (\\y -> y)", "(forall a . a -> a)")
  , ("121 matching singleton list", matching, "[1]", "(forall a . Num a => [a])")
  , ("122 matching mixed tuple", matching, "(1, True)", "(forall a . Num a => (a, Bool))")
  , ("123 matching require under lambda", matching, "\\x -> require x", "(forall a . Num a => a -> a)")
  , ("124 matching int list", matching, "[int, 5]", "[Int]")
  , ("125 matching double-bool pair", matching, "(double, True)", "(Double, Bool)")
  , ("126 matching const lambda", matching, "\\x y -> x", "(forall a b . a -> b -> a)")
  , ("127 matching applied identity", matching, "(\\x -> x) int", "Int")
  , ("128 matching tail of list", matching, "tail [1]", "(forall a . Num a => Maybe ([a]))")
  , ("129 matching tail of empty", matching, "tail []", "(forall a . Maybe ([a]))")
  , ("130 infix mul-add", infix', "1 + 2 * 3", "Int")
  , ("131 infix add-mul", infix', "2 * 3 + 1", "Int")
  , ("133 infix chained addition", infix', "1 + 2 + 3", "Int")
  , ("134 infix program binding", infix', "program", "Int")
  , ("135 prefix program binding", prefix, "program", "Char")
  , ("136 prefix first operator", prefix, "(|>)", "Int -> Bool")
  , ("137 prefix second operator", prefix, "(||>)", "Bool -> Char")
  , ("138 prefix operator applied", prefix, "(|>) 5", "Bool")
  , ("139 prepost chained application", prepost, "(?) ((|>) 23)", "Char")
  , ("140 prepost program binding", prepost, "program", "Char")
  , ("141 prepost prefix operator", prepost, "(|>)", "Int -> Bool")
  , ("142 implicit addition", implicit, "x + 1", "Int")
  , ("143 implicit program binding", implicit, "program", "Int")
  , ("144 implicit int binding", implicit, "x", "Int")
  , ("145 implicit second binding", implicit, "y", "Int")
  , ("146 layout choose applied true", layout, "choose True", "(forall a . Num a => a)")
  , ("147 layout choose applied false", layout, "choose False", "(forall a . Num a => a)")
  , ("149 lists singleton lambda", lists, "\\x -> [x]", "(forall a . a -> [a])")
  , ("150 lists tail applied", lists, "tail [1, 2]", "(forall a . Num a => Maybe ([a]))")
  , ("151 lists cons on literals", lists, "1 : [2, 3]", "(forall a . Num a => [a])")
  , ("152 lists tail of empty", lists, "tail []", "(forall a . Maybe ([a]))")
  , ("154 forall double Just", forall', "Just (Just 1)", "(forall a . Num a => Maybe (Maybe a))")
  , ("155 forall mixed Maybe list", forall', "[Just 1, Nothing]", "(forall a . Num a => [Maybe a])")
  , ("156 lazy take three", lazy, "take 3 endless'list", "[Int]")
  , ("157 lazy head of infinite", lazy, "head endless'list", "Int")
  , ("158 lazy take zero", lazy, "take 0 endless'list", "[Int]")
  , ("159 lazy successor lambda", lazy, "\\x -> x + 1", "(forall a . Num a => a -> a)")
  , ("160 lazy Cons at Bool", lazy, "Cons True Nil", "List Bool")
  , ("161 lazy length of prefix", lazy, "length (take 2 endless'list)", "Int")
  , ("162 prenex pick false", local, "pick (1, False)", "Int")
  , ("165 prenex first arg wins", nested, "f 0 1", "(forall a b . (Num b, Num a, Show b) => a)")
  , ("166 prenex arrow const", nested, "g 2 3", "(forall a . Num a => a)")
  , ("167 prenex inc at Int", nested, "inc 10", "(forall a . Num a => a)")
  , ("168 prenex inline rank-2 double", nested, "apply (\\a -> a + a) 2", "Int")
  , ("171 prenex then binding", branch, "t'if'then", "Int")
  , ("172 prenex inline join applied", branch, "(if True then br1 else br2) 'a' 1 'b'", "Int")
  , ("175 records partial application", records, "Rec 1 Nothing", "(forall a (b :: * -> *) . b a -> Record b a)")
  , ("176 phantom const lambda", phantom, "\\x y -> Ph", "(forall a b c . a -> b -> Phantom c)")
  , ("177 triple tuple", bool, "(1, True, 'a')", "(forall a . Num a => (a, Bool, Char))")
  , ("178 numeric triple", bool, "(1, 2, 3)", "(forall a b c . (Num c, Num b, Num a) => (a, b, c))")
  , ("179 annotated triple", bool, "((1, True, 'a') :: (Int, Bool, Char))", "(Int, Bool, Char)")
  , ("180 applied plus operator", arith, "(+) 1 2", "(forall a . Num a => a)")
  ]
  where
    bool      = "./examples/positive/data/bool.glask"
    forall'   = "./examples/positive/annotations/forall.glask"
    class'    = "./examples/positive/classes/class.glask"
    qualified = "./examples/positive/classes/qualified.glask"
    lists     = "./examples/positive/higher-kinded/lists.glask"
    hk'data   = "./examples/positive/higher-kinded/higherkinded.glask"
    hr'basic  = "./examples/positive/higher-rank/basic.glask"
    hr'data   = "./examples/positive/higher-rank/data.glask"
    hr'case   = "./examples/positive/higher-rank/case.glask"
    lazy      = "./examples/positive/evaluate/lazy.glask"
    arith     = "./examples/positive/evaluate/arith.glask"
    infix'    = "./examples/positive/operators/infix.glask"
    prefix    = "./examples/positive/operators/prefix.glask"
    prepost   = "./examples/positive/operators/prefix-postfix.glask"
    implicit  = "./examples/positive/operators/implicit.glask"
    layout    = "./examples/positive/layout/basic.glask"
    records   = "./examples/positive/data/records.glask"
    phantom   = "./examples/positive/data/phantom.glask"
    hk'class  = "./examples/positive/classes/higherkinded.glask"
    matching  = "./examples/positive/evaluate/matching.glask"
    nested    = "./examples/positive/prenex/nested.glask"
    local     = "./examples/positive/prenex/local.glask"
    branch    = "./examples/positive/prenex/branch.glask"


-- Self-contained copy of the `scheme'within` helper pattern from ExamplesSpec:
-- load the fixture, run the full program inference, then infer the type of an
-- expression in the resulting environment and return the raw inferred scheme.
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
