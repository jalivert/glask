module KindSpec where


import Test.Hspec

import System.IO
import Data.Either ( isLeft )

import Compiler.Counter

import REPL.Type ( read'type, infer'kind )
import REPL.Load ( load'declarations, process'declarations )
import Compiler.Syntax ( Kind )


spec :: Spec
spec = do
  describe "Kinds of types within fixtures" $ do
    mapM_ mk'case kind'cases
  describe "Unknown names have no kind" $ do
    mapM_ mk'err kind'errors
  where
    mk'case (name, file, type', expected) = do
      it name $ do
        r <- kind'within type' file
        fmap show r `shouldBe` Right expected
    mk'err (name, file, type') = do
      it name $ do
        r <- kind'within type' file
        r `shouldSatisfy` isLeft
        case r of
          Left msg -> msg `shouldContain` "not in the kind context"
          Right _ -> expectationFailure "expected a kind error, but it succeeded"


-- (test'name, fixture, type source, expected kind string via `show`).
-- Expected strings were obtained by running `:k <type>` queries through
-- the already-built REPL binary against the real fixture files.
kind'cases :: [(String, String, String, String)]
kind'cases =
  -- evaluate/matching.glask
  [ ("001 Int", matching, "Int", "*")
  , ("002 Bool", matching, "Bool", "*")
  , ("003 Char", matching, "Char", "*")
  , ("004 Double", matching, "Double", "*")
  , ("005 String", matching, "String", "*")
  , ("006 Maybe Int", matching, "Maybe Int", "*")
  , ("007 list of Int", matching, "[Int]", "*")
  , ("008 arrow", matching, "Int -> Bool", "*")
  , ("009 pair", matching, "(Int, Bool)", "*")
  , ("010 nested Maybe", matching, "Maybe (Maybe Int)", "*")
  , ("011 Wrap Int", matching, "Wrap Int", "*")
  , ("012 Phantom Int", matching, "Phantom Int", "*")
  , ("013 higher-kinded Test", matching, "Test Maybe Int", "*")
  , ("014 higher-kinded Record", matching, "Record Maybe Int", "*")
  , ("015 two arrows", matching, "Int -> Int -> Int", "*")
  , ("016 Maybe Bool", matching, "Maybe Bool", "*")
  , ("017 list of String", matching, "[String]", "*")
  , ("018 Char to Double", matching, "Char -> Double", "*")
  , ("019 Double to Int", matching, "Double -> Int", "*")
  , ("020 Test constructor", matching, "Test", "(* -> *) -> * -> *")
  , ("021 Record constructor", matching, "Record", "(* -> *) -> * -> *")
  , ("022 Phantom constructor", matching, "Phantom", "* -> *")
  , ("023 Maybe constructor", matching, "Maybe", "* -> *")
  , ("024 Wrap constructor", matching, "Wrap", "* -> *")
  -- evaluate/lazy.glask
  , ("025 List Int", lazy, "List Int", "*")
  , ("026 List Bool", lazy, "List Bool", "*")
  , ("027 List Char", lazy, "List Char", "*")
  , ("028 nested lists", lazy, "[[Int]]", "*")
  , ("029 arrow from list", lazy, "[Int] -> Int", "*")
  , ("030 arrow to list", lazy, "Int -> [Int]", "*")
  , ("031 parenthesised List", lazy, "(List Int)", "*")
  , ("032 List constructor", lazy, "List", "* -> *")
  , ("033 list constructor", lazy, "[]", "* -> *")
  -- higher-kinded/lists.glask
  , ("034 nested lists", lists, "[[Int]]", "*")
  , ("035 arrow from list", lists, "[Int] -> Int", "*")
  , ("036 arrow to list", lists, "Int -> [Int]", "*")
  , ("037 Maybe Int", lists, "Maybe Int", "*")
  , ("038 Maybe constructor", lists, "Maybe", "* -> *")
  , ("039 list constructor", lists, "[]", "* -> *")
  -- data/records.glask
  , ("040 Record Maybe Int", records, "Record Maybe Int", "*")
  , ("041 Maybe Int", records, "Maybe Int", "*")
  , ("042 list of Bool", records, "[Bool]", "*")
  , ("043 Bool to Bool", records, "Bool -> Bool", "*")
  , ("044 Record constructor", records, "Record", "(* -> *) -> * -> *")
  -- data/bool.glask
  , ("045 list of Bool", bool, "[Bool]", "*")
  , ("046 Bool to Bool", bool, "Bool -> Bool", "*")
  , ("047 Bool to Int", bool, "Bool -> Int", "*")
  , ("048 Char to Bool", bool, "Char -> Bool", "*")
  , ("055 Maybe Bool", matching, "Maybe Bool", "*")
  , ("056 list of Char", matching, "[Char]", "*")
  , ("057 Bool-Int pair", matching, "(Bool, Int)", "*")
  , ("058 two arrows", matching, "Int -> Int -> Bool", "*")
  , ("059 nested Maybe", matching, "Maybe (Maybe Bool)", "*")
  , ("060 nested lists", matching, "[[Int]]", "*")
  , ("061 Bool endomap", matching, "Bool -> Bool", "*")
  , ("062 Double endomap", matching, "Double -> Double", "*")
  , ("063 Char endomap", matching, "Char -> Char", "*")
  , ("064 list of Maybe", matching, "[Maybe Int]", "*")
  , ("065 Maybe of list", matching, "Maybe [Int]", "*")
  , ("066 Maybe-list pair", matching, "(Maybe Int, [Bool])", "*")
  , ("067 two-arg arrow", matching, "Int -> Bool -> Char", "*")
  , ("068 list arrow", matching, "[Int] -> Int", "*")
  , ("069 applied Record", records, "Record Maybe Int", "*")
  , ("070 partial Record", records, "Record Maybe", "* -> *")
  , ("071 Maybe Int in records", records, "Maybe Int", "*")
  , ("072 arrow to Record", records, "Int -> Record Maybe Int", "*")
  , ("073 Maybe list in lists", lists, "[Maybe Int]", "*")
  , ("074 pair with list", lists, "(Int, [Bool])", "*")
  , ("075 Phantom Bool", phantom, "Phantom Bool", "*")
  , ("076 Phantom Char", phantom, "Phantom Char", "*")
  , ("077 bare Phantom", phantom, "Phantom", "* -> *")
  , ("078 Wrap Bool in hk data", hk'data, "Wrap Bool", "*")
  , ("079 String endomap", matching, "String -> String", "*")
  , ("080 list of String", matching, "[String]", "*")
  , ("081 Char pair", matching, "(Char, Char)", "*")
  , ("082 Double to Bool", matching, "Double -> Bool", "*")
  , ("083 Wrap Bool", matching, "Wrap Bool", "*")
  , ("084 Wrap arrow", matching, "Wrap Int -> Int", "*")
  ]
  where
    matching = "./examples/positive/evaluate/matching.glask"
    lazy     = "./examples/positive/evaluate/lazy.glask"
    lists    = "./examples/positive/higher-kinded/lists.glask"
    records  = "./examples/positive/data/records.glask"
    phantom  = "./examples/positive/data/phantom.glask"
    hk'data  = "./examples/positive/higher-kinded/higherkinded.glask"
    bool     = "./examples/positive/data/bool.glask"


-- (test'name, fixture, type source): kind inference must fail.
kind'errors :: [(String, String, String)]
kind'errors =
  [ ("049 free type variable", matching, "a")
  , ("050 class name is not a type", matching, "Num")
  , ("051 tuple constructor has no entry", matching, "(,)")
  , ("052 List unknown in bool", bool, "List Int")
  , ("053 Maybe unknown in bool", bool, "Maybe Int")
  , ("054 String unknown in bool", bool, "String")
  ]
  where
    matching = "./examples/positive/evaluate/matching.glask"
    bool     = "./examples/positive/data/bool.glask"


-- Self-contained copy of the `:k` query pipeline from the REPL:
-- load the fixture, run the full program inference, then read
-- a type source and infer its kind in the resulting environment.
kind'within :: String -> String -> IO (Either String Kind)
kind'within type' file'name = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle

  case load'declarations contents counter of
    Left sem'err -> do
      return $ Left $ show sem'err

    Right (decls, trans'env, counter') -> do
      case process'declarations decls trans'env counter' of
        Left err -> do
          return $ Left $ show err

        Right (_, infer'env, trans'env', counter'', _) -> do
          case read'type type' trans'env' counter'' of
            Left trans'err -> do
              return $ Left $ show trans'err

            Right (type'', counter''') -> do
              case infer'kind type'' infer'env counter''' of
                Left err -> do
                  return $ Left $ show err

                Right (kind, _) -> do
                  return $ Right kind

  where counter   = Counter { counter = 0 }
