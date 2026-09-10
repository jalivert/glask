module ParserSpec where


import Test.Hspec


import Compiler.Syntax.Term


import Compiler.Syntax.Literal


import Compiler.Syntax.Fixity ( Fixity(..) )
import qualified Compiler.Syntax.Associativity as Assoc


import Compiler.Parser.Parser (parse'module, parse'decls, parse'expr, parse'type)


infix 5 ~:, ~.., ~::, `shouldParseEq`


-- |  Equality check for types without a 'Show' instance
-- |  ('shouldBe' needs 'Show' for failure messages).
shouldParseEq :: Eq a => a -> a -> Expectation
shouldParseEq actual expected = (actual == expected) `shouldBe` True

(~:) :: String -> Term'Expr -> Bool
source ~: term = result == term
  where result = parse'expr source


(~::) :: String -> Term'Type -> Bool
source ~:: type' = result == type'
  where result = parse'type source


(~..) :: String -> [Term'Decl] -> Bool
source ~.. decl = result == decl
  where result = parse'decls source


spec :: Spec
spec = do
  describe "Test parsing of type annotations" $ do

    it "parses a simple type signature" $ do
      "foo :: Int" ~.. [Signature "foo" ([], Term'T'Id $ Term'Id'Const "Int")]

    it "parses a simple type signature" $ do
      "foo :: a" ~.. [Signature "foo" ([], Term'T'Id $ Term'Id'Var "a")]

    it "parses a type signature with qualified type" $ do
      "foo :: Bar a => Int" ~.. [Signature "foo" ([Is'In "Bar" $ Term'T'Id $ Term'Id'Var "a"], Term'T'Id $ Term'Id'Const "Int")]

    it "parses a type signature with qualified type" $ do
      "foo :: (Bar a) => Int" ~.. [Signature "foo" ([Is'In "Bar" $ Term'T'Id $ Term'Id'Var "a"], Term'T'Id $ Term'Id'Const "Int")]

    it "parses a type signature with qualified type - bigger context" $ do
      "foo :: (Bar a, Baz b) => Int" ~.. 
        [ Signature"foo" ([ Is'In "Bar" $ Term'T'Id $ Term'Id'Var "a"
                          , Is'In "Baz" $ Term'T'Id $ Term'Id'Var "b"], Term'T'Id $ Term'Id'Const "Int")]

    it "parses a type signature with qualified type with empty context" $ do
      "foo :: () => Int" ~.. [Signature "foo" ([], Term'T'Id $ Term'Id'Const "Int")]


  describe "Test parsing of a simple variable declarations" $ do

    it "parses a simple variable binding" $ do
      "foo = 23" ~.. [Binding (Term'P'Id (Term'Id'Var "foo")) (Term'E'Lit (Lit'Int 23))]

    it "parses a simple variable binding" $ do
      "bar = []" ~.. [Binding (Term'P'Id (Term'Id'Var "bar")) (Term'E'List [])]


  describe "Test parsing of a simple class declarations" $ do

    it "parses a small class declaration" $ do
      "class Foo a" ~.. [Class'Decl "Foo" "a" [] []]


  describe "Test parsing type expressions" $ do

    it "parses a type variable" $ do
      "t'var" ~:: Term'T'Id (Term'Id'Var "t'var")

    it "parses a type constant" $ do
      "Int" ~:: Term'T'Id (Term'Id'Const "Int")

    it "parses a type application `Maybe a`" $ do
      "Maybe a" ~:: Term'T'App [Term'T'Id (Term'Id'Const "Maybe"), Term'T'Id (Term'Id'Var "a")]

    it "parses a type application `Maybe Int`" $ do
      "Maybe Int" ~:: Term'T'App [Term'T'Id (Term'Id'Const "Maybe"), Term'T'Id (Term'Id'Const "Int")]

    it "parses a unit type `()`" $ do
      "()" ~:: Term'T'Unit

    it "parses a small tuple `(Int, String, Maybe Int)`" $ do
      "(Int, String, Maybe Int)" ~:: Term'T'Tuple [ Term'T'Id (Term'Id'Const "Int")
                                                  , Term'T'Id (Term'Id'Const "String")
                                                  , Term'T'App  [ Term'T'Id (Term'Id'Const "Maybe")
                                                                , Term'T'Id (Term'Id'Const "Int") ]]

    it "parses a simple function type `Int -> Int`" $ do
      "Int -> Int" ~:: Term'T'Arrow [ Term'T'Id (Term'Id'Const "Int")
                                    , Term'T'Id (Term'Id'Const "Int")]

    it "parses a function type `Int -> a -> Maybe a`" $ do
      "Int -> a -> Maybe a" ~:: Term'T'Arrow  [ Term'T'Id (Term'Id'Const "Int")
                                              , Term'T'Arrow  [ Term'T'Id (Term'Id'Var "a")
                                                              , Term'T'App  [ Term'T'Id (Term'Id'Const "Maybe")
                                                                            , Term'T'Id (Term'Id'Var "a") ] ] ]

    it "parses a function type `(b -> a) -> Either a b -> Result a`" $ do
      "(b -> a) -> Either a b -> Result a" ~::
        Term'T'Arrow  [ Term'T'Arrow [Term'T'Id (Term'Id'Var "b"), Term'T'Id (Term'Id'Var "a")]
                      , Term'T'Arrow  [ Term'T'App  [ Term'T'Id (Term'Id'Const "Either")
                                                    , Term'T'Id (Term'Id'Var "a")
                                                    , Term'T'Id (Term'Id'Var "b") ]
                                      , Term'T'App  [ Term'T'Id (Term'Id'Const "Result")
                                                    , Term'T'Id (Term'Id'Var "a") ] ] ]


  describe "Test parsing expressions" $ do

    it "parses '3 + 5'" $ do
      "3 + 5" ~: Term'E'App [Term'E'Lit $ Lit'Int 3, Term'E'Op $ Term'Id'Var "+", Term'E'Lit $ Lit'Int 5 ]


  describe "Test implicit layout" $ do

    it "parses an implicit let block like explicit braces" $ do
      parse'expr "let a = 0\n    b = 1\nin a" == parse'expr "let { a = 0 ; b = 1 } in a"

    it "parses an implicit case block like explicit braces" $ do
      parse'expr "case b of\n  True -> 1\n  False -> 0"
        == parse'expr "case b of { True -> 1 ; False -> 0 }"

    it "parses an implicit where block like explicit braces" $ do
      parse'decls "class Num a where\n  (+) :: a -> a"
        == parse'decls "class Num a where { (+) :: a -> a }"

    it "parses an implicit three-binding let like explicit braces" $ do
      parse'expr "let a = 1\n    b = 2\n    c = 3\nin c"
        == parse'expr "let { a = 1 ; b = 2 ; c = 3 } in c"

    it "parses an implicit three-branch case like explicit braces" $ do
      parse'expr "case b of\n  True -> 1\n  False -> 0\n  _ -> 2"
        == parse'expr "case b of { True -> 1 ; False -> 0 ; _ -> 2 }"

    it "parses an implicit two-method class like explicit braces" $ do
      parse'decls "class Num a where\n  (+) :: a -> a\n  (*) :: a -> a"
        == parse'decls "class Num a where { (+) :: a -> a ; (*) :: a -> a }"

    it "parses an implicit two-binding instance like explicit braces" $ do
      parse'decls "instance Num Int where\n  (+) x y = x\n  (*) x y = y"
        == parse'decls "instance Num Int where { (+) x y = x ; (*) x y = y }"

    it "parses an implicit two-binding module like explicit braces" $ do
      parse'module "module Main where\nx = 1\ny = 2"
        == parse'module "module Main where { x = 1 ; y = 2 }"

    it "parses mixed implicit outer and explicit inner lets" $ do
      parse'expr "let a = let { b = 2 } in b\nin a"
        == parse'expr "let { a = let { b = 2 } in b } in a"

    it "parses an implicit case in a let body like explicit braces" $ do
      parse'expr "let a = 1 in case a of\n  _ -> a"
        == parse'expr "let { a = 1 } in case a of { _ -> a }"

    it "parses an implicit signature and binding in let like explicit braces" $ do
      parse'expr "let f :: Int\n    f = 1\nin f"
        == parse'expr "let { f :: Int ; f = 1 } in f"

    it "parses an implicit single-method class like explicit braces" $ do
      parse'decls "class C a where\n  f :: a"
        == parse'decls "class C a where { f :: a }"

    it "parses an implicit single-binding instance like explicit braces" $ do
      parse'decls "instance Eq Bool where\n  (==) x y = x"
        == parse'decls "instance Eq Bool where { (==) x y = x }"


  describe "Bulk data-driven parser coverage" $ do
    let cases :: [(String, IO ())]
        cases =
          [ ("expr var", parse'expr "x" `shouldBe` Term'E'Id (Term'Id'Var "x"))
          , ("expr var with digits", parse'expr "foo2" `shouldBe` Term'E'Id (Term'Id'Var "foo2"))
          , ("expr con True", parse'expr "True" `shouldBe` Term'E'Id (Term'Id'Const "True"))
          , ("expr con Nothing", parse'expr "Nothing" `shouldBe` Term'E'Id (Term'Id'Const "Nothing"))
          , ("expr int literal", parse'expr "23" `shouldBe` Term'E'Lit (Lit'Int 23))
          , ("expr double literal", parse'expr "3.14" `shouldBe` Term'E'Lit (Lit'Double 3.14))
          , ("expr char literal", parse'expr "'a'" `shouldBe` Term'E'Lit (Lit'Char 'a'))
          , ("expr string desugars to char list", parse'expr "\"hi\"" `shouldBe` Term'E'List [Term'E'Lit (Lit'Char 'h'), Term'E'Lit (Lit'Char 'i')])
          , ("expr application", parse'expr "f x" `shouldBe` Term'E'App [Term'E'Id (Term'Id'Var "f"), Term'E'Id (Term'Id'Var "x")])
          , ("expr multi application", parse'expr "f x y" `shouldBe` Term'E'App [Term'E'Id (Term'Id'Var "f"), Term'E'Id (Term'Id'Var "x"), Term'E'Id (Term'Id'Var "y")])
          , ("expr flat infix app", parse'expr "3 + 5" `shouldBe` Term'E'App [Term'E'Lit (Lit'Int 3), Term'E'Op (Term'Id'Var "+"), Term'E'Lit (Lit'Int 5)])
          , ("expr backtick var op", parse'expr "f `add` x" `shouldBe` Term'E'App [Term'E'Id (Term'Id'Var "f"), Term'E'Op (Term'Id'Var "add"), Term'E'Id (Term'Id'Var "x")])
          , ("expr backtick con op", parse'expr "xs `Cons` ys" `shouldBe` Term'E'App [Term'E'Id (Term'Id'Var "xs"), Term'E'Op (Term'Id'Const "Cons"), Term'E'Id (Term'Id'Var "ys")])
          , ("expr parenthesised var op", parse'expr "(+)" `shouldBe` Term'E'Op (Term'Id'Var "+"))
          , ("expr parenthesised con op", parse'expr "(:)" `shouldBe` Term'E'Op (Term'Id'Const ":"))
          , ("expr parenthesised dot op", parse'expr "(.)" `shouldBe` Term'E'Op (Term'Id'Var "."))
          , ("expr unit id", parse'expr "()" `shouldBe` Term'E'Id (Term'Id'Const "()"))
          , ("expr tuple constr id", parse'expr "(,)" `shouldBe` Term'E'Id (Term'Id'Const "(,)"))
          , ("expr paren is transparent", parse'expr "(x)" `shouldBe` Term'E'Id (Term'Id'Var "x"))
          , ("expr pair tuple", parse'expr "(1, 2)" `shouldBe` Term'E'Tuple [Term'E'Lit (Lit'Int 1), Term'E'Lit (Lit'Int 2)])
          , ("expr triple tuple", parse'expr "(1, 2, 3)" `shouldBe` Term'E'Tuple [Term'E'Lit (Lit'Int 1), Term'E'Lit (Lit'Int 2), Term'E'Lit (Lit'Int 3)])
          , ("expr empty list", parse'expr "[]" `shouldBe` Term'E'List [])
          , ("expr singleton list", parse'expr "[1]" `shouldBe` Term'E'List [Term'E'Lit (Lit'Int 1)])
          , ("expr two list", parse'expr "[1, 2]" `shouldBe` Term'E'List [Term'E'Lit (Lit'Int 1), Term'E'Lit (Lit'Int 2)])
          , ("expr arith seq no step", parse'expr "[a .. b]" `shouldBe` Term'E'Arith'Seq (Term'E'Id (Term'Id'Var "a")) Nothing (Term'E'Id (Term'Id'Var "b")))
          , ("expr arith seq with step", parse'expr "[1, 2 .. 10]" `shouldBe` Term'E'Arith'Seq (Term'E'Lit (Lit'Int 1)) (Just (Term'E'Lit (Lit'Int 2))) (Term'E'Lit (Lit'Int 10)))
          , ("expr if", parse'expr "if True then 1 else 0" `shouldBe` Term'E'If (Term'E'Id (Term'Id'Const "True")) (Term'E'Lit (Lit'Int 1)) (Term'E'Lit (Lit'Int 0)))
          , ("expr lambda single var", parse'expr "\\x -> x" `shouldBe` Term'E'Abst (Term'P'Id (Term'Id'Var "x")) (Term'E'Id (Term'Id'Var "x")))
          , ("expr lambda multi var single pattern app", parse'expr "\\x y -> x" `shouldBe` Term'E'Abst (Term'P'App [Term'P'Id (Term'Id'Var "x"), Term'P'Id (Term'Id'Var "y")]) (Term'E'Id (Term'Id'Var "x")))
          , ("expr lambda constr pattern", parse'expr "\\(Just x) -> x" `shouldBe` Term'E'Abst (Term'P'App [Term'P'Id (Term'Id'Const "Just"), Term'P'Id (Term'Id'Var "x")]) (Term'E'Id (Term'Id'Var "x")))
          , ("expr let explicit braces", parse'expr "let { x = 1 } in x" `shouldBe` Term'E'Let [Binding (Term'P'Id (Term'Id'Var "x")) (Term'E'Lit (Lit'Int 1))] (Term'E'Id (Term'Id'Var "x")))
          , ("expr let two bindings explicit", parse'expr "let { x = 1 ; y = 2 } in x" `shouldBe` Term'E'Let [Binding (Term'P'Id (Term'Id'Var "x")) (Term'E'Lit (Lit'Int 1)), Binding (Term'P'Id (Term'Id'Var "y")) (Term'E'Lit (Lit'Int 2))] (Term'E'Id (Term'Id'Var "x")))
          , ("expr annotation unparenthesised", parse'expr "x :: Int" `shouldBe` Term'E'Ann (Term'E'Id (Term'Id'Var "x")) ([], Term'T'Id (Term'Id'Const "Int")))
          , ("expr annotation arrow", parse'expr "(x :: a -> a)" `shouldBe` Term'E'Ann (Term'E'Id (Term'Id'Var "x")) ([], Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "a")]))
          , ("expr case two alts", parse'expr "case x of { True -> 1 ; False -> 0 }" `shouldBe` Term'E'Case (Term'E'Id (Term'Id'Var "x")) [(Term'P'Id (Term'Id'Const "True"), Term'E'Lit (Lit'Int 1)), (Term'P'Id (Term'Id'Const "False"), Term'E'Lit (Lit'Int 0))])
          , ("expr case infix constr pattern", parse'expr "case xs of { (y : ys) -> y }" `shouldBe` Term'E'Case (Term'E'Id (Term'Id'Var "xs")) [(Term'P'App [Term'P'Id (Term'Id'Var "y"), Term'P'Op (Term'Id'Const ":"), Term'P'Id (Term'Id'Var "ys")], Term'E'Id (Term'Id'Var "y"))])
          , ("expr labeled constr", parse'expr "Point { x = 1 }" `shouldBe` Term'E'Labeled'Constr "Point" [("x", Term'E'Lit (Lit'Int 1))])
          , ("expr labeled update on var", parse'expr "p { x = 1 }" `shouldBe` Term'E'Labeled'Update (Term'E'Id (Term'Id'Var "p")) [("x", Term'E'Lit (Lit'Int 1))])
          , ("expr labeled constr uppercase", parse'expr "C { x = 1 }" `shouldBe` Term'E'Labeled'Constr "C" [("x", Term'E'Lit (Lit'Int 1))])
          , ("expr named hole", parse'expr "_hole" `shouldBe` Term'E'Hole "_hole")
          , ("expr hole as argument", parse'expr "f _x" `shouldBe` Term'E'App [Term'E'Id (Term'Id'Var "f"), Term'E'Hole "_x"])
          , ("expr dot application", parse'expr "f . g" `shouldBe` Term'E'App [Term'E'Id (Term'Id'Var "f"), Term'E'Op (Term'Id'Var "."), Term'E'Id (Term'Id'Var "g")])
          , ("expr nested application", parse'expr "f (g x)" `shouldBe` Term'E'App [Term'E'Id (Term'Id'Var "f"), Term'E'App [Term'E'Id (Term'Id'Var "g"), Term'E'Id (Term'Id'Var "x")]])
          , ("expr mixed flat app", parse'expr "f x + g y" `shouldBe` Term'E'App [Term'E'Id (Term'Id'Var "f"), Term'E'Id (Term'Id'Var "x"), Term'E'Op (Term'Id'Var "+"), Term'E'Id (Term'Id'Var "g"), Term'E'Id (Term'Id'Var "y")])
          , ("expr implicit let layout", parse'expr "let x = 1\nin x" `shouldBe` Term'E'Let [Binding (Term'P'Id (Term'Id'Var "x")) (Term'E'Lit (Lit'Int 1))] (Term'E'Id (Term'Id'Var "x")))
          , ("expr cons application", parse'expr "'c' : cs" `shouldBe` Term'E'App [Term'E'Lit (Lit'Char 'c'), Term'E'Op (Term'Id'Const ":"), Term'E'Id (Term'Id'Var "cs")])
          , ("expr prefix application", parse'expr "not True" `shouldBe` Term'E'App [Term'E'Id (Term'Id'Var "not"), Term'E'Id (Term'Id'Const "True")])
          , ("expr tuple of applications", parse'expr "(f x, g y)" `shouldBe` Term'E'Tuple [Term'E'App [Term'E'Id (Term'Id'Var "f"), Term'E'Id (Term'Id'Var "x")], Term'E'App [Term'E'Id (Term'Id'Var "g"), Term'E'Id (Term'Id'Var "y")]])
          , ("expr list of applications", parse'expr "[f x, g]" `shouldBe` Term'E'List [Term'E'App [Term'E'Id (Term'Id'Var "f"), Term'E'Id (Term'Id'Var "x")], Term'E'Id (Term'Id'Var "g")])
          , ("expr nested lists", parse'expr "[[1]]" `shouldBe` Term'E'List [Term'E'List [Term'E'Lit (Lit'Int 1)]])
          , ("expr if over vars", parse'expr "if x then y else z" `shouldBe` Term'E'If (Term'E'Id (Term'Id'Var "x")) (Term'E'Id (Term'Id'Var "y")) (Term'E'Id (Term'Id'Var "z")))
          , ("expr case wildcard", parse'expr "case b of { _ -> 1 }" `shouldBe` Term'E'Case (Term'E'Id (Term'Id'Var "b")) [(Term'P'Wild, Term'E'Lit (Lit'Int 1))])
          , ("decl int binding", parse'decls "num = 42" `shouldBe` [Binding (Term'P'Id (Term'Id'Var "num")) (Term'E'Lit (Lit'Int 42))])
          , ("decl function binding", parse'decls "f x = x" `shouldBe` [Binding (Term'P'App [Term'P'Id (Term'Id'Var "f"), Term'P'Id (Term'Id'Var "x")]) (Term'E'Id (Term'Id'Var "x"))])
          , ("decl constr pattern binding", parse'decls "f (Just x) = x" `shouldBe` [Binding (Term'P'App [Term'P'Id (Term'Id'Var "f"), Term'P'App [Term'P'Id (Term'Id'Const "Just"), Term'P'Id (Term'Id'Var "x")]]) (Term'E'Id (Term'Id'Var "x"))])
          , ("decl operator binding", parse'decls "(+) x y = x" `shouldBe` [Binding (Term'P'App [Term'P'Id (Term'Id'Var "+"), Term'P'Id (Term'Id'Var "x"), Term'P'Id (Term'Id'Var "y")]) (Term'E'Id (Term'Id'Var "x"))])
          , ("decl tuple pattern binding", parse'decls "(x, y) = p" `shouldBe` [Binding (Term'P'Tuple [Term'P'Id (Term'Id'Var "x"), Term'P'Id (Term'Id'Var "y")]) (Term'E'Id (Term'Id'Var "p"))])
          , ("decl list pattern binding", parse'decls "[x, y] = l" `shouldBe` [Binding (Term'P'List [Term'P'Id (Term'Id'Var "x"), Term'P'Id (Term'Id'Var "y")]) (Term'E'Id (Term'Id'Var "l"))])
          , ("decl as pattern binding", parse'decls "xs @ (y : ys) = xs" `shouldBe` [Binding (Term'P'As "xs" (Term'P'App [Term'P'Id (Term'Id'Var "y"), Term'P'Op (Term'Id'Const ":"), Term'P'Id (Term'Id'Var "ys")])) (Term'E'Id (Term'Id'Var "xs"))])
          , ("decl wildcard binding", parse'decls "_ = 1" `shouldBe` [Binding Term'P'Wild (Term'E'Lit (Lit'Int 1))])
          , ("decl pattern annotation binding", parse'decls "(x :: Int) = 1" `shouldBe` [Binding (Term'P'Ann (Term'P'Id (Term'Id'Var "x")) ([], Term'T'Id (Term'Id'Const "Int"))) (Term'E'Lit (Lit'Int 1))])
          , ("decl string pattern binding", parse'decls "\"a\" = x" `shouldBe` [Binding (Term'P'List [Term'P'Lit (Lit'Char 'a')]) (Term'E'Id (Term'Id'Var "x"))])
          , ("decl char pattern binding", parse'decls "'c' = x" `shouldBe` [Binding (Term'P'Lit (Lit'Char 'c')) (Term'E'Id (Term'Id'Var "x"))])
          , ("decl simple signature", parse'decls "f :: Int" `shouldBe` [Signature "f" ([], Term'T'Id (Term'Id'Const "Int"))])
          , ("decl multi-name signature", parse'decls "f, g :: Int" `shouldBe` [Signature "f" ([], Term'T'Id (Term'Id'Const "Int")), Signature "g" ([], Term'T'Id (Term'Id'Const "Int"))])
          , ("decl arrow signature", parse'decls "f :: a -> a" `shouldBe` [Signature "f" ([], Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "a")])])
          , ("decl single pred signature", parse'decls "f :: (Eq a) => a -> Bool" `shouldBe` [Signature "f" ([Is'In "Eq" (Term'T'Id (Term'Id'Var "a"))], Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Const "Bool")])])
          , ("decl multi pred signature", parse'decls "f :: (Eq a, Show a) => a -> String" `shouldBe` [Signature "f" ([Is'In "Eq" (Term'T'Id (Term'Id'Var "a")), Is'In "Show" (Term'T'Id (Term'Id'Var "a"))], Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Const "String")])])
          , ("decl forall signature", parse'decls "f :: forall a . a -> a" `shouldBe` [Signature "f" ([], Term'T'Forall [Term'Id'Var "a"] ([], Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "a")]))])
          , ("decl data bool", parse'decls "data Bool = True | False" `shouldBe` [Data'Decl "Bool" [] [Con'Decl "True" [], Con'Decl "False" []]])
          , ("decl data maybe", parse'decls "data Maybe a = Nothing | Just a" `shouldBe` [Data'Decl "Maybe" ["a"] [Con'Decl "Nothing" [], Con'Decl "Just" [Term'T'Id (Term'Id'Var "a")]]])
          , ("decl data either", parse'decls "data Either a b = Left a | Right b" `shouldBe` [Data'Decl "Either" ["a", "b"] [Con'Decl "Left" [Term'T'Id (Term'Id'Var "a")], Con'Decl "Right" [Term'T'Id (Term'Id'Var "b")]]])
          , ("decl data record", parse'decls "data Person = Person { name :: String, age :: Int }" `shouldBe` [Data'Decl "Person" [] [Con'Record'Decl "Person" [("name", Term'T'Id (Term'Id'Const "String")), ("age", Term'T'Id (Term'Id'Const "Int"))]]])
          , ("decl data paren op constr", parse'decls "data Wrap a = (:.) a a" `shouldBe` [Data'Decl "Wrap" ["a"] [Con'Decl ":." [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "a")]]])
          , ("decl type alias const", parse'decls "type Age = Int" `shouldBe` [Type'Alias "Age" [] (Term'T'Id (Term'Id'Const "Int"))])
          , ("decl type alias tuple", parse'decls "type Pair a b = (a, b)" `shouldBe` [Type'Alias "Pair" ["a", "b"] (Term'T'Tuple [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "b")])])
          , ("decl class no supers", parse'decls "class Eq a" `shouldBe` [Class'Decl "Eq" "a" [] []])
          , ("decl class super with sig", parse'decls "class (Eq a) => Ord a where { (<=) :: a -> a -> Bool }" `shouldBe` [Class'Decl "Ord" "a" [Is'In "Eq" (Term'T'Id (Term'Id'Var "a"))] [Signature "<=" ([], Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Const "Bool")]])]])
          , ("decl instance no binds", parse'decls "instance Eq Bool" `shouldBe` [Instance ([], Is'In "Eq" (Term'T'Id (Term'Id'Const "Bool"))) []])
          , ("decl instance with bind", parse'decls "instance Eq Bool where { (==) = eq }" `shouldBe` [Instance ([], Is'In "Eq" (Term'T'Id (Term'Id'Const "Bool"))) [Binding (Term'P'Id (Term'Id'Var "==")) (Term'E'Id (Term'Id'Var "eq"))]])
          , ("decl module header", parse'module "module Foo where\nx = 1" `shouldBe` [Binding (Term'P'Id (Term'Id'Var "x")) (Term'E'Lit (Lit'Int 1))])
          , ("decl module implicit top layout", parse'module "x = 1\ny = 2" `shouldBe` [Binding (Term'P'Id (Term'Id'Var "x")) (Term'E'Lit (Lit'Int 1)), Binding (Term'P'Id (Term'Id'Var "y")) (Term'E'Lit (Lit'Int 2))])
          , ("type const", parse'type "Int" `shouldParseEq` Term'T'Id (Term'Id'Const "Int"))
          , ("type var", parse'type "a" `shouldParseEq` Term'T'Id (Term'Id'Var "a"))
          , ("type app one arg", parse'type "Maybe a" `shouldParseEq` Term'T'App [Term'T'Id (Term'Id'Const "Maybe"), Term'T'Id (Term'Id'Var "a")])
          , ("type app two args flat", parse'type "Either a b" `shouldParseEq` Term'T'App [Term'T'Id (Term'Id'Const "Either"), Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "b")])
          , ("type nested app", parse'type "Maybe (Either a b)" `shouldParseEq` Term'T'App [Term'T'Id (Term'Id'Const "Maybe"), Term'T'App [Term'T'Id (Term'Id'Const "Either"), Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "b")]])
          , ("type list", parse'type "[Int]" `shouldParseEq` Term'T'List (Term'T'Id (Term'Id'Const "Int")))
          , ("type pair tuple", parse'type "(Int, Bool)" `shouldParseEq` Term'T'Tuple [Term'T'Id (Term'Id'Const "Int"), Term'T'Id (Term'Id'Const "Bool")])
          , ("type unit", parse'type "()" `shouldParseEq` Term'T'Unit)
          , ("type arrow", parse'type "Int -> Bool" `shouldParseEq` Term'T'Arrow [Term'T'Id (Term'Id'Const "Int"), Term'T'Id (Term'Id'Const "Bool")])
          , ("type arrow right nested", parse'type "a -> b -> c" `shouldParseEq` Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Arrow [Term'T'Id (Term'Id'Var "b"), Term'T'Id (Term'Id'Var "c")]])
          , ("type arrow paren left", parse'type "(a -> b) -> c" `shouldParseEq` Term'T'Arrow [Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "b")], Term'T'Id (Term'Id'Var "c")])
          , ("type forall", parse'type "forall a . a -> a" `shouldParseEq` Term'T'Forall [Term'Id'Var "a"] ([], Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "a")]))
          , ("type forall two vars", parse'type "forall a b . (a, b)" `shouldParseEq` Term'T'Forall [Term'Id'Var "a", Term'Id'Var "b"] ([], Term'T'Tuple [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "b")]))
          , ("type empty list constr", parse'type "[]" `shouldParseEq` Term'T'Id (Term'Id'Const "[]"))
          , ("type arrow constr", parse'type "(->)" `shouldParseEq` Term'T'Id (Term'Id'Const "(->)"))
          , ("type tuple constr", parse'type "(,)" `shouldParseEq` Term'T'Id (Term'Id'Const "(,)"))
          , ("type arrow of lists", parse'type "[a] -> [a]" `shouldParseEq` Term'T'Arrow [Term'T'List (Term'T'Id (Term'Id'Var "a")), Term'T'List (Term'T'Id (Term'Id'Var "a"))])
          , ("type mixed tuple", parse'type "(Maybe a, [b], c -> d)" `shouldParseEq` Term'T'Tuple [Term'T'App [Term'T'Id (Term'Id'Const "Maybe"), Term'T'Id (Term'Id'Var "a")], Term'T'List (Term'T'Id (Term'Id'Var "b")), Term'T'Arrow [Term'T'Id (Term'Id'Var "c"), Term'T'Id (Term'Id'Var "d")]])
          ]
    mapM_ (\(case'name, case'action) -> it case'name case'action) cases


  describe "Bulk declaration coverage" $ do
    let cases :: [(String, IO ())]
        cases =
          [ ("fixity infixl", parse'decls "infixl 5 +" `shouldBe` [Fixity Infix Assoc.Left 5 "+"])
          , ("fixity infixr", parse'decls "infixr 0 $" `shouldBe` [Fixity Infix Assoc.Right 0 "$"])
          , ("fixity infix none", parse'decls "infix 6 *" `shouldBe` [Fixity Infix Assoc.None 6 "*"])
          , ("fixity prefix", parse'decls "prefix 9 |>" `shouldBe` [Fixity Prefix Assoc.None 9 "|>"])
          , ("fixity prefixr", parse'decls "prefixr 0 `print`" `shouldBe` [Fixity Prefix Assoc.Right 0 "print"])
          , ("fixity postfix", parse'decls "postfix 9 !" `shouldBe` [Fixity Postfix Assoc.None 9 "!"])
          , ("fixity postfixl", parse'decls "postfixl 0 `done`" `shouldBe` [Fixity Postfix Assoc.Left 0 "done"])
          , ("fixity prefixl", parse'decls "prefixl 5 +" `shouldBe` [Fixity Prefix Assoc.Left 5 "+"])
          , ("data nullary no constructors", parse'decls "data Void" `shouldBe` [Data'Decl "Void" [] []])
          , ("data bool", parse'decls "data Bool = True | False" `shouldBe` [Data'Decl "Bool" [] [Con'Decl "True" [], Con'Decl "False" []]])
          , ("data maybe", parse'decls "data Maybe a = Nothing | Just a" `shouldBe` [Data'Decl "Maybe" ["a"] [Con'Decl "Nothing" [], Con'Decl "Just" [Term'T'Id (Term'Id'Var "a")]]])
          , ("data list", parse'decls "data List a = Nil | Cons a (List a)" `shouldBe` [Data'Decl "List" ["a"] [Con'Decl "Nil" [], Con'Decl "Cons" [Term'T'Id (Term'Id'Var "a"), Term'T'App [Term'T'Id (Term'Id'Const "List"), Term'T'Id (Term'Id'Var "a")]]]])
          , ("data wrap", parse'decls "data Wrap a = Wrap a" `shouldBe` [Data'Decl "Wrap" ["a"] [Con'Decl "Wrap" [Term'T'Id (Term'Id'Var "a")]]])
          , ("data two params", parse'decls "data Either a b = Left a | Right b" `shouldBe` [Data'Decl "Either" ["a", "b"] [Con'Decl "Left" [Term'T'Id (Term'Id'Var "a")], Con'Decl "Right" [Term'T'Id (Term'Id'Var "b")]]])
          , ("data phantom", parse'decls "data Phantom a = Ph" `shouldBe` [Data'Decl "Phantom" ["a"] [Con'Decl "Ph" []]])
          , ("data single record", parse'decls "data Rec = Rec { x :: Int }" `shouldBe` [Data'Decl "Rec" [] [Con'Record'Decl "Rec" [("x", Term'T'Id (Term'Id'Const "Int"))]]])
          , ("data two field record", parse'decls "data R = R { x :: Int, y :: Bool }" `shouldBe` [Data'Decl "R" [] [Con'Record'Decl "R" [("x", Term'T'Id (Term'Id'Const "Int")), ("y", Term'T'Id (Term'Id'Const "Bool"))]]])
          , ("data tuple constructor argument", parse'decls "data T = C (Int, Bool)" `shouldBe` [Data'Decl "T" [] [Con'Decl "C" [Term'T'Tuple [Term'T'Id (Term'Id'Const "Int"), Term'T'Id (Term'Id'Const "Bool")]]]])
          , ("data list constructor argument", parse'decls "data T = C [Int]" `shouldBe` [Data'Decl "T" [] [Con'Decl "C" [Term'T'List (Term'T'Id (Term'Id'Const "Int"))]]])
          , ("data higher-kinded application", parse'decls "data T m = C (m Int)" `shouldBe` [Data'Decl "T" ["m"] [Con'Decl "C" [Term'T'App [Term'T'Id (Term'Id'Var "m"), Term'T'Id (Term'Id'Const "Int")]]]])
          , ("data unit constructor argument", parse'decls "data T = U ()" `shouldBe` [Data'Decl "T" [] [Con'Decl "U" [Term'T'Unit]]])
          , ("data two constructors with args", parse'decls "data E a = L a | R a" `shouldBe` [Data'Decl "E" ["a"] [Con'Decl "L" [Term'T'Id (Term'Id'Var "a")], Con'Decl "R" [Term'T'Id (Term'Id'Var "a")]]])
          , ("class empty", parse'decls "class C a" `shouldBe` [Class'Decl "C" "a" [] []])
          , ("class one method", parse'decls "class Num a where { (+) :: a -> a -> a }" `shouldBe` [Class'Decl "Num" "a" [] [Signature "+" ([], Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "a")]])]])
          , ("class two methods", parse'decls "class Num a where { (+) :: a -> a -> a ; (*) :: a -> a -> a }" `shouldBe` [Class'Decl "Num" "a" [] [Signature "+" ([], Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "a")]]), Signature "*" ([], Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "a")]])]])
          , ("class single superclass", parse'decls "class Eq a => Ord a" `shouldBe` [Class'Decl "Ord" "a" [Is'In "Eq" (Term'T'Id (Term'Id'Var "a"))] []])
          , ("class multi superclass", parse'decls "class (Eq a, Show a) => Ord a" `shouldBe` [Class'Decl "Ord" "a" [Is'In "Eq" (Term'T'Id (Term'Id'Var "a")), Is'In "Show" (Term'T'Id (Term'Id'Var "a"))] []])
          , ("class superclass with signs", parse'decls "class Eq a => Ord a where { (<) :: a -> a -> Bool }" `shouldBe` [Class'Decl "Ord" "a" [Is'In "Eq" (Term'T'Id (Term'Id'Var "a"))] [Signature "<" ([], Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Const "Bool")]])]])
          , ("class qualified method", parse'decls "class C a where { f :: Eq a => a -> a }" `shouldBe` [Class'Decl "C" "a" [] [Signature "f" ([Is'In "Eq" (Term'T'Id (Term'Id'Var "a"))], Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "a")])]])
          , ("class higher-kinded method", parse'decls "class M m where { f :: m Int }" `shouldBe` [Class'Decl "M" "m" [] [Signature "f" ([], Term'T'App [Term'T'Id (Term'Id'Var "m"), Term'T'Id (Term'Id'Const "Int")])]])
          , ("class tuple method", parse'decls "class C a where { f :: (a, a) -> a }" `shouldBe` [Class'Decl "C" "a" [] [Signature "f" ([], Term'T'Arrow [Term'T'Tuple [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "a")], Term'T'Id (Term'Id'Var "a")])]])
          , ("class list method", parse'decls "class C a where { f :: [a] -> Int }" `shouldBe` [Class'Decl "C" "a" [] [Signature "f" ([], Term'T'Arrow [Term'T'List (Term'T'Id (Term'Id'Var "a")), Term'T'Id (Term'Id'Const "Int")])]])
          , ("instance empty", parse'decls "instance Num Int" `shouldBe` [Instance ([], Is'In "Num" (Term'T'Id (Term'Id'Const "Int"))) []])
          , ("instance one binding", parse'decls "instance Num Int where { (+) x y = x }" `shouldBe` [Instance ([], Is'In "Num" (Term'T'Id (Term'Id'Const "Int"))) [Binding (Term'P'App [Term'P'Id (Term'Id'Var "+"), Term'P'Id (Term'Id'Var "x"), Term'P'Id (Term'Id'Var "y")]) (Term'E'Id (Term'Id'Var "x"))]])
          , ("instance qualified head", parse'decls "instance Num a => Foo a" `shouldBe` [Instance ([Is'In "Num" (Term'T'Id (Term'Id'Var "a"))], Is'In "Foo" (Term'T'Id (Term'Id'Var "a"))) []])
          , ("instance qualified applied head", parse'decls "instance Num a => Constant (Wrap a)" `shouldBe` [Instance ([Is'In "Num" (Term'T'Id (Term'Id'Var "a"))], Is'In "Constant" (Term'T'App [Term'T'Id (Term'Id'Const "Wrap"), Term'T'Id (Term'Id'Var "a")])) []])
          , ("instance multi context", parse'decls "instance (Eq a, Show a) => Ord a" `shouldBe` [Instance ([Is'In "Eq" (Term'T'Id (Term'Id'Var "a")), Is'In "Show" (Term'T'Id (Term'Id'Var "a"))], Is'In "Ord" (Term'T'Id (Term'Id'Var "a"))) []])
          , ("instance two bindings", parse'decls "instance Num Int where { (+) x y = x ; (*) x y = y }" `shouldBe` [Instance ([], Is'In "Num" (Term'T'Id (Term'Id'Const "Int"))) [Binding (Term'P'App [Term'P'Id (Term'Id'Var "+"), Term'P'Id (Term'Id'Var "x"), Term'P'Id (Term'Id'Var "y")]) (Term'E'Id (Term'Id'Var "x")), Binding (Term'P'App [Term'P'Id (Term'Id'Var "*"), Term'P'Id (Term'Id'Var "x"), Term'P'Id (Term'Id'Var "y")]) (Term'E'Id (Term'Id'Var "y"))]])
          , ("instance list head", parse'decls "instance Foo [a]" `shouldBe` [Instance ([], Is'In "Foo" (Term'T'List (Term'T'Id (Term'Id'Var "a")))) []])
          , ("instance arrow head", parse'decls "instance Foo (a -> b)" `shouldBe` [Instance ([], Is'In "Foo" (Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "b")])) []])
          , ("instance implicit where layout", parse'decls "instance Num Int where\n  (+) x y = x" `shouldBe` [Instance ([], Is'In "Num" (Term'T'Id (Term'Id'Const "Int"))) [Binding (Term'P'App [Term'P'Id (Term'Id'Var "+"), Term'P'Id (Term'Id'Var "x"), Term'P'Id (Term'Id'Var "y")]) (Term'E'Id (Term'Id'Var "x"))]])
          , ("instance double head", parse'decls "instance Fractional Double" `shouldBe` [Instance ([], Is'In "Fractional" (Term'T'Id (Term'Id'Const "Double"))) []])
          , ("type alias list", parse'decls "type String = [Char]" `shouldBe` [Type'Alias "String" [] (Term'T'List (Term'T'Id (Term'Id'Const "Char")))])
          , ("type alias identity", parse'decls "type Id a = a" `shouldBe` [Type'Alias "Id" ["a"] (Term'T'Id (Term'Id'Var "a"))])
          , ("type alias pair", parse'decls "type Pair' a b = (a, b)" `shouldBe` [Type'Alias "Pair'" ["a", "b"] (Term'T'Tuple [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "b")])])
          , ("type alias arrow", parse'decls "type Fn a = a -> a" `shouldBe` [Type'Alias "Fn" ["a"] (Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "a")])])
          , ("type alias application", parse'decls "type MInt m = m Int" `shouldBe` [Type'Alias "MInt" ["m"] (Term'T'App [Term'T'Id (Term'Id'Var "m"), Term'T'Id (Term'Id'Const "Int")])])
          , ("type alias unit", parse'decls "type U = ()" `shouldBe` [Type'Alias "U" [] Term'T'Unit])
          , ("module explicit block", parse'module "module Main where { foo = 23 }" `shouldBe` [Binding (Term'P'Id (Term'Id'Var "foo")) (Term'E'Lit (Lit'Int 23))])
          , ("module implicit block", parse'module "module Main where\nfoo = 23" `shouldBe` [Binding (Term'P'Id (Term'Id'Var "foo")) (Term'E'Lit (Lit'Int 23))])
          , ("module bare explicit block", parse'module "{ foo = 23 }" `shouldBe` [Binding (Term'P'Id (Term'Id'Var "foo")) (Term'E'Lit (Lit'Int 23))])
          , ("module two bindings", parse'module "{ foo = 23 ; bar = 24 }" `shouldBe` [Binding (Term'P'Id (Term'Id'Var "foo")) (Term'E'Lit (Lit'Int 23)), Binding (Term'P'Id (Term'Id'Var "bar")) (Term'E'Lit (Lit'Int 24))])
          , ("module fixity and binding", parse'module "module Main where { infixl 5 + ; foo = 23 }" `shouldBe` [Fixity Infix Assoc.Left 5 "+", Binding (Term'P'Id (Term'Id'Var "foo")) (Term'E'Lit (Lit'Int 23))])
          , ("module signature and binding", parse'module "module Main where { foo :: Int ; foo = 23 }" `shouldBe` [Signature "foo" ([], Term'T'Id (Term'Id'Const "Int")), Binding (Term'P'Id (Term'Id'Var "foo")) (Term'E'Lit (Lit'Int 23))])
          , ("data three params", parse'decls "data Trio a b c = T3 a b c" `shouldBe` [Data'Decl "Trio" ["a", "b", "c"] [Con'Decl "T3" [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "b"), Term'T'Id (Term'Id'Var "c")]]])
          , ("data nested application", parse'decls "data Nested = N [Maybe Int]" `shouldBe` [Data'Decl "Nested" [] [Con'Decl "N" [Term'T'List (Term'T'App [Term'T'Id (Term'Id'Const "Maybe"), Term'T'Id (Term'Id'Const "Int")])]]])
          , ("data operator constructor", parse'decls "data OpT = (:+) Int Int" `shouldBe` [Data'Decl "OpT" [] [Con'Decl ":+" [Term'T'Id (Term'Id'Const "Int"), Term'T'Id (Term'Id'Const "Int")]]])
          , ("fixity two names", parse'decls "infixl 5 +, *" `shouldBe` [Fixity Infix Assoc.Left 5 "+", Fixity Infix Assoc.Left 5 "*"])
          , ("fixity paren op", parse'decls "infixl 5 (+)" `shouldBe` [Fixity Infix Assoc.Left 5 "+"])
          , ("fixity three names", parse'decls "infixr 0 $, +++, ???" `shouldBe` [Fixity Infix Assoc.Right 0 "$", Fixity Infix Assoc.Right 0 "+++", Fixity Infix Assoc.Right 0 "???"])
          , ("fixity bare minus", parse'decls "infixl 6 -" `shouldBe` [Fixity Infix Assoc.Left 6 "-"])
          , ("fixity bare cons", parse'decls "infixr 5 :" `shouldBe` [Fixity Infix Assoc.Right 5 ":"])
          , ("data three nullary", parse'decls "data E2 = A | B | C" `shouldBe` [Data'Decl "E2" [] [Con'Decl "A" [], Con'Decl "B" [], Con'Decl "C" []]])
          , ("data unit alias", parse'decls "data Unit2 = U2" `shouldBe` [Data'Decl "Unit2" [] [Con'Decl "U2" []]])
          , ("data pair params", parse'decls "data Pair2 a b = P2 a b" `shouldBe` [Data'Decl "Pair2" ["a", "b"] [Con'Decl "P2" [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "b")]]])
          , ("data two-field record", parse'decls "data Rec2 = R2 { a :: Int, b :: Int }" `shouldBe` [Data'Decl "Rec2" [] [Con'Record'Decl "R2" [("a", Term'T'Id (Term'Id'Const "Int")), ("b", Term'T'Id (Term'Id'Const "Int"))]]])
          , ("data list argument", parse'decls "data L2 a = C2 [a]" `shouldBe` [Data'Decl "L2" ["a"] [Con'Decl "C2" [Term'T'List (Term'T'Id (Term'Id'Var "a"))]]])
          , ("data tuple argument", parse'decls "data T2 = C2 (Int, Int)" `shouldBe` [Data'Decl "T2" [] [Con'Decl "C2" [Term'T'Tuple [Term'T'Id (Term'Id'Const "Int"), Term'T'Id (Term'Id'Const "Int")]]]])
          , ("data applied argument", parse'decls "data M2 m a = C2 (m a)" `shouldBe` [Data'Decl "M2" ["m", "a"] [Con'Decl "C2" [Term'T'App [Term'T'Id (Term'Id'Var "m"), Term'T'Id (Term'Id'Var "a")]]]])
          , ("instance empty Eq Int", parse'decls "instance Eq Int" `shouldBe` [Instance ([], Is'In "Eq" (Term'T'Id (Term'Id'Const "Int"))) []])
          , ("instance empty Ord Bool", parse'decls "instance Ord Bool" `shouldBe` [Instance ([], Is'In "Ord" (Term'T'Id (Term'Id'Const "Bool"))) []])
          , ("instance empty Show Int", parse'decls "instance Show Int" `shouldBe` [Instance ([], Is'In "Show" (Term'T'Id (Term'Id'Const "Int"))) []])
          , ("instance empty Num Double", parse'decls "instance Num Double" `shouldBe` [Instance ([], Is'In "Num" (Term'T'Id (Term'Id'Const "Double"))) []])
          , ("instance empty Eq list", parse'decls "instance Eq [a]" `shouldBe` [Instance ([], Is'In "Eq" (Term'T'List (Term'T'Id (Term'Id'Var "a")))) []])
          , ("class two supers no sigs", parse'decls "class (Eq a, Ord a) => C a" `shouldBe` [Class'Decl "C" "a" [Is'In "Eq" (Term'T'Id (Term'Id'Var "a")), Is'In "Ord" (Term'T'Id (Term'Id'Var "a"))] []])
          , ("class single var method", parse'decls "class C a where { f :: a }" `shouldBe` [Class'Decl "C" "a" [] [Signature "f" ([], Term'T'Id (Term'Id'Var "a"))]])
          , ("class two const methods", parse'decls "class C a where { f :: Int ; g :: Bool }" `shouldBe` [Class'Decl "C" "a" [] [Signature "f" ([], Term'T'Id (Term'Id'Const "Int")), Signature "g" ([], Term'T'Id (Term'Id'Const "Bool"))]])
          , ("type alias bool", parse'decls "type B = Bool" `shouldBe` [Type'Alias "B" [] (Term'T'Id (Term'Id'Const "Bool"))])
          , ("type alias maybe", parse'decls "type M2 a = Maybe a" `shouldBe` [Type'Alias "M2" ["a"] (Term'T'App [Term'T'Id (Term'Id'Const "Maybe"), Term'T'Id (Term'Id'Var "a")])])
          , ("type alias single arrow", parse'decls "type F2 a b = a -> b" `shouldBe` [Type'Alias "F2" ["a", "b"] (Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Id (Term'Id'Var "b")])])
          , ("type alias int list", parse'decls "type L2 = [Int]" `shouldBe` [Type'Alias "L2" [] (Term'T'List (Term'T'Id (Term'Id'Const "Int")))])
          , ("signature three names", parse'decls "f, g, h :: Int" `shouldBe` [Signature "f" ([], Term'T'Id (Term'Id'Const "Int")), Signature "g" ([], Term'T'Id (Term'Id'Const "Int")), Signature "h" ([], Term'T'Id (Term'Id'Const "Int"))])
          , ("signature int endomap", parse'decls "op :: Int -> Int" `shouldBe` [Signature "op" ([], Term'T'Arrow [Term'T'Id (Term'Id'Const "Int"), Term'T'Id (Term'Id'Const "Int")])])
          , ("module two bindings explicit", parse'module "module Main where { x = 1 ; y = 2 }" `shouldBe` [Binding (Term'P'Id (Term'Id'Var "x")) (Term'E'Lit (Lit'Int 1)), Binding (Term'P'Id (Term'Id'Var "y")) (Term'E'Lit (Lit'Int 2))])
          , ("module bare sig and binding", parse'module "{ x :: Int ; x = 1 }" `shouldBe` [Signature "x" ([], Term'T'Id (Term'Id'Const "Int")), Binding (Term'P'Id (Term'Id'Var "x")) (Term'E'Lit (Lit'Int 1))])
          , ("type bool const", parse'type "Bool" `shouldParseEq` Term'T'Id (Term'Id'Const "Bool"))
          , ("type char const", parse'type "Char" `shouldParseEq` Term'T'Id (Term'Id'Const "Char"))
          , ("type double const", parse'type "Double" `shouldParseEq` Term'T'Id (Term'Id'Const "Double"))
          , ("type string const", parse'type "String" `shouldParseEq` Term'T'Id (Term'Id'Const "String"))
          , ("type var b", parse'type "b" `shouldParseEq` Term'T'Id (Term'Id'Var "b"))
          , ("type maybe bool", parse'type "Maybe Bool" `shouldParseEq` Term'T'App [Term'T'Id (Term'Id'Const "Maybe"), Term'T'Id (Term'Id'Const "Bool")])
          , ("type char list", parse'type "[Char]" `shouldParseEq` Term'T'List (Term'T'Id (Term'Id'Const "Char")))
          , ("type bool-char pair", parse'type "(Bool, Char)" `shouldParseEq` Term'T'Tuple [Term'T'Id (Term'Id'Const "Bool"), Term'T'Id (Term'Id'Const "Char")])
          , ("type nested arrows", parse'type "Int -> Int -> Bool" `shouldParseEq` Term'T'Arrow [Term'T'Id (Term'Id'Const "Int"), Term'T'Arrow [Term'T'Id (Term'Id'Const "Int"), Term'T'Id (Term'Id'Const "Bool")]])
          , ("type maybe list", parse'type "Maybe [Int]" `shouldParseEq` Term'T'App [Term'T'Id (Term'Id'Const "Maybe"), Term'T'List (Term'T'Id (Term'Id'Const "Int"))])
          , ("instance maybe head", parse'decls "instance C (Maybe a)" `shouldBe` [Instance ([], Is'In "C" (Term'T'App [Term'T'Id (Term'Id'Const "Maybe"), Term'T'Id (Term'Id'Var "a")])) []])
          , ("type alias two arrows", parse'decls "type F a b = a -> b -> a" `shouldBe` [Type'Alias "F" ["a", "b"] (Term'T'Arrow [Term'T'Id (Term'Id'Var "a"), Term'T'Arrow [Term'T'Id (Term'Id'Var "b"), Term'T'Id (Term'Id'Var "a")]])])
          , ("module implicit two bindings", parse'module "module Main where\nfoo = 23\nbar = 24" `shouldBe` [Binding (Term'P'Id (Term'Id'Var "foo")) (Term'E'Lit (Lit'Int 23)), Binding (Term'P'Id (Term'Id'Var "bar")) (Term'E'Lit (Lit'Int 24))])
          ]
    mapM_ (\(case'name, case'action) -> it case'name case'action) cases
