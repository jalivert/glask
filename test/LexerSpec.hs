module LexerSpec where


import Test.Hspec

import Data.Function

import Compiler.Lexer
import Compiler.Lexer.Token
import Compiler.Lexer.Position
import Compiler.Lexer.Located
import Compiler.Lexer.Utils


infix 5 |=>

(|=>) :: String -> (Position -> Token) -> Bool
source |=> place'tok = produces
  where
    lexeme = eval'parser read'token source
    pos = at lexeme :: Position
    produces = lexeme == place'tok pos


infix 5 ~:

(~:) :: [Position -> Token] -> String -> Bool
place'toks ~: source = equivalent
  where
    parsed'tokens = use'parser read'token source
    equivalent = and $ zipWith (~~) parsed'tokens (map ($ None) place'toks)

-- (~:) :: [Position -> Token] -> String -> Bool
-- place'toks ~: source = equaivalent
--   where
--     parsed'tokens = use'parser read'token source
--     references = zipWith ((&) . at) parsed'tokens place'toks
--     all'same = zipWith (==) references parsed'tokens
--     equaivalent = all all'same


spec :: Spec
spec = do
  describe "Test reading keywords" $ do

    it "reads 'data'" $ do
      "data" |=> Tok'Data

    it "reads 'if'" $ do
      "if" |=> Tok'If

    it "reads 'then'" $ do
      "then" |=> Tok'Then

    it "reads 'else'" $ do
      "else" |=> Tok'Else

    it "reads 'let'" $ do
      "let" |=> Tok'Let

    it "reads 'in'" $ do
      "in" |=> Tok'In

    it "reads 'case'" $ do
      "case" |=> Tok'Case

    it "reads 'of'" $ do
      "of" |=> Tok'Of

    it "reads 'type'" $ do
      "type" |=> Tok'Type

    it "reads '_'" $ do
      "_" |=> Tok'Named'Hole "_"

    it "reads '\\'" $ do
      "\\" |=> Tok'Lambda

    it "reads 'class'" $ do
      "class" |=> Tok'Class

    it "reads 'instance'" $ do
      "instance" |=> Tok'Instance

    it "reads 'where'" $ do
      "where" |=> Tok'Where

    it "reads 'module'" $ do
      "module" |=> Tok'Module

    it "reads '::'" $ do
      "::" |=> Tok'Has'Type

    it "reads a decimal number" $ do
      "23" |=> Tok'Int 23

  describe "Test reading identifiers" $ do

    it "reads a simple alphabetical variable identifier" $ do
      "identifier" |=> Tok'Ident'Var "identifier"

    it "reads a variable identifier with apostrophe" $ do
      "id'" |=> Tok'Ident'Var "id'"

    it "reads a constant identifier" $ do
      "Int" |=> Tok'Ident'Const "Int"

    it "reads a constant identifier with apostrophe" $ do
      "Special'Type" |=> Tok'Ident'Const "Special'Type"

    it "reads a variable operator identifier" $ do
      "+" |=> Tok'Operator "+"

    it "reads a simple constant operator identifier" $ do
      ":" |=> Tok'Operator'Const ":"

    it "reads a constant operator identifier" $ do
      ":=>" |=> Tok'Operator'Const ":=>"

  describe "Test reading special symbols" $ do

    it "reads '('" $ do
      "(" |=> Tok'Left'Paren



  describe "Test reading sequence of tokens" $ do

    -- this one passes when the list is empty even though it should
    it "reads a simple arithmetic operation" $ do
      [Tok'Int 23, Tok'Operator "+", Tok'Int 42] ~: "23 + 42"

  describe "Test implicit layout" $ do

    it "opens and closes a virtual block around aligned declarations" $ do
      [ Tok'Let, Tok'Left'Brace,
        Tok'Ident'Var "a", Tok'Operator "=", Tok'Int 0, Tok'Semicolon,
        Tok'Ident'Var "b", Tok'Operator "=", Tok'Int 1, Tok'Right'Brace,
        Tok'In, Tok'Ident'Var "a" ] ~: "let a = 0\n    b = 1\nin a"

    it "treats an indented line as a continuation" $ do
      [ Tok'Ident'Var "foo", Tok'Operator "=",
        Tok'Ident'Var "bar", Tok'Operator "+", Tok'Ident'Var "baz" ] ~: "foo = bar\n  + baz"

    it "closes a virtual block before a dedented token" $ do
      [ Tok'Where, Tok'Left'Brace,
        Tok'Ident'Var "foo", Tok'Operator "=", Tok'Int 23, Tok'Right'Brace ] ~: "where\n  foo = 23\n"

  describe "Test virtual layout token sequences" $ do

    it "opens a virtual block with semicolons across three bindings" $ do
      [ Tok'Let, Tok'Left'Brace,
        Tok'Ident'Var "x", Tok'Operator "=", Tok'Int 1, Tok'Semicolon,
        Tok'Ident'Var "y", Tok'Operator "=", Tok'Int 2, Tok'Semicolon,
        Tok'Ident'Var "z", Tok'Operator "=", Tok'Int 3, Tok'Right'Brace,
        Tok'In, Tok'Ident'Var "x" ] ~: "let x = 1\n    y = 2\n    z = 3\nin x"

    it "treats two indented lines as continuations" $ do
      [ Tok'Ident'Var "foo", Tok'Operator "=",
        Tok'Ident'Var "bar", Tok'Operator "+", Tok'Ident'Var "baz",
        Tok'Operator "+", Tok'Ident'Var "qux" ] ~: "foo = bar\n  + baz\n  + qux"

    it "closes a where block before a dedented top-level declaration" $ do
      [ Tok'Ident'Var "f", Tok'Ident'Var "x", Tok'Operator "=", Tok'Ident'Var "y",
        Tok'Where, Tok'Left'Brace,
        Tok'Ident'Var "y", Tok'Operator "=", Tok'Int 1, Tok'Right'Brace,
        Tok'Ident'Var "z", Tok'Operator "=", Tok'Int 2 ] ~: "f x = y\n  where\n    y = 1\nz = 2"

    it "separates case branches with a virtual semicolon and closes at EOF" $ do
      [ Tok'Case, Tok'Ident'Var "x", Tok'Of, Tok'Left'Brace,
        Tok'Ident'Const "A", Tok'Operator "->", Tok'Int 1, Tok'Semicolon,
        Tok'Ident'Const "B", Tok'Operator "->", Tok'Int 2, Tok'Right'Brace ] ~: "case x of\n  A -> 1\n  B -> 2"

    it "reads an explicit brace block after let without virtual tokens" $ do
      [ Tok'Let, Tok'Left'Brace,
        Tok'Ident'Var "a", Tok'Operator "=", Tok'Int 0, Tok'Right'Brace,
        Tok'In, Tok'Ident'Var "a" ] ~: "let { a = 0 } in a"

    it "opens and closes a virtual block around a single let binding" $ do
      [ Tok'Let, Tok'Left'Brace,
        Tok'Ident'Var "a", Tok'Operator "=", Tok'Int 0, Tok'Right'Brace,
        Tok'In, Tok'Ident'Var "a" ] ~: "let a = 0\nin a"

    it "closes a let block on dedent without an in keyword" $ do
      [ Tok'Let, Tok'Left'Brace,
        Tok'Ident'Var "a", Tok'Operator "=", Tok'Int 1, Tok'Semicolon,
        Tok'Ident'Var "b", Tok'Operator "=", Tok'Int 2, Tok'Right'Brace,
        Tok'Ident'Var "c", Tok'Operator "=", Tok'Int 3 ] ~: "let a = 1\n    b = 2\nc = 3"

    it "treats a comment line as a newline inside a layout block" $ do
      [ Tok'Let, Tok'Left'Brace,
        Tok'Ident'Var "a", Tok'Operator "=", Tok'Int 0, Tok'Semicolon,
        Tok'Ident'Var "b", Tok'Operator "=", Tok'Int 1, Tok'Right'Brace,
        Tok'In, Tok'Ident'Var "a" ] ~: "let a = 0\n-- comment\n    b = 1\nin a"

    it "separates two where bindings and closes the block at EOF" $ do
      [ Tok'Ident'Var "f", Tok'Operator "=", Tok'Int 1,
        Tok'Where, Tok'Left'Brace,
        Tok'Ident'Var "g", Tok'Operator "=", Tok'Int 2, Tok'Semicolon,
        Tok'Ident'Var "h", Tok'Operator "=", Tok'Int 3, Tok'Right'Brace ] ~: "f = 1\n  where\n    g = 2\n    h = 3\n"

    it "reads an explicit brace block after of on one line" $ do
      [ Tok'Case, Tok'Ident'Var "x", Tok'Of, Tok'Left'Brace,
        Tok'Ident'Const "A", Tok'Operator "->", Tok'Int 1, Tok'Right'Brace ] ~: "case x of { A -> 1 }"

  describe "Test reading operator token sequences" $ do

    it "reads '=>' as one operator" $ do
      [Tok'Operator "=>"] ~: "=>"

    it "reads '->' as one operator" $ do
      [Tok'Operator "->"] ~: "->"

    it "reads '<-' as one operator" $ do
      [Tok'Operator "<-"] ~: "<-"

    it "reads '==' as one operator" $ do
      [Tok'Operator "=="] ~: "=="

    it "reads '/=' as one operator" $ do
      [Tok'Operator "/="] ~: "/="

    it "reads '++' as one operator" $ do
      [Tok'Operator "++"] ~: "++"

    it "reads '>>=' as one operator" $ do
      [Tok'Operator ">>="] ~: ">>="

    it "reads '<$>' as one operator" $ do
      [Tok'Operator "<$>"] ~: "<$>"

    it "reads ':' as a constructor operator" $ do
      [Tok'Operator'Const ":"] ~: ":"

    it "reads ':=>' as a constructor operator" $ do
      [Tok'Operator'Const ":=>"] ~: ":=>"

    it "reads '::' as a type signature symbol" $ do
      [Tok'Has'Type] ~: "::"

    it "splits ':abc' into an operator and an identifier" $ do
      [Tok'Operator'Const ":", Tok'Ident'Var "abc"] ~: ":abc"

  describe "Test reading literal token sequences" $ do

    it "reads a multi-digit decimal number" $ do
      [Tok'Int 12345] ~: "12345"

    it "reads a negative decimal number as one token" $ do
      [Tok'Int (-23)] ~: "-23"

    it "reads a spaced minus as an operator and a number" $ do
      [Tok'Operator "-", Tok'Int 23] ~: "- 23"

    it "reads an octal literal" $ do
      [Tok'Int 15] ~: "0o17"

    it "reads a hexadecimal literal" $ do
      [Tok'Int 255] ~: "0xFF"

    it "reads a negative hexadecimal literal as one token" $ do
      [Tok'Int (-255)] ~: "-0xFF"

    it "reads a decimal fraction" $ do
      [Tok'Double 3.14] ~: "3.14"

    it "reads a negative decimal fraction as one token" $ do
      [Tok'Double (-2.5)] ~: "-2.5"

    it "reads a decimal with a signed exponent" $ do
      [Tok'Double 1.0e10] ~: "1e+10"

    it "reads a fraction with a negative exponent" $ do
      [Tok'Double 2.5e-3] ~: "2.5e-3"

    it "splits an unsigned exponent into a number and an identifier" $ do
      [Tok'Int 1, Tok'Ident'Var "e10"] ~: "1e10"

  describe "Test reading character and string token sequences" $ do

    it "reads a lowercase character literal" $ do
      [Tok'Char 'a'] ~: "'a'"

    it "reads an uppercase character literal" $ do
      [Tok'Char 'Z'] ~: "'Z'"

    it "reads a character escape for newline" $ do
      [Tok'Char '\n'] ~: "'\\n'"

    it "reads a character escape for a quote" $ do
      [Tok'Char '\''] ~: "'\\''"

    it "reads a simple string literal" $ do
      [Tok'String "hello"] ~: "\"hello\""

    it "reads an empty string literal" $ do
      [Tok'String ""] ~: "\"\""

    it "reads a string with a newline escape" $ do
      [Tok'String "a\nb"] ~: "\"a\\nb\""

    it "reads a string with escaped quotes" $ do
      [Tok'String "say \"hi\""] ~: "\"say \\\"hi\\\"\""

    it "reads a string with an escaped backslash" $ do
      [Tok'String "a\\b"] ~: "\"a\\\\b\""

    it "reads a string with a tab escape" $ do
      [Tok'String "a\tb"] ~: "\"a\\tb\""

    it "reads a string that strips a non-special backslash" $ do
      [Tok'String "aqb"] ~: "\"a\\qb\""

  describe "Test comments and edge case token sequences" $ do

    it "skips a full-line comment before a token" $ do
      [Tok'Int 42] ~: "-- hello\n42"

    it "skips a trailing comment after a token" $ do
      [Tok'Int 42] ~: "42 -- hello\n"

    it "reads only-comment input as no tokens" $ do
      [] ~: "-- just a comment\n"

    it "reads empty input as no tokens" $ do
      [] ~: ""

    it "skips a comment between two tokens" $ do
      [Tok'Int 1, Tok'Int 2] ~: "1 -- skip\n2"

  describe "Test keyword and symbol token sequences" $ do

    it "reads a parenthesised pair with a comma" $ do
      [ Tok'Left'Paren, Tok'Ident'Var "a", Tok'Comma,
        Tok'Ident'Var "b", Tok'Right'Paren ] ~: "(a, b)"

    it "reads a bracketed pair with a comma" $ do
      [ Tok'Left'Bracket, Tok'Int 1, Tok'Comma,
        Tok'Int 2, Tok'Right'Bracket ] ~: "[1, 2]"

    it "reads an if-then-else keyword sequence" $ do
      [ Tok'If, Tok'Ident'Var "x", Tok'Then,
        Tok'Ident'Var "y", Tok'Else, Tok'Ident'Var "z" ] ~: "if x then y else z"

    it "reads a lambda abstraction head" $ do
      [ Tok'Lambda, Tok'Ident'Var "x",
        Tok'Operator "->", Tok'Ident'Var "x" ] ~: "\\x -> x"

    it "reads a data declaration head with alternatives" $ do
      [ Tok'Data, Tok'Ident'Const "Bool", Tok'Operator "=",
        Tok'Ident'Const "True", Tok'Operator "|", Tok'Ident'Const "False" ] ~: "data Bool = True | False"

    it "reads a type signature with '::'" $ do
      [Tok'Ident'Var "f", Tok'Has'Type, Tok'Ident'Const "Int"] ~: "f :: Int"

    it "reads a class declaration head" $ do
      [Tok'Class, Tok'Ident'Const "Eq", Tok'Ident'Var "a"] ~: "class Eq a"

    it "reads an instance declaration head" $ do
      [Tok'Instance, Tok'Ident'Const "Eq", Tok'Ident'Const "Int"] ~: "instance Eq Int"

    it "reads a forall type with a dot operator" $ do
      [ Tok'Forall, Tok'Ident'Var "a",
        Tok'Operator ".", Tok'Ident'Var "a" ] ~: "forall a . a"

    it "reads a named hole with a name" $ do
      [Tok'Named'Hole "_foo"] ~: "_foo"

    it "reads a keyword prefix as an identifier" $ do
      [Tok'Ident'Var "iffy"] ~: "iffy"

