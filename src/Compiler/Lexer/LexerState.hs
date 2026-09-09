module Compiler.Lexer.LexerState where


import Control.Monad.State
import Control.Monad
import Data.Word

import Compiler.Lexer.Token
import Compiler.Lexer.Position


-- TODO: CONSIDER: If I change it to ExceptT I can report specific errors
-- do I need that though?
type Parser a = State Parse'State a


data AlexInput = AlexInput
  { ai'prev           :: Char
  , ai'bytes          :: [Word8]
  , ai'rest           :: String
  , ai'line'number    :: Int
  , ai'column'number  :: Int }
  deriving Show


data Parse'State = Parse'State
  { input             :: AlexInput
  , lex'start'code    :: Int              -- lexer start code
  , string'buffer     :: String           -- temporary storage for strings
  , pending'tokens    :: [Token]          -- for when Parser consumes the lookeahead and decided to put it back
  , pending'position  :: Position         -- needed when parsing strings, chars, multi-line strings
  , done              :: Bool             -- flag signalizing that we got Tok'EOF
  , layout'stack      :: [Layout'Context] -- stack of open layout contexts
  , layout'expected    :: Bool             -- a layout keyword was just read, the next token opens a block
  , top'marker        :: Bool             -- the first token opens an implicit top-level block
  , saw'newline       :: Bool             -- a newline was read since the last emitted token
  , pending'virtuals  :: [Token] }        -- virtual tokens queued ahead, emitted without examination
  deriving Show


-- |  Layout context: an explicit brace block, an implicit block
-- |  aligned at a column, or a `let` waiting for its block and `in`.
data Layout'Context
  = Explicit
  | Implicit Int
  | LetPending
  deriving (Eq, Show)


initial'state :: String -> Parse'State
initial'state s = Parse'State
  { input = AlexInput
    { ai'prev = '\n'
    , ai'bytes = []
    , ai'rest = s
    , ai'line'number = 1
    , ai'column'number = 1 }
  , lex'start'code = 0
  , string'buffer = ""
  , pending'tokens = []
  , pending'position = Position { line = 1, column = 1 }
  , done = False
  , layout'stack = []
  , layout'expected = False
  , top'marker = False
  , saw'newline = False
  , pending'virtuals = [] }
