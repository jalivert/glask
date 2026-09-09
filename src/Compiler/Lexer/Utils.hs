module Compiler.Lexer.Utils where


import Control.Monad.State ( MonadState(get, put), evalState, runState )
import Codec.Binary.UTF8.String (encode)


import Compiler.Lexer.LexerState
    ( Parse'State(Parse'State, done, lex'start'code, string'buffer,
                  pending'position, pending'tokens, pending'virtuals, input,
                  layout'stack, layout'expected, top'marker, saw'newline),
      AlexInput(..), Parser, initial'state,
      Layout'Context(Explicit, Implicit, LetPending) )
import Compiler.Lexer.Position ( Position(Position, column, line) )
import Compiler.Lexer.Located ( Located(at) )
import Compiler.Lexer.Token ( Token(..) )
import Data.Word ( Word8 )


type Number'Of'Chars'Matched = Int


type Matched'Sequence = String


type Lex'Action = Number'Of'Chars'Matched -> Matched'Sequence -> Parser (Maybe Token)


plain'tok :: (Position -> Token) -> Lex'Action
plain'tok tok len _ = do
  pos <- get'position len
  let token = tok pos
  return $ Just token


parametrized'tok :: (a -> Position -> Token) -> (String -> a) -> Lex'Action
parametrized'tok tok read' len matched = do
  pos <- get'position len
  let token = tok (read' matched) pos
  return $ Just token


read'char :: Lex'Action
read'char 2 (c : '\'' : []) = do
  s <- get
  pos <- get'position 2
  put s{ lex'start'code = 0 }
  let token = Tok'Char c pos
  return $ Just token

read'char 3 ('\\' : seq : '\'' : []) = do
  let unesc =
        case seq of
          '\'' -> '\''
          '\\' -> '\\'
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'
          'b' -> '\b'
          'f' -> '\f'
          'v' -> '\v'
          _   -> seq
  s <- get
  pos <- get'position 3
  put s{ lex'start'code = 0 }
  let token = Tok'Char unesc pos
  return $ Just token

read'char _ _ = error "Can never happen."


append'string :: Lex'Action
append'string _ (c : _) = do
  s <- get
  put s{ string'buffer = c : (string'buffer s) }
  return Nothing
append'string _ _ = error "Can never happen."


escape'string :: Lex'Action
escape'string 2 ('\\' : c : []) = do
  let unesc =
        case c of
          {- '\'' -> '\'' -- is this needed? -}
          '"' -> '"'
          '\\' -> '\\'
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'
          'b' -> '\b'
          'f' -> '\f'
          'v' -> '\v'
          _   -> c
  s <- get
  put s{ string'buffer = unesc : (string'buffer s) }
  return Nothing
escape'string _ _ = error "Can never happen."


strip'backslash :: Lex'Action
strip'backslash 2 ('\\' : c : []) = do
  s <- get
  put s{ string'buffer = c : (string'buffer s) }
  return Nothing
strip'backslash _ _ = error "Can never happen."


carry'return'string :: Lex'Action
carry'return'string _ _ = do
  s <- get
  put s{ string'buffer = '\n' : (string'buffer s) }
  return Nothing


end'string :: Lex'Action
end'string _ _ = do
  s <- get
  -- (lexSC', prevSCs') <- getPreviousSC
  let buf = string'buffer s
  put  s{ lex'start'code = 0
        , string'buffer = "" }
  let token = Tok'String (reverse buf) (pending'position s)
  return $ Just token



-- The functions that must be provided to Alex's basic interface

-- The input: last character, unused bytes, remaining string
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai =
  case ai'bytes ai of
    (b : bs) ->
      Just (b, ai{ ai'bytes = bs })

    [] ->
      case ai'rest ai of
        [] -> Nothing

        (char : chars) ->
          let
            n = ai'line'number ai
            n' = if char == '\n' then n + 1 else n
            c = ai'column'number ai
            c' = if char == '\n' then 1 else c + 1
            (b : bs) = encode [char]
          in
            Just (b, AlexInput  { ai'prev = char
                                , ai'bytes = bs
                                , ai'rest = chars
                                , ai'line'number = n'
                                , ai'column'number = c' })


get'line'no :: Parser Int
get'line'no = do
  state <- get
  return . ai'line'number . input $ state


get'col'no :: Parser Int
get'col'no = do
  state <- get
  return . ai'column'number . input $ state


get'position :: Int -> Parser Position
get'position tok'len = do
  Parse'State { input = AlexInput { ai'line'number = ln, ai'column'number = cn } } <- get
  return $ Position { line = ln, column = cn - tok'len }


eval'parser :: Parser a -> String -> a
eval'parser parser source = evalState parser (initial'state source)


run'parser :: Parser a -> String -> (a, Parse'State)
run'parser parser source = runState parser (initial'state source)


use'parser :: Parser a -> String -> [a]
use'parser parser source = go [] $ runState parser (initial'state source)
  where
    -- go :: [a] -> (a, Parse'State) -> [a]
    go acc (token, p'state)
      = if finished
        then reverse acc
        else go (token : acc) $ runState parser p'state
      where
        finished = done p'state


-- Off-side rule: virtual braces and semicolons.
--
-- After a layout keyword (`let`, `where`, `of`), the next token either opens
-- an explicit brace block, or it opens an implicit block aligned at its own
-- column (a virtual '{' is emitted first). After a newline, a token aligned
-- with the current implicit block emits a virtual ';', an indented one is a
-- continuation, and a dedented one closes blocks (a virtual '}' per popped
-- block). End of input closes all implicit blocks. Explicit braces nest
-- inside implicit blocks and vice versa.
-- The grammar is unchanged: virtual tokens reuse the explicit token shapes.

-- |  Set by the module/declaration entry points: the first token opens
-- |  an implicit top-level block (unless it is `module` or `{`).
push'top'marker :: Parser ()
push'top'marker = do
  s <- get
  put s{ top'marker = True }


-- |  Set when a newline (or a line comment) is scanned.
mark'newline :: Lex'Action
mark'newline _ _ = do
  s <- get
  put s{ saw'newline = True }
  return Nothing


-- |  Decide what a scanned (or pushed back) token means for layout.
-- |  Returns the token to emit; the examined token is pushed back
-- |  whenever a virtual token is emitted instead of it.
examine'token :: Token -> Parser Token
examine'token tok = do
  s <- get
  if top'marker s
    then examine'top tok
    else if layout'expected s
      then examine'layout'kw tok
      else examine'ordinary tok


examine'top :: Token -> Parser Token
examine'top tok = do
  s <- get
  case tok of
    Tok'Module _ -> do
      put s{ top'marker = False, saw'newline = False }
      return tok
    Tok'Left'Brace _ -> do
      put s{ top'marker = False
           , saw'newline = False
           , layout'stack = Explicit : layout'stack s }
      return tok
    Tok'EOF _ -> do
      put s{ top'marker = False, saw'newline = False }
      emit'eof tok
    _ -> do
      let pos = at tok
      put s{ top'marker = False
           , saw'newline = False
           , layout'stack = Implicit (column pos) : layout'stack s
           , pending'tokens = tok : pending'tokens s }
      return (Tok'Left'Brace pos)


examine'layout'kw :: Token -> Parser Token
examine'layout'kw tok = do
  s <- get
  put s{ layout'expected = False }
  case tok of
    Tok'Left'Brace _ -> do
      push'context Explicit
      emit tok
    Tok'EOF _ ->
      -- an empty trailing `where`-block: nothing to open
      emit'eof tok
    _ -> do
      let pos = at tok
      s' <- get
      put s'{ layout'stack = Implicit (column pos) : layout'stack s'
            , pending'tokens = tok : pending'tokens s'
            , saw'newline = False }
      return (Tok'Left'Brace pos)


examine'ordinary :: Token -> Parser Token
examine'ordinary tok =
  case tok of
    Tok'EOF _ -> emit'eof tok
    -- An `in` closes the implicit blocks up to its `let`
    -- (as in `let a = 0 in a`, where no column rule can
    -- produce the '}'). Blocks belonging to an explicit
    -- inner `let` are already closed, so nothing is popped
    -- for them. Without an open `let`, plain handling applies.
    Tok'In _ -> close'let tok
    Tok'Let _ -> do
      push'context LetPending
      expect'layout
      emit tok
    Tok'Where _ -> do
      expect'layout
      emit tok
    Tok'Of _ -> do
      expect'layout
      emit tok
    Tok'Left'Brace _ -> do
      push'context Explicit
      emit tok
    Tok'Right'Brace _ -> do
      s <- get
      case layout'stack s of
        Explicit : _ -> do
          pop'context
          emit tok
        _ ->
          error $ "Parse error: unmatched '}' at " ++ show (at tok)
    _ ->
      examine'plain tok


-- An ordinary token: after a newline it is compared
-- against the current implicit block, otherwise emitted.
examine'plain :: Token -> Parser Token
examine'plain tok = do
  s <- get
  if saw'newline s
    then case layout'stack s of
      Implicit context : _ ->
        case compare (column (at tok)) context of
          EQ -> do
            pend tok
            clear'newline
            return (Tok'Semicolon (at tok))
          GT ->
            emit tok
          LT -> do
            pop'context
            pend tok
            -- keep the newline: the token is re-examined
            -- against the next outer context
            return (Tok'Right'Brace (at tok))
      _ ->
        emit tok
    else emit tok


-- |  At the end of input, close all implicit blocks
-- |  with virtual '}' before emitting the end of file.
emit'eof :: Token -> Parser Token
emit'eof tok = do
  s <- get
  case layout'stack s of
    Implicit _ : _ -> do
      pop'context
      return (Tok'Right'Brace (at tok))
    _ -> do
      put s{ layout'expected = False, top'marker = False, done = True }
      return tok


-- |  An `in` closes every implicit block up to its `let`
-- |  (emitting one virtual '}' per block), then passes through.
-- |  All of it is decided in this single call: the virtual tokens
-- |  are queued ahead and the `in` behind them, so nothing is
-- |  re-examined and no block is closed twice.
close'let :: Token -> Parser Token
close'let tok = do
  s <- get
  let (run, rest) = span is'implicit (layout'stack s)
  case rest of
    LetPending : rest' ->
      -- the `in` belongs to this `let`: close every implicit
      -- block up to it, then let the `in` itself through
      case map (const (Tok'Right'Brace (at tok))) run of
        [] -> do
          put s{ layout'stack = rest' }
          return tok
        close : closes -> do
          -- virtual tokens are queued ahead and emitted
          -- without examination; the `in` goes behind them
          put s{ layout'stack = rest'
               , pending'virtuals = pending'virtuals s ++ closes ++ [tok] }
          return close
    _ ->
      examine'plain tok
  where
    is'implicit (Implicit _) = True
    is'implicit _            = False


-- Emitting a real token consumes a pending newline.
emit :: Token -> Parser Token
emit tok = do
  clear'newline
  return tok


pend :: Token -> Parser ()
pend tok = do
  s <- get
  put s{ pending'tokens = tok : pending'tokens s }


expect'layout :: Parser ()
expect'layout = do
  s <- get
  put s{ layout'expected = True }


push'context :: Layout'Context -> Parser ()
push'context context = do
  s <- get
  put s{ layout'stack = context : layout'stack s }


pop'context :: Parser ()
pop'context = do
  s <- get
  case layout'stack s of
    _ : rest -> put s{ layout'stack = rest }
    [] -> return ()


clear'newline :: Parser ()
clear'newline = do
  s <- get
  put s{ saw'newline = False }
          
