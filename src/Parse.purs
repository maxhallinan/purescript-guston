module Parse where

import Prelude

import Control.Alt ((<|>))
import Data.Array (many)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Array (cons)
import Data.Functor.Compose (Compose(..))
import Expr as E
import Mu as M
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.Language as L
import Text.Parsing.Parser.String as S
import Text.Parsing.Parser.Token as T
import Text.Parsing.Parser.Pos (Position(..))
import Util (words)

type Parser a = P.Parser String a

langDef :: T.LanguageDef
langDef = T.LanguageDef (T.unGenLanguageDef L.emptyDef)
  { commentLine = ";"
  , identLetter = identLetter
  , identStart = identStart
  }

lexer :: T.TokenParser
lexer = T.makeTokenParser langDef

identStart :: Parser Char
identStart = T.letter <|> oneOf "!$%&*/:<=>?~_^"

identLetter :: Parser Char
identLetter = identStart <|> T.digit <|> oneOf ".+-"

annotate :: forall a. Parser a -> Parser (E.ExprAnnF a)
annotate p = do
  begin <- location
  result <- p
  end <- location
  let annotation = E.Ann $ { location: { begin: begin, end: end } }
  pure $ E.ExprAnnF result annotation
  where location = P.position >>= (pure <<< unwrapPos)
        unwrapPos (Position loc) = loc

expr :: Parser (E.ExprF E.Expr) -> Parser E.Expr
expr = map (M.roll <<< Compose) <<< annotate

symbol :: forall a. Parser (E.ExprF a)
symbol = do
  identifier <- lexer.identifier
  case identifier of
    "::" ->
      pure $ E.SFrm E.Cons
    "=" ->
      pure $ E.SFrm E.Def
    "==" ->
      pure $ E.SFrm E.IsEq
    "atom?" ->
      pure $ E.SFrm E.IsAtm
    "first" ->
      pure $ E.SFrm E.First
    "fn" ->
      pure $ E.SFrm E.Lambda
    "if" ->
      pure $ E.SFrm E.If
    "quote" ->
      pure $ E.SFrm E.Quote
    "rest" ->
      pure $ E.SFrm E.Rest
    _ ->
      pure $ E.Sym identifier

oneOf :: String -> Parser Char
oneOf = S.oneOf <<< toCharArray
