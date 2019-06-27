module Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either)
import Data.List (List(..), manyRec)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Functor.Compose (Compose(..))
import Expr as E
import Mu as M
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.Language as L
import Text.Parsing.Parser.String as S
import Text.Parsing.Parser.Token as T
import Text.Parsing.Parser.Pos (Position(..))

type Parser a = P.Parser String a

parseStr :: forall a. String -> Either P.ParseError (List (E.Expr a))
parseStr = flip P.runParser program

program :: forall a. Parser (List (E.Expr a))
program = C.between lexer.whiteSpace S.eof exprs
  where exprs = C.sepBy expr lexer.whiteSpace

expr :: forall a. Parser (E.Expr a)
expr = fix $ \p -> symbol <|> quoted p <|> listOf p

listOf :: forall a. Parser (E.Expr a) -> Parser (E.Expr a)
listOf p = exprOf $ E.Lst <$> lexer.parens (manyRec p)

quoted :: forall a. Parser (E.Expr a) -> Parser (E.Expr a)
quoted p = exprOf $ do
  _ <- S.string "'"
  quote <- exprOf $ pure (E.SFrm E.Quote)
  x <- p
  pure $ E.Lst (Cons quote (pure x))

symbol :: forall a. Parser (E.Expr a)
symbol = exprOf $ do
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

exprOf :: forall a. Parser (E.ExprF a (E.Expr a)) -> Parser (E.Expr a)
exprOf = map (M.roll <<< Compose) <<< annotate

annotate :: forall a. Parser a -> Parser (E.ExprAnnF a)
annotate p = do
  begin <- location
  result <- p
  end <- location
  let annotation = E.Ann $ { location: { begin: begin, end: end } }
  pure $ E.ExprAnnF result annotation
  where location = P.position >>= (pure <<< unwrapPos)
        unwrapPos (Position loc) = loc

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

oneOf :: String -> Parser Char
oneOf = S.oneOf <<< toCharArray
