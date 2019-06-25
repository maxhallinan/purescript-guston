module Expr
  ( Ann(..)
  , Env
  , Expr(..)
  , ExprAnnF(..)
  , ExprF(..)
  , Location
  , Position
  , SpecialForm(..)
  ) where

import Prelude
import Data.Functor.Compose (Compose(..))
import Data.Map as M
import Data.Newtype (class Newtype, unwrap)
import Mu (Mu)
import Util (concatStrings, unwords)

type Expr = Mu (Compose ExprAnnF ExprF)

data ExprAnnF a = ExprAnnF a Ann

derive instance functorExprAnnF :: Functor ExprAnnF

instance showExprAnnF :: Show a => Show (ExprAnnF a) where
  show (ExprAnnF x _) = show x

newtype Ann = Ann { location :: Location }

derive instance newtypeInfo :: Newtype Ann _

type Location = { begin :: Position, end :: Position }

type Position = { line :: Int, column :: Int }

data ExprF a
  = Sym String
  | SFrm SpecialForm
  | Fn (Env a) (Array a) a
  | Lst (Array a)

derive instance functorExprF :: Functor ExprF

instance showExprF :: Show a => Show (ExprF a) where
  show (Sym s) = s
  show (SFrm sform) = show sform
  show (Fn _ _ _) = "<function>"
  show (Lst xs) = concatStrings [ "(", unwords $ show <$> xs ,")"]

data SpecialForm
  = First
  | Rest
  | Cons
  | If
  | Def
  | IsAtm
  | IsEq
  | Lambda
  | Quote

instance showSpecialForm :: Show SpecialForm where
  show First = "first"
  show Rest = "rest"
  show Cons = "::"
  show If = "if"
  show Def = "def"
  show IsAtm = "atom?"
  show IsEq = "=="
  show Lambda = "fn"
  show Quote = "quote"

type Env a = M.Map String a
