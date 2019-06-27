module Expr
  ( Ann(..)
  , Env
  , Expr(..)
  , ExprAnnF(..)
  , ExprF(..)
  , Location
  , Position
  , SFrm(..)
  ) where

import Prelude

import Data.Functor.Compose (Compose(..))
import Data.List (List)
import Data.Map as M
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Foldable, class Traversable, traverseDefault, sequenceDefault, foldl, foldr, foldMap)
import Mu (Mu)
import Util (concatStrings, unwords)

type Expr a = Mu (Compose ExprAnnF (ExprF a))

data ExprAnnF a = ExprAnnF a Ann

derive instance functorExprAnnF :: Functor ExprAnnF

instance foldableExprAnnF :: Foldable ExprAnnF where
  foldr f y (ExprAnnF x _) = f x y
  foldl f y (ExprAnnF x _) = f y x
  foldMap f (ExprAnnF x _) = f x

instance traversableExprAnnF :: Traversable ExprAnnF where
  traverse = traverseDefault
  sequence = sequenceDefault

instance showExprAnnF :: Show a => Show (ExprAnnF a) where
  show (ExprAnnF x _) = show x

newtype Ann = Ann { location :: Location }

derive instance newtypeInfo :: Newtype Ann _

type Location = { begin :: Position, end :: Position }

type Position = { line :: Int, column :: Int }

data ExprF a b
  = Sym String
  | SFrm SFrm
  | Fn (Env a) (List b) b
  | Lst (List b)

derive instance functorExprF :: Functor (ExprF a)

instance foldableExprF :: Foldable (ExprF a) where
  foldr f y (Sym _) = y
  foldr f y (SFrm _) = y
  foldr f y (Fn env params body) = f body (foldr f y params)
  foldr f y (Lst xs) = foldr f y xs

  foldl f y (Sym _) = y
  foldl f y (SFrm _) = y
  foldl f y (Fn env params body) = f (foldl f y params) body
  foldl f y (Lst xs) = foldl f y xs

  foldMap f (Sym _) = mempty
  foldMap f (SFrm _) = mempty
  foldMap f (Fn _ _ _) = mempty
  foldMap f (Lst xs) = foldMap f xs

instance traversableExprF :: Traversable (ExprF a) where
  traverse = traverseDefault
  sequence = sequenceDefault

instance showExprF :: Show b => Show (ExprF a b) where
  show (Sym s) = s
  show (SFrm sform) = show sform
  show (Fn _ _ _) = "<function>"
  show (Lst xs) = concatStrings [ "(", unwords $ show <$> xs ,")"]

data SFrm
  = First
  | Rest
  | Cons
  | If
  | Def
  | IsAtm
  | IsEq
  | Lambda
  | Quote

instance showSFrm :: Show SFrm where
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
