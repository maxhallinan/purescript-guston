module Expr 
  ( Expr(..)
  , XExprF(..)
  , Info
  , Env
  , ExprF(..)
  , SpecialForm(..)
  ) where

import Prelude
import Data.Map as M
import Data.Newtype (class Newtype, unwrap)
import Mu (Mu)
import Util (concatStrings, unwords)

type Expr = Mu XExprF

data XExprF a = XExprF a Info

instance showXExprF :: Show a => Show (XExprF a) where
  show (XExprF x _) = show x

newtype Info = Info { offsetRange :: OffsetRange }

derive instance newtypeInfo :: Newtype Info _

type OffsetRange = { line :: Int, col :: Int } 

data ExprF a 
  = Sym String
  | SFrm SpecialForm
  | Fn (Env a) (Array a) a
  | Lst (Array a)

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
