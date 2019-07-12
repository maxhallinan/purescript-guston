module Expr
  ( Ann(..)
  , Env
  , Expr(..)
  , ExprAnnF(..)
  , ExprF(..)
  , Location
  , Position
  , SFrm(..)
  , ShowExpr(..)
  , mkExpr
  , unExpr
  , unExpr'
  , unExprAnnF
  ) where

import Prelude

import Control.Apply (apply, lift3)
import Data.Functor.Compose (Compose(..))
import Data.List (List)
import Data.Map as M
import Data.Newtype (class Newtype, unwrap)
import Data.Foldable (intercalate)
import Data.Traversable (class Foldable, class Traversable, traverse, traverseDefault, sequenceDefault, foldl, foldr, foldMap)
import Mu (Mu, roll, unroll)
import Util (concatStrings, unwords)
import Recursive (Algebra, cata)

newtype ShowExpr = ShowExpr Expr

type Expr = Mu (Compose ExprAnnF ExprF)

derive instance newtypeExpr :: Newtype ShowExpr _

instance showExpr :: Show ShowExpr where
  show = cata showExpr' <<< unwrap
    where
    showExpr' :: Algebra (Compose ExprAnnF ExprF) String
    showExpr' expr = 
      case unExprAnnF (unwrap expr) of
        Sym name ->
          name
        SFrm sfrm ->
          show sfrm
        Fn _ _ _ ->
          "<function>"
        Lst xs ->
          "(" <> intercalate " " xs <> ")"

data ExprAnnF a = ExprAnnF a Ann

derive instance functorExprAnnF :: Functor ExprAnnF

mkExpr :: ExprF Expr -> Ann -> Expr
mkExpr e a = roll (Compose (ExprAnnF e a))

unExpr :: Expr -> ExprF Expr
unExpr = unExprAnnF <<< unExpr'

unExpr' :: Expr -> ExprAnnF (ExprF Expr)
unExpr' = unwrap <<< unroll

unExprAnnF :: forall a. ExprAnnF a -> a
unExprAnnF (ExprAnnF x _) = x

instance foldableExprAnnF :: Foldable ExprAnnF where
  foldr f y (ExprAnnF x _) = f x y
  foldl f y (ExprAnnF x _) = f y x
  foldMap f (ExprAnnF x _) = f x

instance traversableExprAnnF :: Traversable ExprAnnF where
  traverse f (ExprAnnF a ann) = map (\b -> ExprAnnF b ann) $ f a
  sequence = sequenceDefault

{-
instance showExprAnnF :: Show a => Show (ExprAnnF a) where
  show (ExprAnnF x _) = show x
-}

newtype Ann = Ann { location :: Location }

derive instance newtypeInfo :: Newtype Ann _

type Location = { begin :: Position, end :: Position }

type Position = { line :: Int, column :: Int }

data ExprF a
  = Sym String
  | SFrm SFrm
  | Fn (Env a) (List a) a
  | Lst (List a)

derive instance functorExprF :: Functor ExprF

instance foldableExprF :: Foldable ExprF where
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

instance traversableExprF :: Traversable ExprF where
  traverse f (Sym name) = pure (Sym name)
  traverse f (SFrm sfrm) = pure (SFrm sfrm)
  traverse f (Fn env params body) = 
    lift3 (\e p b -> (Fn e p b)) env' params' body'
    where env' = traverse f env
          params' = traverse f params
          body' = f body
  traverse f (Lst xs) = apply (pure Lst) xs'
    where xs' = traverse f xs

  sequence = sequenceDefault

{-
instance showExprF :: Show a => Show (ExprF a) where
  show (Sym s) = s
  show (SFrm sform) = show sform
  show (Fn _ _ _) = "<function>"
  show (Lst xs) = concatStrings [ "(", unwords $ show <$> xs ,")"]
-}

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
