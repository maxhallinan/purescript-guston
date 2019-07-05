module Eval where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT, mapExceptT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State as S
import Control.Monad.State.Trans (StateT, withStateT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity)
import Data.Tuple (Tuple(..))
import Data.Traversable (class Traversable, sequence, traverse)
import Data.List (List(..), zipWith)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Mu as Mu
import Data.Newtype (class Newtype, unwrap)
import Expr as E
import Recursive (class Recursive, AlgebraM, cataM)
import Corecursive (class Corecursive, CoalgebraM)

type Eval a = EvalT Identity a

newtype EvalT m a = EvalT (ExceptT EvalErr (StateT (EvalState a) m) a)

derive instance newtypeEvalT :: Newtype (EvalT m a) _
derive newtype instance functorEvalT :: Functor m => Functor (EvalT m)
derive newtype instance applyEvalT :: Monad m => Apply (EvalT m)
derive newtype instance applicativeEvalT :: Monad m => Applicative (EvalT m)
derive newtype instance bindEvalT :: Monad m => Bind (EvalT m)
derive newtype instance monadParserT :: Monad m => Monad (EvalT m)
derive newtype instance monadRecParserT :: MonadRec m => MonadRec (EvalT m)
derive newtype instance monadStateParserT :: Monad m => MonadState (EvalState a) (EvalT m)
derive newtype instance monadErrorParserT :: Monad m => MonadError EvalErr (EvalT m)
derive newtype instance monadThrowParserT :: Monad m => MonadThrow EvalErr (EvalT m)

data EvalErr = EvalErr ErrName E.Location

throw :: forall m a. Monad m => ErrName -> E.Ann -> EvalT m a
throw name ann = throwError $ EvalErr name location
  where location = _.location $ unwrap ann

newtype EvalState a = EvalState { env :: E.Env a }

derive instance evalStateNewtype :: Newtype (EvalState a) _

getEnv :: forall m a. Monad m => EvalT m (E.Env a)
getEnv = S.get >>= (unwrap >>> _.env >>> pure)

data ErrName
  = NumArgs
  | WrongTipe
  | LstLength
  | UnknownVar String
  | NotFn
  | Unknown
  | NotPair
  | NotImplemented

evalProgram :: List E.Expr -> Eval (List E.Expr)
evalProgram = sequence <<< map eval

eval :: E.Expr -> Eval E.Expr
eval = cataM evalAlg

evalAlg :: AlgebraM Eval (Compose E.ExprAnnF E.ExprF) E.Expr
evalAlg = worker <<< unwrap where
  worker (E.ExprAnnF exprf ann) =
    case exprf of
      (E.Sym name) ->
        evalSym ann name
      (E.Lst (Cons (Mu.In (Compose (E.ExprAnnF (E.SFrm sfrm) _))) args)) ->
        evalSFrm sfrm ann args
      (E.Lst xs) ->
        evalLst ann xs
      _ ->
        throw Unknown ann

evalSym :: E.Ann -> String -> Eval E.Expr
evalSym ann name = do
  env <- getEnv
  case M.lookup name env of
    Just expr ->
      pure expr
    Nothing ->
      throw (UnknownVar name) ann

evalSFrm :: E.SFrm -> E.Ann -> List E.Expr -> Eval E.Expr
evalSFrm _ ann Nil = throw NumArgs ann
evalSFrm E.First ann args = evalFirst ann args
evalSFrm _ ann _ = throw NumArgs ann
evalSFrm E.Rest ann args = evalRest ann args
evalSFrm E.If ann args = evalIf ann args
evalSFrm E.Def ann args = evalDef ann args
evalSFrm E.Cons ann args = evalCons ann args
evalSFrm E.IsAtm ann args = evalIsAtm ann args
evalSFrm E.IsEq ann args = evalIsEq ann args
evalSFrm E.Quote ann args = evalQuote ann args
evalSFrm E.Lambda ann args = evalLambda ann args

evalFirst :: E.Ann -> List E.Expr -> Eval E.Expr
evalFirst ann Nil = throw NumArgs ann
evalFirst ann (Cons h _) =
  case E.unExpr h of
    E.Lst Nil ->
      throw LstLength ann
    E.Lst (Cons h _) ->
      pure $ h
    _ -> throw WrongTipe ann

evalRest :: E.Ann -> List E.Expr -> Eval E.Expr
evalRest ann Nil = throw NumArgs ann
evalRest ann (Cons h _) =
  case E.unExpr h of
    E.Lst Nil ->
      pure h
    E.Lst (Cons _ t) ->
      pure $ E.mkExpr (E.Lst t) ann
    _ -> throw WrongTipe ann

evalIf :: E.Ann -> List E.Expr -> Eval E.Expr
evalIf ann (Cons p (Cons e1 (Cons e2 _))) =
  case E.unExpr p of
    E.Lst Nil -> pure e2
    _ -> pure e1
evalIf ann _ = throw NumArgs ann

evalDef :: E.Ann -> List E.Expr -> Eval E.Expr
evalDef ann (Cons sym (Cons expr Nil)) =
  case E.unExpr sym of
    E.Sym name -> do
      _ <- updateEnv name expr
      pure expr
    _ ->
      throw WrongTipe ann
evalDef ann _ = throw NumArgs ann

evalCons :: E.Ann -> List E.Expr -> Eval E.Expr
evalCons ann (Cons h (Cons t Nil)) =
  case E.unExpr t of
    (E.Lst t) ->
      pure $ E.mkExpr (E.Lst (Cons h t)) ann
    _ ->
      throw WrongTipe ann
evalCons ann _ = throw NumArgs ann

evalIsAtm :: E.Ann -> List E.Expr -> Eval E.Expr
evalIsAtm ann (Cons h Nil) =
  case E.unExpr h of
    E.Sym _ ->
      pure $ E.mkExpr (E.Sym "t") ann
    E.Lst Nil ->
      pure $ E.mkExpr (E.Sym "t") ann
    _ ->
      pure $ E.mkExpr (E.Lst Nil) ann
evalIsAtm ann _ = throw NumArgs ann

evalIsEq :: E.Ann -> List E.Expr -> Eval E.Expr
evalIsEq ann (Cons e1 (Cons e2 Nil)) =
  case Tuple (E.unExpr e1) (E.unExpr e2) of
    Tuple (E.Sym name1) (E.Sym name2) ->
      if name1 == name2
      then pure $ E.mkExpr (E.Sym "t") ann
      else pure $ E.mkExpr (E.Lst Nil) ann
    Tuple (E.Lst Nil) (E.Lst Nil) ->
      pure $ E.mkExpr (E.Sym "t") ann
    _ ->
      pure $ E.mkExpr (E.Lst Nil) ann
evalIsEq ann _ = throw NumArgs ann

evalLambda :: E.Ann -> List E.Expr -> Eval E.Expr
evalLambda ann (Cons params (Cons body Nil)) =
  case E.unExpr params of
    E.Lst p -> do
      env <- getEnv
      pure $ E.mkExpr (E.Fn env p body) ann
    _ ->
      throw WrongTipe ann
evalLambda ann _ = throw NumArgs ann

evalQuote :: forall a. E.Ann -> List E.Expr -> Eval E.Expr
evalQuote ann (Cons e Nil) = pure e
evalQuote ann _ = throw NumArgs ann

updateEnv :: String -> E.Expr -> Eval Unit
updateEnv key val = do
  EvalState evalState <- S.get
  S.put $ EvalState (evalState { env = M.insert key val evalState.env })

evalLst :: E.Ann -> List E.Expr -> Eval E.Expr
evalLst ann (Cons x xs) =
  case E.unExpr x of
    E.Fn localEnv params body -> do
      globalEnv <- getEnv
      let env = localEnv <> globalEnv
      let expr = applyLambda ann params xs body
      evalInLocalEnv env expr
    _ ->
      throw NotFn ann
evalLst ann Nil = throw NumArgs ann

applyLambda :: E.Ann -> List E.Expr -> List E.Expr -> E.Expr -> Eval E.Expr
applyLambda ann params args body = do
  env <- bindArgs ann params args
  let evalState = EvalState { env: env }
  EvalT $ mapExceptT (withStateT $ const evalState) (pure body)

bindArgs :: E.Ann -> List E.Expr -> List E.Expr -> Eval (E.Env E.Expr)
bindArgs ann params args = do
  env <- getEnv
  bindings <- sequence (zipWith toBinding params args)
  pure $ M.fromFoldable bindings <> env
  where
    toBinding :: E.Expr -> E.Expr -> Eval (Tuple String E.Expr)
    toBinding param arg = do
      paramName <- toParamName param
      pure $ Tuple paramName arg
    toParamName :: E.Expr -> Eval String
    toParamName param =
      case E.unExpr' param of
        E.ExprAnnF (E.Sym name) _ ->
          pure name
        E.ExprAnnF _ ann ->
          throw Unknown ann

evalInLocalEnv :: E.Env E.Expr -> Eval E.Expr -> Eval E.Expr
evalInLocalEnv env (EvalT expr) = do
  let localState = EvalState { env: env }
  EvalT $ mapExceptT (withStateT $ const localState) expr
