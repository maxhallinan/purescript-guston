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
import Data.Traversable (sequence)
import Data.List (List(..), zipWith)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Mu as Mu
import Data.Newtype (class Newtype, unwrap)
import Expr as E
import Recursive (class Recursive, AlgebraM, cataM)

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

evalProgram :: forall t a. Recursive t (Compose E.ExprAnnF (E.ExprF a)) => List t -> Eval (List (Mu.Mu (E.ExprF a)))
evalProgram = sequence <<< map eval

eval :: forall t a. Recursive t (Compose E.ExprAnnF (E.ExprF a)) => t -> Eval (Mu.Mu (E.ExprF a))
eval = cataM evalAlg

evalAlg :: forall a. AlgebraM Eval (Compose E.ExprAnnF (E.ExprF a)) (Mu.Mu (E.ExprF a))
evalAlg = worker <<< unwrap where
  worker (E.ExprAnnF exprf ann) =
    case exprf of
      (E.Sym name) ->
        evalSym ann name
      (E.Lst (Cons (Mu.In (E.SFrm sfrm)) args)) ->
        evalSFrm sfrm ann args
      (E.Lst xs) ->
        evalLst ann xs
      _ ->
        throw Unknown ann

evalSym :: forall a. E.Ann -> String -> Eval (Mu.Mu (E.ExprF a))
evalSym ann name = do
  env <- getEnv
  case M.lookup name env of
    Just expr ->
      pure expr
    Nothing ->
      throw (UnknownVar name) ann

evalSFrm :: forall a. E.SFrm -> E.Ann -> List (Mu.Mu (E.ExprF a)) -> Eval (Mu.Mu (E.ExprF a))
evalSFrm _ ann Nil = throw NumArgs ann
evalSFrm E.First ann args = evalFirst ann args
evalSFrm E.Rest ann args = evalRest ann args
evalSFrm E.If ann args = evalIf ann args
evalSFrm E.Def ann args = evalDef ann args
evalSFrm E.Cons ann args = evalCons ann args
evalSFrm E.IsAtm ann args = evalIsAtm ann args
evalSFrm E.IsEq ann args = evalIsEq ann args
evalSFrm E.Quote ann args = evalQuote ann args
evalSFrm E.Lambda ann args = evalLambda ann args

evalFirst :: forall a. E.Ann -> List (Mu.Mu (E.ExprF a)) -> Eval (Mu.Mu (E.ExprF a))
evalFirst ann Nil = throw NumArgs ann
evalFirst ann (Cons h _) =
  case Mu.unroll h of
    E.Lst Nil ->
      throw LstLength ann
    E.Lst (Cons h _) ->
      pure h
    _ -> throw WrongTipe ann

evalRest :: forall a. E.Ann -> List (Mu.Mu (E.ExprF a)) -> Eval (Mu.Mu (E.ExprF a))
evalRest ann Nil = throw NumArgs ann
evalRest ann (Cons h _) =
  case Mu.unroll h of
    E.Lst Nil ->
      pure h
    E.Lst (Cons _ t) ->
      pure $ Mu.roll $ E.Lst t
    _ -> throw WrongTipe ann

evalIf :: forall a. E.Ann -> List (Mu.Mu (E.ExprF a)) -> Eval (Mu.Mu (E.ExprF a))
evalIf ann (Cons p (Cons e1 (Cons e2 _))) =
  case Mu.unroll p of
    E.Lst Nil -> pure e2
    _ -> pure e1
evalIf ann _ = throw NumArgs ann

evalDef :: forall a. E.Ann -> List (Mu.Mu (E.ExprF a)) -> Eval (Mu.Mu (E.ExprF a))
evalDef ann (Cons sym (Cons expr Nil)) =
  case Mu.unroll sym of
    E.Sym name -> do
      _ <- updateEnv name expr
      pure expr
    _ ->
      throw WrongTipe ann
evalDef ann _ = throw NumArgs ann

evalCons :: forall a. E.Ann -> List (Mu.Mu (E.ExprF a)) -> Eval (Mu.Mu (E.ExprF a))
evalCons ann (Cons h (Cons t Nil)) =
  case Mu.unroll t of
    (E.Lst t) ->
      pure $ Mu.roll $ E.Lst $ Cons h t
    _ ->
      throw WrongTipe ann
evalCons ann _ = throw NumArgs ann

evalIsAtm :: forall a. E.Ann -> List (Mu.Mu (E.ExprF a)) -> Eval (Mu.Mu (E.ExprF a))
evalIsAtm ann (Cons h Nil) =
  case Mu.unroll h of
    E.Sym _ ->
      pure $ Mu.roll $ E.Sym "t"
    E.Lst Nil ->
      pure $ Mu.roll $ E.Sym "t"
    _ ->
      pure $ Mu.roll $ E.Lst Nil
evalIsAtm ann _ = throw NumArgs ann

evalIsEq :: forall a. E.Ann -> List (Mu.Mu (E.ExprF a)) -> Eval (Mu.Mu (E.ExprF a))
evalIsEq ann (Cons e1 (Cons e2 Nil)) =
  case Tuple (Mu.unroll e1) (Mu.unroll e2) of
    Tuple (E.Sym name1) (E.Sym name2) ->
      if name1 == name2
      then pure $ Mu.roll $ E.Sym "t"
      else pure $ Mu.roll $ E.Lst Nil
    Tuple (E.Lst Nil) (E.Lst Nil) ->
      pure $ Mu.roll $ E.Sym "t"
    _ ->
      pure $ Mu.roll $ E.Lst Nil
evalIsEq ann _ = throw NumArgs ann

evalLambda :: forall a. E.Ann -> List (Mu.Mu (E.ExprF a)) -> Eval (Mu.Mu (E.ExprF a))
evalLambda ann (Cons params (Cons body Nil)) =
  case Mu.unroll params of
    E.Lst p -> do
      env <- getEnv
      pure $ Mu.roll $ E.Fn env p body
    _ ->
      throw WrongTipe ann
evalLambda ann _ = throw NumArgs ann

evalQuote :: forall a. E.Ann -> List (Mu.Mu (E.ExprF a)) -> Eval (Mu.Mu (E.ExprF a))
evalQuote ann (Cons e Nil) = pure e
evalQuote ann _ = throw NumArgs ann

updateEnv :: forall a. String -> Mu.Mu (E.ExprF a) -> Eval Unit
updateEnv key val = do
  EvalState evalState <- S.get
  S.put $ EvalState (evalState { env = M.insert key val evalState.env })

evalLst :: forall a. E.Ann -> List (Mu.Mu (E.ExprF a)) -> Eval (Mu.Mu (E.ExprF a))
evalLst ann (Cons x xs) = throw NotImplemented ann
{-
  case Mu.unroll x of
    E.Fn localEnv params body -> do
      globalEnv <- getEnv
      let env = localEnv <> globalEnv
      let expr = applyLambda params xs body
      evalInLocalEnv env expr
    _ ->
      throw NotFn ann
-}
evalLst ann Nil = throw NumArgs ann

{-
applyLambda :: forall a. List (Mu.Mu (E.ExprF a)) -> List (Mu.Mu (E.ExprF a)) -> Mu.Mu (E.ExprF a) -> Eval (Mu.Mu (E.ExprF a))
applyLambda params args body = do
  env <- getEnv
  let env' = M.fromFoldable (zipWith (\k v -> Tuple (toSymName (Mu.unroll k)) v) params args) <> env
  let evalState = EvalState { env: env' }
  EvalT $ mapExceptT (withStateT $ const evalState) body
  where toSymName E.Sym name = name

evalInLocalEnv :: forall a. E.Env a -> Eval (Mu.Mu (E.ExprF a)) -> Eval (Mu.Mu (E.ExprF a))
evalInLocalEnv env (EvalT expr) = do
  let localState = EvalState { env: env }
  EvalT $ mapExceptT (withStateT $ const localState) expr
-}
