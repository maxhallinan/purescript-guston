module Recursive (class Recursive, Algebra, AlgebraM, Attr(..), CVAlgebra, GAlgebraM, paraM, cata, cataM, histo, project, topDownCataM) where

import Prelude

import Corecursive (class Corecursive, embed)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))
import Data.Newtype (class Newtype, unwrap)

class Functor f <= Recursive t f | t -> f where
  project :: t -> f t

type Algebra f a = f a -> a

cata :: forall t f a. Recursive t f => Algebra f a -> t -> a
cata f = go
  where go x = f $ map go $ project x

type AlgebraM m f a = f a -> m a

cataM
  :: forall t f m a
  . Recursive t f
  => Monad m
  => Traversable f
  => AlgebraM m f a
  -> t
  -> m a
cataM f = go
 where go t = f =<< traverse go (project t)

topDownCataM
  ∷ forall t f m a
  . Recursive t f
  => Corecursive t f
  => Monad m
  => Traversable f
  => (a -> t -> m (Tuple a t))
  -> a
  -> t
  -> m t
topDownCataM f = go
  where
  go a t = f a t >>= case _ of
    Tuple a' tf -> traverseR (traverse (go a')) tf

traverseR
  ∷ forall t f u g m
  . Recursive t f
  => Corecursive u g
  => Functor m
  => (f t -> m (g u))
  -> t
  -> m u
traverseR f = map embed <<< f <<< project

newtype Attr f a = Attr { attribute :: a, hole :: f (Attr f a) }

derive instance newtypeAttr :: Newtype (Attr f a) _

type CVAlgebra f a = f (Attr f a) -> a

histo :: forall t f a. Recursive t f => CVAlgebra f a -> t -> a
histo f = worker >>> getAttribute
  where worker = project >>> map (\x -> worker x) >>> mkAttr
        getAttribute = unwrap >>> _.attribute
        mkAttr x = Attr { attribute: f x, hole: x }

type GAlgebraM w m f a = f (w a) -> m a

paraM
  :: forall t f m a
  .  Recursive t f
  => Monad m
  => Traversable f
  => GAlgebraM (Tuple t) m f a
  -> t
  -> m a
paraM f = go
  where
  go t = f =<< traverse (map (Tuple t) <<< go) (project t)
