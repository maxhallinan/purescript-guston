module Recursive (class Recursive, Algebra, AlgebraM, Attr, CVAlgebra, cata, cataM, histo, project) where

import Prelude

import Data.Traversable (class Traversable, traverse)
import Data.Newtype (class Newtype, unwrap)

class Functor f <= Recursive t f | t -> f where
  project :: t -> f t

type Algebra f a = f a -> a

cata :: forall t f a. Recursive t f => Algebra f a -> t -> a
cata f = go
  where go x = f $ map go $ project x

type AlgebraM m f a = f a -> m a

cataM :: forall t f m a. Recursive t f => Monad m => Traversable f => AlgebraM m f a -> t -> m a 
cataM f = go
 where go t = f =<< traverse go (project t)

newtype Attr f a = Attr { attribute :: a, hole :: f (Attr f a) }

derive instance newtypeAttr :: Newtype (Attr f a) _

type CVAlgebra f a = f (Attr f a) -> a

histo :: forall t f a. Recursive t f => CVAlgebra f a -> t -> a 
histo f = worker >>> getAttribute
  where worker = project >>> map (\x -> worker x) >>> mkAttr
        getAttribute = unwrap >>> _.attribute
        mkAttr x = Attr { attribute: f x, hole: x }
