module Recursive (class Recursive, project) where

import Prelude

import Data.Newtype (class Newtype, unwrap)

class Functor f <= Recursive t f | t -> f where
  project :: t -> f t

type Algebra f a = f a -> a

cata :: forall t f a. Recursive t f => Algebra f a -> t -> a
cata f = go
  where go x = f $ map go $ project x

newtype Attr f a = Attr { attribute :: a, hole :: f (Attr f a) }

derive instance newtypeAttr :: Newtype (Attr f a) _

type CVAlgebra f a = f (Attr f a) -> a

histo :: forall t f a. Recursive t f => CVAlgebra f a -> t -> a 
histo f = worker >>> getAttribute
  where worker = project >>> map (\x -> worker x) >>> mkAttr
        getAttribute = unwrap >>> _.attribute
        mkAttr x = Attr { attribute: f x, hole: x }
