module Corecursive (class Corecursive, CoalgebraM, embed) where

import Prelude

type CoalgebraM m f a = a -> m (f a)

class Functor f <= Corecursive t f | t -> f where
  embed :: f t -> t
