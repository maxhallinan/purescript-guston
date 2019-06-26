module Corecursive (class Corecursive, embed) where

import Prelude

class Functor f <= Corecursive t f | t -> f where
  embed :: f t -> t
