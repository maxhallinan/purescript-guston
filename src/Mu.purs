module Mu (Mu(..), roll, unroll) where

import Prelude

import Corecursive (class Corecursive)
import Recursive (class Recursive)

newtype Mu f = In (f (Mu f))

instance showMu :: (Show (f String), Functor f) => Show (Mu f) where
  show (In x) = show $ show <$> x

roll :: forall f. f (Mu f) -> Mu f
roll = In

unroll :: forall f. Mu f -> f (Mu f)
unroll (In x) = x

instance recursiveMu :: Functor f => Recursive (Mu f) f where
  project = unroll

instance corecursiveMu :: Functor f => Corecursive (Mu f) f where
  embed = roll
