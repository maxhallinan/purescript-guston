module Mu (Mu(..), roll, unroll) where

import Prelude

newtype Mu f = In (f (Mu f))

instance showMu :: (Show (f String), Functor f) => Show (Mu f) where
  show (In x) = show $ show <$> x

roll :: forall f. f (Mu f) -> Mu f
roll x = In x

unroll :: forall f. Mu f -> f (Mu f)
unroll (In x) = x
