module Util (unwords, concatStrings) where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Data.String as S

unwords :: forall t. Foldable t => t String -> String
unwords = foldl go mempty
  where go x y = x <> " " <> y

concatStrings :: forall t. Foldable t => t String -> String
concatStrings xs = foldl (<>) mempty xs
