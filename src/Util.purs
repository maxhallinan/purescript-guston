module Util (unwords, concatStrings) where

import Prelude
import Data.Foldable (class Foldable, foldl)

unwords :: Array String -> String
unwords xs = foldl go mempty xs
  where go x y = x <> " " <> y

concatStrings :: Array String -> String
concatStrings xs = foldl (<>) mempty xs
