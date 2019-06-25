module Util (words, unwords, concatStrings) where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Data.String as S

unwords :: Array String -> String
unwords = foldl go mempty
  where go x y = x <> " " <> y

words :: String -> Array String
words = S.split (S.Pattern " ")

concatStrings :: Array String -> String
concatStrings xs = foldl (<>) mempty xs
