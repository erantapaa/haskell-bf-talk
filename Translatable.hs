
module Translatable where

import Address
import Pair

class Translatable a where
  translate :: Int -> a -> a

instance Translatable R where
  translate n (R x) = R (x+n)

instance Translatable Pair where
  translate n (Pair x) = Pair (x+n)

