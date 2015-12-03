{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Hell (Hell(..)) where

import GHC.Generics (Generic)
import Control.DeepSeq

data Hell a = Hell a String
              deriving (Eq, Generic, NFData)
