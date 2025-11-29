module Bull.Message.Inv
  ( Inv(..)
  ) where

import Bull.Message.CompactSize
import Bull.Message.Inventory
import Control.Monad
import Data.Binary
import Prettyprinter

data Inv = Inv
  { invCount     :: Integer -- ^ compact size
  , invInventory :: [Inventory]
  }
  deriving (Eq, Read, Show)

instance Pretty Inv where
  pretty i = vsep
    [ pretty "inv:"
    , indent 2 $ vsep
      [ pretty "count:" <+> pretty (invCount i)
      , pretty "inventory:"
      , indent 2 $ vsep $ pretty <$> invInventory i
      ]
    ]

instance Binary Inv where
  get = do
    sz <- getCompactSize
    Inv sz <$> replicateM (fromIntegral sz) get
  put i = do
    putCompactSize $ invCount i
    mapM_ put $ invInventory i
