module Bull.Message.Inventory
  ( Inventory(..)
  ) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)

data Inventory = Inventory
  { inventoryType :: InventoryType
  , inventoryHash :: ByteString
  }
  deriving (Eq, Read, Show)

instance Binary Inventory where
  get = getInventory
  put = putInventory

getInventory :: Get Inventory
getInventory =
  Inventory
    <$> get
    <*> getLazyByteString 32

putInventory :: Inventory -> Put
putInventory (Inventory t h) = do
  put t
  putLazyByteString h

data InventoryType
  = MsgTx
  | MsgBlock
  | MsgFilteredBlock
  | MsgCmpctBlock
  deriving (Eq, Read, Show)

instance Binary InventoryType where
  get = getInventoryType
  put = putInventoryType

getInventoryType :: Get InventoryType
getInventoryType = do
  w <- getWord32le
  case w of
    1 -> return MsgTx
    2 -> return MsgBlock
    3 -> return MsgFilteredBlock
    4 -> return MsgCmpctBlock
    _ -> fail "invalid inventory type"

putInventoryType :: InventoryType -> Put
putInventoryType t = putWord32le $ case t of
  MsgTx            -> 1
  MsgBlock         -> 2
  MsgFilteredBlock -> 3
  MsgCmpctBlock    -> 4
