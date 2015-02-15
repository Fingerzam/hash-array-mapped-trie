{-# LANGUAGE QuasiQuotes, DataKinds, ScopedTypeVariables #-}
import Data.Word (Word32)
import Data.Array.IArray (Array, array, (!))
import Data.Bits ((.&.), shift, popCount, testBit)
import Record
import Record.Lens

data Word32Map value = Split {childBitMask :: Word32,
                              children :: Array Word32 (Word32Map value)}
                       |
                       Leaf {valueBitMask :: Word32,
                             values :: Array Word32 value }

emptyMap32 = Split 0 (array (0, 31) [])

get :: Word32Map value -> Word32 -> Maybe value
get map32 index = getHelper map32 0 index

getHelper :: Word32Map v -> Int -> Word32 -> Maybe v
getHelper map32@(Split {}) j index | 0 <= j && j < 5 =
  do (childIndex :: Word32) <- (getChildIndex (childBitMask map32) (getIndex j index) :: Maybe Word32)
     getHelper ((children map32) ! childIndex) (j+1) index
getHelper map32@(Leaf {}) 5 index =
  do childIndex <- getChildIndex (valueBitMask map32)
                                 (getIndex 6 index)
     return $ (values map32) ! childIndex

getChildIndex :: Word32 -> Word32 -> Maybe Word32
getChildIndex childbitmask index
  | 0 <= index && index < 32 = if testBit childbitmask (fromIntegral index) == True
                               then Just childIndex
                               else Nothing
  where mask = (2^(index+1)) - 1 :: Word32
        masked = childbitmask .&. mask :: Word32
        childIndex = fromIntegral $ (popCount masked) - 1 :: Word32

getIndex :: Int -> Word32 -> Word32
getIndex j index | 0 <= j && j < 7 = masked
  where mask = (2^5 - 1)
        shifted = (index`shift` (-5*j))
        masked = (shifted .&. mask)
