{-# LANGUAGE ScopedTypeVariables #-}
import Debug.Trace
import Data.Word (Word32)
import Data.Array.IArray (Array, array, (!), elems, (//), listArray, indices, assocs, bounds)
import Data.List (genericLength)
import Data.Bits ((.&.), shift, popCount, testBit, setBit, bit, clearBit)

data Word32Map value = Split {childBitMask :: Word32,
                              children :: Array Word32 (Word32Map value)}
                       |
                       Leaf {valueBitMask :: Word32,
                             values :: Array Word32 value } deriving (Show)

emptyMap32 = Split zeroBits (listArray (1, 0) [])

get :: Word32Map value -> Word32 -> Maybe value
get map32 index = getHelper map32 0 index

getHelper :: Word32Map v -> Int -> Word32 -> Maybe v
getHelper map32@(Split {}) j index | 0 <= j && j < 6 =
  do childIndex <- getChildIndex (childBitMask map32) (getIndex j index)
     let child = ((children map32) ! childIndex)
     getHelper child (j+1) index
getHelper map32@(Leaf {}) 6 index =
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
        childIndex = fromIntegral $ (popCount masked) - 1

getIndex :: Int -> Word32 -> Word32
getIndex j index | 0 <= j && j < 7 = masked
  where mask = (2^5 - 1)
        shifted = (index `shift` (-5*j))
        masked = (shifted .&. mask)

set :: (Show value) => Word32Map value -> Word32 -> value -> Word32Map value
set map32 index value = setHelper map32 index 0 value

setHelper :: (Show value) =>  Word32Map value -> Word32 -> Int -> value -> Word32Map value
setHelper map32@(Split {}) index level value
  | hasChildAtIndex map32 index level
    = Split newBitMask $
            setValue (children map32) childIndex $
                     setHelper ((children map32) !  currentIndex)
                               index
                               (level + 1)
                               value
  | otherwise
    = Split newBitMask $
            addValue (children map32) childIndex newBitMask $
                     setHelper (emptyMapForLevel (level + 1))
                               index
                               (level+1)
                               value
  where newBitMask = setIndex (childBitMask map32) level index
        currentIndex = (getIndex level index)
        childIndex = (extractMaybe (getChildIndex newBitMask currentIndex))
setHelper map32@(Leaf{}) index 6 value
  | hasChildAtIndex map32 index 6
    = Leaf newBitMask $
           setValue (values map32) childIndex value
  | otherwise
    = Leaf newBitMask $
           addValue (values map32) childIndex newBitMask value
  where newBitMask = (setIndex (valueBitMask map32) 6 index)
        childIndex = (extractMaybe (getChildIndex newBitMask (getIndex 6 index)))

extractMaybe (Just x) = x

hasChildAtIndex :: Word32Map value -> Word32 -> Int -> Bool
hasChildAtIndex map32 index level = testBit (bitMask map32) (fromIntegral (getIndex level index))

bitMask :: Word32Map value -> Word32
bitMask map32@(Split {}) = childBitMask map32
bitMask map32@(Leaf{}) = valueBitMask map32

emptyMapForLevel n | 0 <= n && n < 6 = Split zeroBits (listArray (1,0) [])
                   | n == 6          = Leaf zeroBits (listArray (1,0) [])

zeroBits = clearBit (bit 0) 0

setIndex :: Word32 -> Int -> Word32 -> Word32
setIndex    old       level  index = setBit old (fromIntegral (getIndex level index))

setValue arr ind elem = arr // [(ind, elem)]

addValue arr ind mask elem = listArray (0, genericLength elems') newElems
  where elems' = elems arr
        before = take (fromIntegral ind) elems'
        after  = drop (fromIntegral ind) elems'
        newElems = before ++ [elem] ++ after
