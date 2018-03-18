{- |
Module      : VTS.Base.Instance
Description : The instance of various type class
Copyright   : (C) 2018 Johann Lee
License     : GPL-3
Maintainer  : me@qinka.pro
Stability   : experimental
Portability : unknown

The instance of various type class.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Unsafe                   #-}

module VTS.Base.Instance
  () where

import           Control.Monad         (replicateM, when)
import           Data.Binary           (Binary (..), Get)
import           Data.Word             (Word16, Word8)
import           Foreign.C.Types       (CSize (..))
import           Foreign.ForeignPtr    (ForeignPtr, mallocForeignPtrArray,
                                        withForeignPtr)
import           Foreign.Marshal.Array (peekArray, pokeArray)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (Storable (..))
import           System.IO.Unsafe      (unsafePerformIO)
import           VTS.Base.Internal

-- | foregin functions for pointers

foreign import ccall "memcmp" c_memcmp :: Ptr a -> Ptr a -> CSize -> IO Int

-- | internal functions

compareVTS :: (Storable a, Ord a)
           => VoxelTensor a
           -> VoxelTensor a
           -> Ordering
compareVTS (VTS d1 w1 h1 fp1) (VTS d2 w2 h2 fp2) = unsafePerformIO $ do
  cmp <- withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 -> do
    x1 <- peekArray len1 p1
    x2 <- peekArray len2 p2
    return $ x1 `compare` x2
  return $ case cmp of
    EQ -> len1 `compare` len2
    x  -> x
  where len1 = d1 * w1 * h1
        len2 = d2 * w2 * h2

-- | instance of Eq
instance (Storable a, Ord a) => Eq (VoxelTensor a) where
  (==) v1@(VTS d1 w1 h1 fp1) v2@(VTS d2 w2 h2 fp2) =
    d1 == d2 && w1 == w2 && h1 == h2 && (fp1 == fp2 || compareVTS v1 v2 == EQ)

-- | instance of Ord
instance (Storable a, Ord a) => Ord (VoxelTensor a) where
  compare = compareVTS

-- | instance show
instance (Show a, Storable a) => Show (VoxelTensor a) where
  show (VTS d w h fp) =
    unwords [ "size:"
            , show d
            , show w
            , show h
            , "["
            , list
            , "]"
            ]
    where list = unwords $ unsafePerformIO $ withForeignPtr fp $ \p ->
            map show <$> peekArray (d*w*h) p

-- | type infos will hold the info of type, the first paramater should not be use
-- because it might always be @undefined@, and the it will be stored in big endian.
class Typeinfos t where
  toTypeMagic :: t -> Word16

instance Typeinfos Float where
  toTypeMagic _ = 0x6604

instance Typeinfos Int where
  toTypeMagic _ = 0x6908 -- on 64 cpu

instance Typeinfos Word8 where
  toTypeMagic _ = 0x6501

-- | instance binary's Get and Put
-- type id : xb(e,g, f4, i4) "x" for the type, "b" for bytes,
-- For example, i4 means int with 4 bytes, w8 unsigned int with 8 bytes, f12,
-- float with 12 bytes(long double)
--
-- the magic number of vts is 0x92 0x19 0x24
instance (Storable a, Binary a, Typeinfos a) => Binary (VoxelTensor a) where
  put (VTS d w h fp) = do
    -- write magic number
    put ((0x92,0x19,0x24)::(Word8,Word8,Word8))
    -- write type info
    put $ typeInfo undefined fp
    -- write the depth, width, height
    put (d,w,h)
    -- write the data
    let list = unsafePerformIO $ withForeignPtr fp $ \p -> peekArray (d * w * h) p
    mapM_ put list
    where typeInfo :: Typeinfos a => a -> ForeignPtr a -> Word16
          typeInfo dummy _ = toTypeMagic dummy
          size :: Storable a => ForeignPtr a -> a -> Int
          size _ dummy = sizeOf dummy
  get = do
    _ <- getVTXTI
    (d,w,h) <- get
    es <- replicateM (d * w * h) get
    let fp = unsafePerformIO $ do
          fp <- mallocForeignPtrArray $ d * w * h
          withForeignPtr fp $ \p -> pokeArray p es
          return fp
    return $ VTS d w h fp

getVTXTI :: Get Word16
getVTXTI = getCheckMaginNumber >> get

getCheckMaginNumber :: Get()
getCheckMaginNumber = do
  (m,g,n) <- get :: Get (Word8,Word8,Word8)
  when (m /= 0x92 || g /= 0x19 || n /= 0x24) $ fail "error magic number"
  return ()
