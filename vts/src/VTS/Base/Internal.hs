{- |
Module      : VTS.Base.Internal
Description : Internal data and functions
Copyright   : (C) 2018 Johann Lee
License     : GPL-3
Maintainer  : me@qinka.pro
Stability   : experimental
Portability : unknown

The internal definations of VTS and its functions.
-}

{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE Unsafe                 #-}

module VTS.Base.Internal
  ( fromList
  , toList
  , fromArrayTensor
  , toArrayTensor
  , fromArray
  , toArray
  , VoxelTensor(..)
  ) where

import           Data.Array.Repa                 ((:.) (..), Array (..),
                                                  Shape (..), Z (..), extent,
                                                  size)
import           Data.Array.Repa.Repr.ForeignPtr (F (..), fromForeignPtr,
                                                  toForeignPtr)
import           Data.Typeable
import           Foreign.ForeignPtr              (ForeignPtr (..),
                                                  mallocForeignPtrArray,
                                                  withForeignPtr)
import           Foreign.Marshal.Array           (copyArray, peekArray,
                                                  withArray)
import           Foreign.Storable                (Storable (..))
import qualified GHC.Exts                        as L (IsList (..))
import           System.IO.Unsafe                (unsafePerformIO)


data VoxelTensor a = VTS
                     {-# UNPACK #-} !Int            -- ^ depth
                     {-# UNPACK #-} !Int            -- ^ width
                     {-# UNPACK #-} !Int            -- ^ height
                     {-# UNPACK #-} !(ForeignPtr a) -- ^ real tensor
  deriving(Typeable)


-- | create vts from list
fromList :: (L.IsList l, Storable a, a ~ (L.Item l))
         => l                                         -- ^ list of elements
         -> Int                                       -- ^ depth
         -> Int                                       -- ^ width
         -> Int                                       -- ^ height
         -> Either String (VoxelTensor a)
fromList list = fromList' (L.toList list)

fromList' items d w h =
  if length items < d * w * h
  then Left "More elements needed"
  else Right $ unsafePerformIO $ genList items
    where genList ls = do
           fp <- mallocForeignPtrArray $ d * w * h
           withForeignPtr fp $ \p ->
             withArray ls $ \pls -> do
             copyArray p pls $ d * w * h
           return $ VTS d w h fp

-- | transformed to list
toList :: (L.IsList l, Storable a, a ~ (L.Item l))
       => VoxelTensor a                            -- ^ the vts
       -> (l,Int,Int,Int)                          -- ^ list, depth, width, height
toList vts = let (ls,d,w,h) = toList' vts
             in (L.fromList ls, d,w,h)

toList' (VTS d w h fp) =
  let list = unsafePerformIO $ genList fp
  in (list,d,w,h)
  where genList fp = do
          withForeignPtr fp $ \p -> do
            peekArray (d * w * h) p

fromArrayTensor :: Storable a
                => Array F (Z :. Int :. Int :. Int) a -- ^ Repa array tensor (3d)
                -> VoxelTensor a                      -- ^ VTS
fromArrayTensor arr =
  let (Z :. d :. w :. h) = extent arr
      fp = toForeignPtr arr
  in VTS d w h fp

toArrayTensor :: Storable a
              => VoxelTensor a                       -- ^ voxel tensor
              -> Array F (Z :. Int :. Int :. Int) a  -- ^ repa list in shape Z :. d :. w :. h
toArrayTensor (VTS d w h fp) = fromForeignPtr (Z :. d :. w :. h) fp

fromArray :: (Storable a, Shape sh)
          => Array F sh a                  -- ^ repa array, we do not concern about the shape (or say, just reshape/resize it)
          -> Int                           -- ^ depth
          -> Int                           -- ^ width
          -> Int                           -- ^ height
          -> Either String (VoxelTensor a) -- ^ a tensor voxel
fromArray arr d w h =
  let len = size $ extent arr
      fp = toForeignPtr arr
  in if len < d * w * h
     then Left "More elements needed"
     else Right $ VTS d w h fp

toArray :: (Storable a, Shape sh)
        => VoxelTensor a                    -- ^ voxel tensor
        -> sh                               -- ^ shape
        -> Either String (Array F sh a)     -- ^ repa array
toArray (VTS d w h fp) sh =
  if size sh > d * w * h
  then Left "More elements needed"
  else Right $ fromForeignPtr sh fp


