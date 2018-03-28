{- |
Module      : VTS.Viewer.Binding
Description : Binding for C files
Copyright   : (C) 2018 Johann Lee
License     : GPL-3
Maintainer  : me@qinka.pro
Portability : unknown

cbits function binding.
-}

{-# LANGUAGE ForeignFunctionInterface    #-}
{-# LANGUAGE RecordWildCards            #-}

module VTS.Viewer.Binding
  ( DeviceContext
  , ComputingContext
  , Computing(..)
  , getDepth
  , getWidth
  , getHeight
  , getNum
  , getScale
  , getBottom
  , getTop
  , getEdgePoint
  , getFaceColor
  , getFacePoint
  , getLimitTensor
  , getVoxelTensor
  , setBottom
  , setDepth
  , setHeight
  , setScale
  , setTop
  , setWidth
  , createContext
  , createDevContext
  , releaseContext
  , releaseDevContext
  , runComputingM
  , addEdgePoints
  , addFaceColors
  , addFacePoints
  , addLimit
  , addScale
  , copyEdgePoints
  , copyFaceColors
  , copyFacePoints
  , copyVoxelTensor
  , copyLimitTensor
  , sync
  , updateBottom
  , updateScale
  , updateTop
  , loadLibContext
  , freeLibContext
  , printAllPlaDev
  ) where

#include "vts-view.h"

import Foreign.Ptr(Ptr(..))
import Foreign.C.Types(CSize(..),CFloat(..),CInt(..))
import Foreign.Storable(Storable(..))
import VTS.Base(VoxelTensor(..))
import Control.Monad.IO.Class(MonadIO(..))
import Foreign.ForeignPtr(withForeignPtr)

unCFlt :: CFloat -> Float
unCFlt (CFloat f) = f

data DeviceContext = DeviceContext
  { unDeviceContextPtr :: Ptr DeviceContext}
  deriving (Eq,Show)

data ComputingContext = ComputingContext
  { unComputingContextPtr :: Ptr ComputingContext}
  deriving (Eq,Show)

getDepth  :: ComputingContext -> IO Int
getDepth  (ComputingContext p) = #{peek struct computing_context,  depth} p

getWidth  :: ComputingContext -> IO Int
getWidth  (ComputingContext p) = #{peek struct computing_context,  width} p

getHeight :: ComputingContext -> IO Int
getHeight (ComputingContext p) = #{peek struct computing_context, height} p

getScale  :: ComputingContext -> IO Float
getScale  (ComputingContext p) = unCFlt <$> #{peek struct computing_context,   scal} p

getBottom :: ComputingContext -> IO Float
getBottom (ComputingContext p) = unCFlt <$> #{peek struct computing_context, bottom} p

getTop    :: ComputingContext -> IO Float
getTop    (ComputingContext p) = unCFlt <$> #{peek struct computing_context,    top} p

getNum    :: ComputingContext -> IO Int
getNum    (ComputingContext p) = #{peek struct computing_context, all} p

setDepth  :: ComputingContext -> Int   -> IO ()
setDepth  (ComputingContext p) e = #{poke struct computing_context,  depth} p e
setWidth  :: ComputingContext -> Int   -> IO ()
setWidth  (ComputingContext p) e = #{poke struct computing_context,  width} p e
setHeight :: ComputingContext -> Int   -> IO ()
setHeight (ComputingContext p) e = #{poke struct computing_context, height} p e
setScale  :: ComputingContext -> Float -> IO ()
setScale  (ComputingContext p) e = #{poke struct computing_context,   scal} p e
setBottom :: ComputingContext -> Float -> IO ()
setBottom (ComputingContext p) e = #{poke struct computing_context, bottom} p e
setTop    :: ComputingContext -> Float -> IO ()
setTop    (ComputingContext p) e = #{poke struct computing_context,    top} p e


getVoxelTensor :: ComputingContext -> IO (Ptr Float)
getVoxelTensor (ComputingContext p) = #{peek struct computing_context, voxel_tensor} p

getLimitTensor :: ComputingContext -> IO (Ptr Float)
getLimitTensor (ComputingContext p) = #{peek struct computing_context, limit_tensor} p

getEdgePoint   :: ComputingContext -> IO (Ptr Float)
getEdgePoint   (ComputingContext p) = #{peek struct computing_context,  edge_points} p

getFacePoint   :: ComputingContext -> IO (Ptr Float)
getFacePoint   (ComputingContext p) = #{peek struct computing_context,  face_points} p

getFaceColor   :: ComputingContext -> IO (Ptr Float)
getFaceColor   (ComputingContext p) = #{peek struct computing_context,  face_colors} p

foreign import ccall "memcpy" _memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)

foreign import ccall "create_context" _create_context :: CSize -> CSize -> CSize -> IO (Ptr ComputingContext)
foreign import ccall "release_context" _release_context :: Ptr ComputingContext -> IO ()

createContext :: VoxelTensor Float -> IO ComputingContext
createContext (VTS d w h fp) = do
  ccPtr <- _create_context (fromIntegral d) (fromIntegral w) (fromIntegral h)
  let cc = ComputingContext ccPtr
  vtPtr <- getVoxelTensor cc
  withForeignPtr fp $ \p ->
    _memcpy vtPtr p $ fromIntegral $ d * w * h * sizeOf (undefined :: Float)
  return cc

releaseContext :: ComputingContext -> IO ()
releaseContext (ComputingContext p) = _release_context p


foreign import ccall "create_dev_context" _create_dev_context :: Ptr ComputingContext -> CInt -> IO (Ptr DeviceContext)
foreign import ccall "release_dev_context" _release_dev_context :: Ptr DeviceContext -> IO ()



createDevContext :: ComputingContext -> Int -> IO DeviceContext
createDevContext (ComputingContext p) idx =
  DeviceContext <$>  _create_dev_context p (fromIntegral idx)

releaseDevContext :: DeviceContext -> IO ()
releaseDevContext (DeviceContext p) = _release_dev_context p

foreign import ccall "add_scale_computing" _add_scale_computing :: Ptr ComputingContext -> Ptr DeviceContext -> IO CInt
foreign import ccall "add_limit_computing" _add_limit_computing :: Ptr ComputingContext -> Ptr DeviceContext -> IO CInt
foreign import ccall "add_edgeps_computing" _add_edgeps_computing :: Ptr ComputingContext -> Ptr DeviceContext -> IO CInt
foreign import ccall "add_faceps_computing" _add_faceps_computing :: Ptr ComputingContext -> Ptr DeviceContext -> IO CInt
foreign import ccall "add_color_computing" _add_color_computing :: Ptr ComputingContext -> Ptr DeviceContext -> IO CInt
foreign import ccall "sync_computing" _sync_computing :: Ptr ComputingContext -> Ptr DeviceContext -> IO CInt
foreign import ccall "copy_memory_voxel_tensor" _copy_memory_voxel_tensor :: Ptr ComputingContext -> Ptr DeviceContext -> IO CInt
foreign import ccall "copy_memory_limit_tensor" _copy_memory_limit_tensor :: Ptr ComputingContext -> Ptr DeviceContext -> IO CInt
foreign import ccall "copy_memory_edge_points" _copy_memory_edge_points :: Ptr ComputingContext -> Ptr DeviceContext -> IO CInt
foreign import ccall "copy_memory_face_points" _copy_memory_face_points :: Ptr ComputingContext -> Ptr DeviceContext -> IO CInt
foreign import ccall "copy_memory_face_colors" _copy_memory_face_colors :: Ptr ComputingContext -> Ptr DeviceContext -> IO CInt

newtype Computing m a = Computing
  { runComputing :: ComputingContext -> DeviceContext -> m (a,ComputingContext, DeviceContext)
  }

instance Monad m => Functor (Computing m) where
  fmap f (Computing c) = Computing $ \cc dc -> do
    (a,cc1,dc1) <- c cc dc
    return (f a, cc1, dc1)

instance Monad m => Applicative (Computing m) where
  pure x = Computing $ \cc dc -> return (x, cc, dc)
  (<*>) (Computing ff) (Computing fa) = Computing $ \cc dc -> do
    (a,cc1,dc1) <- fa cc dc
    (f,cc2,dc2) <- ff cc1 dc1
    return (f a, cc2, dc2)

instance Monad m => Monad (Computing m) where
  (>>=) (Computing ma) fm = Computing $ \cc dc -> do
    (a,cc1,dc1) <- ma cc dc
    let (Computing mb) = fm a
    mb cc1 dc1

instance MonadIO m => MonadIO (Computing m) where
  liftIO m = Computing $ \cc dc -> do
    rt <- liftIO m
    return (rt,cc,dc)

runComputingM :: Monad m
              => ComputingContext
              -> DeviceContext
              -> Computing m a
              -> m a
runComputingM cc dc (Computing cm) = 
  (\(e,_,_) -> e) <$> cm cc dc

addScale        :: Computing IO Int
addLimit        :: Computing IO Int
addEdgePoints   :: Computing IO Int
addFacePoints   :: Computing IO Int
addFaceColors   :: Computing IO Int
sync            :: Computing IO Int
copyVoxelTensor :: Computing IO Int
copyEdgePoints  :: Computing IO Int
copyFacePoints  :: Computing IO Int
copyFaceColors  :: Computing IO Int

mkComputing :: Monad m
            => (ComputingContext -> DeviceContext -> m a)
            -> Computing m a
mkComputing f = Computing $ \cc dc -> (\x -> (x,cc,dc)) <$> f cc dc

addScale = mkComputing $ \(ComputingContext pcc) (DeviceContext pdc) ->
  fromIntegral <$> _add_scale_computing pcc pdc

addLimit = mkComputing $ \(ComputingContext pcc) (DeviceContext pdc) ->
  fromIntegral <$> _add_limit_computing pcc pdc

addEdgePoints = mkComputing $ \(ComputingContext pcc) (DeviceContext pdc) ->
  fromIntegral <$> _add_edgeps_computing pcc pdc

addFacePoints = mkComputing $ \(ComputingContext pcc) (DeviceContext pdc) ->
  fromIntegral <$> _add_faceps_computing pcc pdc

addFaceColors = mkComputing $ \(ComputingContext pcc) (DeviceContext pdc) ->
  fromIntegral <$> _add_color_computing pcc pdc

sync = mkComputing $ \(ComputingContext pcc) (DeviceContext pdc) ->
  fromIntegral <$> _sync_computing pcc pdc

copyVoxelTensor = mkComputing $ \(ComputingContext pcc) (DeviceContext pdc) ->
  fromIntegral <$> _copy_memory_voxel_tensor pcc pdc

copyLimitTensor = mkComputing $ \(ComputingContext pcc) (DeviceContext pdc) ->
  fromIntegral <$> _copy_memory_limit_tensor pcc pdc

copyEdgePoints = mkComputing $ \(ComputingContext pcc) (DeviceContext pdc) ->
  fromIntegral <$> _copy_memory_edge_points pcc pdc

copyFacePoints = mkComputing $ \(ComputingContext pcc) (DeviceContext pdc) ->
  fromIntegral <$> _copy_memory_face_points  pcc pdc
  
copyFaceColors = mkComputing $ \(ComputingContext pcc) (DeviceContext pdc) ->
  fromIntegral <$> _copy_memory_face_colors  pcc pdc

foreign import ccall "load_library_context" loadLibContext :: Int -> IO Int
foreign import ccall "free_library_context" freeLibContext :: IO ()
foreign import ccall "print_all_plat_n_dev" printAllPlaDev :: IO ()

updateScale :: Maybe Float -> Computing IO ()
updateScale Nothing  = return ()
updateScale (Just s) = mkComputing $ \cc _ -> setScale cc s

updateBottom :: Maybe Float -> Computing IO ()
updateBottom Nothing  = return ()
updateBottom (Just b) = mkComputing $ \cc _ -> setBottom cc b

updateTop :: Maybe Float -> Computing IO ()
updateTop Nothing  = return ()
updateTop (Just t) = mkComputing $ \cc _ -> setTop cc t
