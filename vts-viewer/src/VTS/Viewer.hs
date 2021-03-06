{- |
Module      : VTS.Viewer
Description :
Copyright   : (C) 2018 Johann Lee
License     : GPL-3
Maintainer  : me@qinka.pro
Portability : unknown


-}

{-# LANGUAGE RecordWildCards #-}

module VTS.Viewer
  ( display
  )where

import           Control.Concurrent
import           Control.Monad                             (when, void)
import           Control.Monad.IO.Class                    (liftIO)
import           Graphics.Rendering.OpenGL                 as GL
import qualified Graphics.Rendering.OpenGL.GL.VertexArrays as VA
import           Graphics.UI.GLFW                          as GLFW
import           VTS.Viewer.Binding


data SyncSignals = SyncSignals
  { mvarScal      :: MVar Float
  , mvarBottom    :: MVar Float
  , mvarTop       :: MVar Float
  , mvarReadyCopy :: MVar ()
  , mvarCopyDone  :: MVar ()
  }

display :: ComputingContext -- ^ computing context
        -> DeviceContext    -- ^ device context
        -> Double           -- ^ sleeping time
        -> IO ()
display cc dc st = do
  -- initialize
  GLFW.initialize

  -- open windows
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "Voxel Tensor "
  GL.shadeModel    $= GL.Smooth

  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 2

  -- depth
  GL.depthClamp $= Enabled
  GL.depthFunc  $= Just GL.Lequal

  -- set the color to clear background
  GL.clearColor $= Color4 0 0 0 0

   -- OpenGL Viewport
  GLFW.windowSizeCallback $= \size@(GL.Size w h) ->
    let m = min w h
    in GL.viewport   $= (GL.Position 0 0, GL.Size m h)

  -- launch thread for drawing and computing
  ss <- SyncSignals
    <$> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar
    
  tid <- forkOS $ computing cc dc ss
  drawLoop st cc dc ss
  killThread tid

  -- finish, clean up
  GLFW.closeWindow
  GLFW.terminate

drawLoop :: Double -> ComputingContext -> DeviceContext -> SyncSignals -> IO ()
drawLoop  st cc dc  ss@SyncSignals{..} = do
  w0 <- getParam Opened
  esc <- GLFW.getKey GLFW.ESC

  when (esc /= GLFW.Press && w0) $ do
    GLFW.pollEvents
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    -- get sizes and pointers
    depth <- getDepth cc
    width <- getWidth cc
    height <- getHeight cc
    num <- fromIntegral <$> getNum cc -- get (depth * width * height)
    vt  <- getVoxelTensor cc
    lt  <- getLimitTensor cc
    eps <- getEdgePoint cc
    fcs <- getFaceColor cc
    fps <- getFacePoint cc

    -- draw
    clientState VertexArray $= Enabled
    clientState ColorArray  $= Enabled

    -- faces
    let vadF = VertexArrayDescriptor 3 VA.Float 0 fps
        vadC = VertexArrayDescriptor 3 VA.Float 0 fcs
    arrayPointer VertexArray $= vadF
    arrayPointer ColorArray  $= vadC
    drawArrays Quads 0 (num * 24)

    clientState ColorArray $= Disabled

    -- lines
    color $ Color3 0 1 (0 :: Float)
    let vadL = VertexArrayDescriptor 3 VA.Float 0 eps
    arrayPointer VertexArray $= vadL
    drawArrays Lines 0 (num * 24)

    clientState VertexArray $= Disabled

    -- control
    let keyPass key m = GLFW.getKey key
          >>= (\status -> when (status == GLFW.Press) m)

    -- rolling
    keyPass 'W' $ GL.rotate ( 10 :: Double) (Vector3 1 0 0)
    keyPass 'S' $ GL.rotate (-10 :: Double) (Vector3 1 0 0)
    keyPass 'A' $ GL.rotate ( 10 :: Double) (Vector3 0 1 0)
    keyPass 'D' $ GL.rotate (-10 :: Double) (Vector3 0 1 0)
    keyPass 'Q' $ GL.rotate ( 10 :: Double) (Vector3 0 0 1)
    keyPass 'E' $ GL.rotate (-10 :: Double) (Vector3 0 0 1)
    -- scaling
    keyPass '=' $ GL.scale 1.1 1.1 (1.1 ::Double)
    keyPass '-' $ GL.scale 0.9 0.9 (0.9 :: Double)
    -- moving
    keyPass GLFW.UP    $ GL.translate (Vector3   1  0  0 :: Vector3 Float)
    keyPass GLFW.DOWN  $ GL.translate (Vector3 (-1) 0  0 :: Vector3 Float)
    keyPass GLFW.LEFT  $ GL.translate (Vector3 0    1  0 :: Vector3 Float)
    keyPass GLFW.RIGHT $ GL.translate (Vector3 0  (-1) 0 :: Vector3 Float)

    -- flush finish and swap
    GL.flush
    GL.finish

    -- parameter changeed
    let up x = x + 0.01
        down x = x - 0.01
    keyPass ']'  $ toUpdateParam (getScale  cc) mvarScal   up
    keyPass '['  $ toUpdateParam (getScale  cc) mvarScal   down
    keyPass '\'' $ toUpdateParam (getBottom cc) mvarBottom up
    keyPass ';'  $ toUpdateParam (getBottom cc) mvarBottom down
    keyPass '.'  $ toUpdateParam (getTop    cc) mvarTop    up
    keyPass ','  $ toUpdateParam (getTop    cc) mvarTop    down

    -- send signal to make copy
    putMVar mvarReadyCopy  ()
    -- wait signal for copying finish
    takeMVar mvarCopyDone

    GLFW.swapBuffers

    -- sleep
    GLFW.sleep st
    -- draw again
    drawLoop st cc dc ss

toUpdateParam :: IO a -> MVar a -> (a -> a) -> IO ()
toUpdateParam getOld mvar updating = do
  old <- getOld
  let new = updating old
  err <- tryPutMVar mvar new
  when (not err) $ void $ swapMVar mvar new

computing :: ComputingContext
          -> DeviceContext
          -> SyncSignals
          -> IO ()
computing cc dc ss@SyncSignals{..} = runComputingM cc dc $ do
  copyVoxelTensor
  sync
  liftIO $ putMVar mvarScal   1
  liftIO $ putMVar mvarBottom 0
  liftIO $ putMVar mvarTop    1
  computeLoop ss
  where computeLoop ss@SyncSignals{..} = do
          -- get param
          newScale  <- liftIO $ tryTakeMVar mvarScal
          newBottom <- liftIO $ tryTakeMVar mvarBottom
          newTop    <- liftIO $ tryTakeMVar mvarTop
          mDo <- case (newScale, newBottom, newTop) of
                 (Nothing, Nothing, Nothing) -> return (return ())
                 (s,b,t) -> do
                   updateScale  s
                   updateBottom b
                   updateTop    t
               
                   -- computing limits
                   addScale
                   sync
                   addLimit
                   sync

                   -- computing points
                   addEdgePoints
                   addFaceColors
                   addFacePoints
                   sync
                   return $ do
                     -- copy points and colors
                     copyEdgePoints
                     copyFaceColors
                     copyFacePoints
                     copyLimitTensor
                     sync
                     return ()

          -- wait signal
          liftIO $ takeMVar mvarReadyCopy

          -- sycn term action
          mDo

          -- send signal
          liftIO $ putMVar mvarCopyDone ()

          -- loop and again
          computeLoop ss
