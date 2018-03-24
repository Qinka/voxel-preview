{- |
Module      : VTS.Viewer
Description :
Copyright   : (C) 2018 Johann Lee
License     : GPL-3
Maintainer  : me@qinka.pro
Portability : unknown


-}

module VTS.Viewer where

import           Control.Concurrent
import           Control.Monad                             (when)
import           Control.Monad.IO.Class                    (liftIO)
import           Graphics.Rendering.OpenGL                 as GL
import qualified Graphics.Rendering.OpenGL.GL.VertexArrays as VA
import           Graphics.UI.GLFW                          as GLFW
import           VTS.Viewer.Binding


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
  s1 <- newEmptyMVar
  s2 <- newEmptyMVar
  tid <- forkIO $ computing cc dc s1 s2
  drawLoop st cc dc s1 s2
  killThread tid

  -- finish, clean up
  GLFW.closeWindow
  GLFW.terminate

drawLoop :: Double -> ComputingContext -> DeviceContext -> MVar () -> MVar () -> IO ()
drawLoop  st cc dc s1 s2 = do
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
    keyPass '+' $ GL.scale 1.1 1.1 (1.1 ::Double)
    keyPass '-' $ GL.scale 0.9 0.9 (0.9 :: Double)
    -- moving
    keyPass GLFW.UP    $ GL.translate (Vector3   1  0  0 :: Vector3 Float)
    keyPass GLFW.DOWN  $ GL.translate (Vector3 (-1) 0  0 :: Vector3 Float)
    keyPass GLFW.LEFT  $ GL.translate (Vector3 0    1  0 :: Vector3 Float)
    keyPass GLFW.RIGHT $ GL.translate (Vector3 0  (-1) 0 :: Vector3 Float)

    -- flush finish and swap
    GL.flush
    GL.finish

    -- send signal to make copy
    putMVar s1 ()
    -- wait signal for copying finish
    takeMVar s2

    GLFW.swapBuffers


    -- sleep
    GLFW.sleep st
    -- draw again
    drawLoop st cc dc s1 s2


computing :: ComputingContext
          -> DeviceContext
          -> MVar () -> MVar ()
          -> IO ()
computing cc dc s1 s2 = runComputingM cc dc $ do
  copyVoxelTensor
  sync
  computeLoop s1 s2
  where computeLoop s1 s2 = do
          -- wait signal
          liftIO $ takeMVar s1

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

          -- copy points and colors
          copyEdgePoints
          copyFaceColors
          copyFacePoints
          copyLimitTensor
          sync

          -- send signal
          liftIO $ putMVar s2 ()

          -- loop and again
          computeLoop s1 s2
