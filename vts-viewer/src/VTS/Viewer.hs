{- |
Module      : VTS.Viewer
Description : 
Copyright   : (C) 2018 Johann Lee
License     : GPL-3
Maintainer  : me@qinka.pro
Portability : unknown


-}

module VTS.Viewer where

import VTS.Viewer.Binding
import           Graphics.Rendering.OpenGL                 as GL
import qualified Graphics.Rendering.OpenGL.GL.VertexArrays as VA
import Control.Monad ()

display :: ComputingContext -- ^ computing context
        -> Double           -- ^ sleeping time
        -> MVar Bool        -- ^ status codes, false will exit
        -> IO ()
display cc sleep mvar = do
  -- initialize
  GLFW.initialize
  -- TODO initialize opencl and computing

  -- open windows
  GLFW.openWindows (GL.Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "Voxel Tensor "
  GL.shadeModel    $= GL.Smooth

  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5

  -- set the color to clear background
  GL.clearColor $= Color4 1 1 1 0

   -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \size@(GL.Size w h) ->
    GL.viewport   $= (GL.Position 0 0, size)

  -- loop

  -- finish up
  GLFW.closeWindow
  GLFW.terminate
  where drawLoop = do
          w0 <- getParam Opened
          esc <- GLFW.getKey GLFW.ESC
          when (esc /= GLFW.Press && w0) $ do
            GLFW.pollEvents
            GL.clear [GL.ColorBuffer]

            -- send signal to make copy
            -- wait signal for copying finish

            -- draw
            depth <- getDepth cc
            width <- getWidth cc
            height <- getHeight cc
            let num = fromIntegral $ depth * width * height
            eps <- getEdgePoint cc
            fps <- getFacePoint cc
            fcs <- getFaceColor cc
            clientState VertexArrays $= Enabled
            -- lines
            color $ Color3 0 1 (0 :: Float)
            let vadL = VertexArrayDescriptor 3 VA.Float 0 eps
            arrayPointer VertexArray $= vadL
            drawArrays Lines 0 (num * 12)
            -- faces
            clientState ColorArray $= Enabled
            let vadF = VertexArrayDescriptor 3 VA.Float 0 fps
                vadC = VertexArrayDescriptor 3 VA.Float 0 fcs
            arrayPointer VertexArray $= vadF
            arrayPointer ColorArray  $= vadC
            drawArrays Quads 0 (num * 6)
            clientState VertexArray $= Disabled
            clientState ColorArray  %= Disabled

            -- swap
            GLFW.swapBuffers
            -- sleep
            GLFW.sleep sleep
            -- draw again
            drawLoop


computing :: ComputingContext
          -> DeviceContext
          -> MVar
          -> IO ()
computing cc dc mvar = do
  runComputingM cc dc $ do
    copyVoxelTensor
    -- loop begin
    -- computing limits
    addScale
    sync
    addLimit
    sync
    addEdgePoints
    addFaceColors
    addFacePoints
    sync
    -- copy wait signal
    copyEdgePoints
    copyFaceColors
    copyFacePoints
    -- end copy send signal
    -- loop again
