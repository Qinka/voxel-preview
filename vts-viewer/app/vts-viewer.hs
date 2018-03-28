module Main where

import Data.Binary
import System.Environment
import VTS.Base
import VTS.Viewer
import VTS.Viewer.Binding

main :: IO ()
main = do
  files <- getArgs
  print files
  flip mapM_ files $ \file -> do
    vts <- decodeFile file :: IO (VoxelTensor Word8)
    let (ls,d,w,h) = toList vts :: ([Word8],Int,Int,Int)
        Right o = fromList ((/255).fromIntegral <$> ls) d w h :: Either String (VoxelTensor Float)
    loadLibContext 0
    cc <- createContext o
    dc <- createDevContext cc 0
    display cc dc 0.05
    freeLibContext
