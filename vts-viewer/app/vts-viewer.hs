{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Data.Binary
import           System.Console.CmdArgs
import           System.Environment
import           VTS.Base
import           VTS.Viewer
import           VTS.Viewer.Binding


data Viewer = Viewer
              { filePath :: FilePath
              , platformId :: Int
              , deviceId   :: Int
              , fps        :: Double
              }
            | ListDev
            deriving (Show, Data, Typeable)

viewer :: Viewer
viewer = Viewer
  { filePath = "" &= args
  , platformId = 0
    &= help "The platform id, you can use listdev mode to display all the platform."
    &= typ "NUM"
    &= explicit
    &= name "p"
    &= name "platform"
  , deviceId = 0
    &= help "The device id(of a platform), you can use listdev mode to display all the devices."
    &= typ "NUM"
    &= explicit
    &= name "d"
    &= name "device"
  , fps = 20
    &= help "The frame per second, for controlling  frame rate."
    &= explicit
    &= name "f"
    &= name "frame"
  } &= details ["The viewer mode is to display the voxel."]

listDev :: Viewer
listDev = ListDev &= details ["The listdev mode is to list all available devices."]

vtsViewer :: Viewer
vtsViewer = modes [viewer &= auto, listDev]

main :: IO ()
main = do
  mode <- cmdArgs vtsViewer
  case mode of
    ListDev -> printAllPlaDev
    Viewer{..} -> do
      putStrLn $ "Openning " ++ filePath
      vts <- decodeFile filePath :: IO (VoxelTensor Word8)
      let (ls,d,w,h) = toList vts :: ([Word8],Int,Int,Int)
          Right o = fromList ((/255).fromIntegral <$> ls) d w h :: Either String (VoxelTensor Float)
      loadLibContext platformId
      cc <- createContext o
      dc <- createDevContext cc deviceId
      display cc dc (1/(fps+2))
      freeLibContext
