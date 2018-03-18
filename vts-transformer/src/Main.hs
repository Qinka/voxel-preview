{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Data.Binary
import           Data.Vector.Storable   (Vector)
import qualified Data.Vector.Storable   as V
import           Data.Word
import           Foreign.Storable
import           System.Console.CmdArgs
import           System.IO
import           VTS.Base

-- | operator
data Transformer = Transformer
  { tInFile  :: FilePath -- ^ input file path
  , tOutFile :: FilePath -- ^ output file path (prefix)
  , tTop     :: Int -- ^ top padding
  , tBottom  :: Int -- ^ bottom padding
  , tLeft    :: Int -- ^ left padding
  , tRight   :: Int -- ^ right padding
  , tDepth   :: Int -- ^ depth
  , tWidth   :: Int -- ^ width
  , tHeight  :: Int -- ^ height
  }
  deriving (Show,Data, Typeable)

transformer :: Transformer
transformer = Transformer
  { tInFile = def
    &= help "input image file, should be grey images"
    &= typ "FILE"
    &= explicit
    &= name "i"
    &= name "in"
  , tOutFile = def
    &= help "output file path prefix"
    &= typ "FILE"
    &= explicit
    &= name "o"
    &= name "out"
  , tLeft = 3
    &= help "the padding of left side"
    &= typ "INT"
    &= explicit
    &= name "l"
    &= name "left"
  , tRight = 1
    &= help "the padding of right side"
    &= typ "INT"
    &= explicit
    &= name "r"
    &= name "right"
  , tTop = 3
    &= help "the padding of top side"
    &= typ "INT"
    &= explicit
    &= name "t"
    &= name "top"
  , tBottom = 1
    &= help "the padding of bottom side"
    &= typ "INT"
    &= explicit
    &= name "b"
    &= name "bottom"
  , tDepth = 8
    &= help "the shape, depth"
    &= typ "INT"
    &= explicit
    &= name "d"
    &= name "depth"
  , tWidth = 8
    &= help "the shape, width"
    &= typ "INT"
    &= explicit
    &= name "w"
    &= name "width"
  , tHeight = 8
    &= help "the shape, height"
    &= typ "INT"
    &= explicit
    &= name "h"
    &= name "height"
  }

main :: IO ()
main = do
  trs@Transformer{..} <- cmdArgs transformer
  print trs
  rdImg <- readImage tInFile
  let trans' = trans tOutFile (tLeft, tTop, tRight, tBottom) (tDepth, tWidth, tHeight)
  case rdImg of
    Right (ImageY8 img) -> trans' img
    Right (ImageYA8 img) -> trans' $ dropAlphaLayer img
    Left err -> do
      hPutStrLn stderr "Sorry got error: "
      hPutStrLn stderr err
    Right _ ->
      hPutStrLn stderr "Sorry got error type, should be grey image (with or without alpha channle)"
  where trans prefix padding shape img = do
          let (w, h, v) = transToListGrey8 img
              rows = splitIntoBoxes padding shape (w,h) v
              vtss = zip [0..] $ genVoxelTensors shape rows
          flip mapM_ vtss $ \(i,vts) ->
            let fn = prefix ++ "_" ++ show i ++ ".vts"
            in encodeFile fn vts
          return ()

transToListGrey8 :: Image Pixel8 -> (Int, Int, Vector Word8)
transToListGrey8 (Image w h v) = (w, h, v) -- flip

-- | splite the hold image into pices
splitIntoBoxes :: Storable x
                => (Int, Int, Int, Int) -- ^ siding (left, top, right, bottom)
                -> (Int, Int, Int) -- ^ tensor sizes d w h
                -> (Int, Int) -- ^ images
                -> Vector x -- ^ images
                -> [[[Vector x]]] -- ^ rows n * d * h
splitIntoBoxes (l, t, r ,b) (d, w, h) (imgW, imgH) v =
  let n_w = imgW `div` boxW
      n_h = imgH `div` boxH
      indexes = [(i,j) | i <- [0..n_w-1], j <- [0..n_h-1]]
      allRows = single <$> indexes
  in split d allRows
  where boxH = h + t + b
        split _ [] = []
        split i ks = take i ks : split i (drop i ks)
        boxW = w + l + r
        single (i, j) =
          let skip = boxH * imgW * j + i * boxW + l + t * imgW
              takeList = map (\x -> skip + x * imgW) [0..h-1]
              rows = map (\x -> V.slice x w v) takeList
          in rows

genVoxelTensors :: Storable x
                => (Int, Int, Int) -- ^ depth weight height
                -> [[[Vector x]]] -- ^ rows
                -> [VoxelTensor x]
genVoxelTensors (d,w,h) rows =
  let tens = map (V.concat . concat) rows
      vtss = getAll tens
  in vtss
  where getAll [] = []
        getAll (t:ts) =
          case fromList t d w h of
            Left _    -> getAll ts
            Right vts -> vts : getAll ts
