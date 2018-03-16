{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}

module VTS.BaseSpec
    ( spec
    ) where

import           Foreign.ForeignPtr
import           Test.Hspec
import           VTS.Base
import           Foreign.Marshal.Array
import           Data.Vector.Unboxed (Vector)
import Data.Array.Repa hiding (toList,map)
import Data.Array.Repa.Repr.ForeignPtr
import Data.Binary

spec :: Spec
spec = do
  describe "create from list" $ do
    it "list [0..7] 2 2 2" $ do
      let (Right (VTS d w h fp)) = fromList (mkList 7) 2 2 2
      d `shouldBe` 2
      w `shouldBe` 2
      h `shouldBe` 2
      peekFP (d*w*h) fp `shouldReturn` [0..7]
    it "list [0..8] 2 2 2" $ do
      let (Right (VTS d w h fp)) = fromList (mkList 8) 2 2 2
      d `shouldBe` 2
      w `shouldBe` 2
      h `shouldBe` 2
      peekFP (d*w*h) fp `shouldReturn` [0..7]
    it "list [0..6] 2 2 2" $ do
      let (Left str) = fromList (mkList 6) 2 2 2
      str `shouldBe` strError
    it "vec [0..7] 2 2 2" $ do
      let (Right (VTS d w h fp)) = fromList (mkVec 7) 2 2 2
      d `shouldBe` 2
      w `shouldBe` 2
      h `shouldBe` 2
      peekFP (d*w*h) fp `shouldReturn` [0..7]
    it "vec [0..8] 2 2 2" $ do
      let (Right (VTS d w h fp)) = fromList (mkVec 8) 2 2 2
      d `shouldBe` 2
      w `shouldBe` 2
      h `shouldBe` 2
      peekFP (d*w*h) fp `shouldReturn` [0..7]
    it "vec [0..6] 2 2 2" $ do
      let (Left str) = fromList (mkVec 6) 2 2 2
      str `shouldBe` strError
  describe "get list from vts" $ do
    it "get list" $ do
      let (Right vts) = fromList (mkList 7) 2 2 2
      toList vts `shouldBe` (mkList 7,2,2,2)
    it "get vec" $ do
      let (Right vts) = fromList (mkList 7) 2 2 2
      toList vts `shouldBe` (mkVec 7,2,2,2)
    it "get list(more)" $ do
      let (Right vts) = fromList (mkList 8) 2 2 2
      toList vts `shouldBe` (mkList 7,2,2,2)
    it "get vec(more)" $ do
      let (Right vts) = fromList (mkList 8) 2 2 2
      toList vts `shouldBe` (mkVec 7,2,2,2)
  describe "repa array (tensor)" $ do
    it "from" $ do
      let array = copyS $ fromUnboxed (Z :. 2 :. 2 :. 2) [0..7] :: Array F (Z :. Int :. Int :. Int) Int
          (VTS d w h fp) = fromArrayTensor array
      d `shouldBe` 2
      w `shouldBe` 2
      h `shouldBe` 2
      peekFP (d*w*h) fp `shouldReturn` [0..7 :: Int]
    it "to" $ do
      let (Right vts) = fromList (mkList 7) 2 2 2
          array = fromUnboxed (Z :. 2 :. 2 :. 2) [0..7] :: Array U (Z :. Int :. Int :. Int) Int
      copyS (toArrayTensor vts) `shouldBe` array
  describe "repa array (whatever)" $ do
    it "from 1d vector" $ do
      let array = copyS $ fromUnboxed (Z :. 8) [0..7] :: Array F (Z :. Int) Int
          (Right (VTS d w h fp)) = fromArray array 2 2 2
      d `shouldBe` 2
      w `shouldBe` 2
      h `shouldBe` 2
      peekFP (d*w*h) fp `shouldReturn` [0..7]
    it "from 1d vector(should be failed)" $ do
      let array = copyS $ fromUnboxed (Z :. 7) [0..6] :: Array F (Z :. Int) Int
          (Left str) = fromArray array 2 2 2
      str `shouldBe` strError
    it "to 1d vector" $ do
      let (Right vts) = fromList (mkList 7) 2 2 2
          array = fromUnboxed (Z :.8) [0..7] :: Array U (Z :. Int) Int
      fmap copyS (toArray vts (Z :. 8)) `shouldBe` Right array
    it "to 1d vector(should be failed)" $ do
      let (Left str) = fromList (mkList 6) 2 2 2
      str `shouldBe` strError
    it "from 2d vector" $ do
      let array = copyS $ fromUnboxed (Z :. 4 :. 2) [0..7] :: Array F (Z :. Int :. Int) Int
          (Right (VTS d w h fp)) = fromArray array 2 2 2
      d `shouldBe` 2
      w `shouldBe` 2
      h `shouldBe` 2
      peekFP (d*w*h) fp `shouldReturn` [0..7]
    it "from 2d vector(should be failed)" $ do
      let array = copyS $ fromUnboxed (Z :. 2 :. 2) [0..4] :: Array F (Z :. Int :. Int) Int
          (Left str) = fromArray array 2 2 2
      str `shouldBe` strError
    it "to 2d vector" $ do
      let (Right vts) = fromList (mkList 7) 2 2 2
          array = fromUnboxed (Z :.4 :. 2) [0..7] :: Array U (Z :. Int :. Int) Int
      fmap copyS (toArray vts (Z :. 4 :. 2)) `shouldBe` Right array
    it "to 2d vector(should be failed)" $ do
      let (Left str) = fromList (mkList 6) 2 2 2
      str `shouldBe` strError
    it "from 3d vector" $ do
      let array = copyS $ fromUnboxed (Z :. 2 :. 2 :. 2) [0..7] :: Array F (Z :. Int :. Int :. Int) Int
          (Right (VTS d w h fp)) = fromArray array 2 2 2
      d `shouldBe` 2
      w `shouldBe` 2
      h `shouldBe` 2
      peekFP (d*w*h) fp `shouldReturn` [0..7]
    it "from 3d vector(should be failed)" $ do
      let array = copyS $ fromUnboxed (Z :. 2 :. 2 :. 2) [0..7] :: Array F (Z :. Int :. Int :. Int) Int
          (Left str) = fromArray array 2 2 3
      str `shouldBe` strError
    it "to 3d vector" $ do
      let (Right vts) = fromList (mkList 7) 2 2 2
          array = fromUnboxed (Z :.2 :. 2:. 2) [0..7] :: Array U (Z :. Int :. Int :. Int) Int
      fmap copyS (toArray vts (Z :. 2 :. 2 :. 2)) `shouldBe` Right array
    it "to 3d vector(should be failed)" $ do
      let (Left str) = fromList (mkList 6) 2 2 2
      str `shouldBe` strError
  describe "vts eq" $ do
    it "self" $ do
      let vts = mkVTS 2 2 2
      vts == vts `shouldBe` True
      vts /= vts `shouldBe` False
      vts `shouldBe` vts
    it "two same" $ do
      let v1 = mkVTS 2 2 2
          v2 = mkVTS 2 2 2
      v1 == v2 `shouldBe` True
      v2 == v1 `shouldBe` True
      v1 /= v2 `shouldBe` False
      v2 /= v1 `shouldBe` False
      v1 `shouldBe` v2
      v2 `shouldBe` v1
    it "two with different shape & same size" $ do
      let v1 = mkVTS 2 1 2
          v2 = mkVTS 1 2 2
          v3 = mkVTS 2 2 1
      v1 == v2 `shouldBe` False
      v1 /= v2 `shouldBe` True
      v2 == v1 `shouldBe` False
      v2 /= v1 `shouldBe` True
      v1 == v3 `shouldBe` False
      v1 /= v3 `shouldBe` True
      v3 == v1 `shouldBe` False
      v3 /= v1 `shouldBe` True
      v2 == v3 `shouldBe` False
      v2 /= v3 `shouldBe` True
      v3 == v2 `shouldBe` False
      v3 /= v2 `shouldBe` True
      v1 `shouldNotBe` v2
      v2 `shouldNotBe` v1
      v1 `shouldNotBe` v3
      v3 `shouldNotBe` v1
      v2 `shouldNotBe` v3
      v3 `shouldNotBe` v2
    it "two, both shape and size is different" $ do
      let v1 = mkVTS 2 2 2
          v2 = mkVTS 3 3 3
      v1 == v2 `shouldBe` False
      v2 == v1 `shouldBe` False
      v1 /= v2 `shouldBe` True
      v2 /= v1 `shouldBe` True
      v1 `shouldNotBe` v2
      v2 `shouldNotBe` v1
  describe "vts ord" $ do
    it "eq self" $ do
      let v = mkVTS 2 2 2
      compare v v `shouldBe` EQ
    it "eq same" $ do
      let v1 = mkVTS 2 2 2
          v2 = mkVTS 2 2 2
      compare v1 v2 `shouldBe` EQ
    it "\"same\" value, different size " $ do
      let v1 = mkVTS 2 2 2
          v2 = mkVTS 2 2 1
      compare v1 v2 `shouldBe` GT
      compare v2 v1 `shouldBe` LT
    it "different value, same size " $ do
      let v1 = mkVTSBase 1 2 2 2
          v2 = mkVTSBase 2 2 2 2
      compare v1 v2 `shouldBe` LT
      compare v2 v1 `shouldBe` GT
  describe "vts binary" $ do
    it "id test" $ do
      let origin = mkVTS 2 3 4
          str = encode origin
          coded = decode str
      coded `shouldBe` origin
  describe "show " $
    it "show" $ do
    print (mkVTS 2 3 4) `shouldReturn` ()

peekFP len fp = withForeignPtr fp $ \p -> peekArray len p

mkList :: Int -> [Int]
mkList x = [0..x]

mkVec :: Int -> Vector Int
mkVec x = [0..x]

strError = "More elements needed"

mkVTS :: Int -> Int -> Int -> VoxelTensor Int
mkVTS d w h =
  let l = d * w * h
      Right v = fromList (mkList l) d w h
  in v

mkVTSBase :: Int -> Int -> Int -> Int -> VoxelTensor Int
mkVTSBase b d w h =
  let l = d * w * h
      Right v = fromList (map (+b) [0 .. l]) d w h
  in v
