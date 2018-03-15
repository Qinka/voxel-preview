{-# LANGUAGE OverloadedLists #-}

module VTS.Base.InternalSpec
    ( spec
    ) where

import           Foreign.ForeignPtr
import           Test.Hspec
import           VTS.Base.Internal
import           Foreign.Marshal.Array
import           Data.Vector.Unboxed (Vector)

-- | using hspec
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
      str `shouldBe` "More elements needed"
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
      str `shouldBe` "More elements needed"
  describe "get list from vts" $ do
    it "get list" $ do
      let (Right vts) = fromList (mkList 7) 2 2 2
      toList vts `shouldBe` (mkList 7,2,2,2)
    it "get vec" $ do
      let (Right vts) = fromList (mkList 7) 2 2 2
      toList vts `shouldBe` (mkVec 7,2,2,2)


peekFP len fp = withForeignPtr fp $ \p -> peekArray len p

mkList :: Int -> [Int]
mkList x = [0..x]

mkVec :: Int -> Vector Int
mkVec x = [0..x]
