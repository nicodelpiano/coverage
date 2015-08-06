module ECL.ECLSpec (main, spec) where

import Test.Hspec

f :: Int -> Int
f 1 = 1
f _ = 0

spec :: Spec
spec = do
  describe "Test" $ do
    it "Suite" $ f 1 `shouldBe` 1

main :: IO ()
main = hspec spec
