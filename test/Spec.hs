import Test.Hspec
import Lib


main :: IO ()
main = hspec $ do
    describe "Blah" $ do
        it "bleh" $ do 
            someFunc
            True `shouldBe` True
