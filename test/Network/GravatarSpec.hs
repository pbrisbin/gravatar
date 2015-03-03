{-# LANGUAGE OverloadedStrings #-}
module Network.GravatarSpec where

import Network.Gravatar
import Test.Hspec
import Data.Monoid ((<>))

import qualified Data.Text as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "gravatar" $ do
    let email = "pbrisbin@gmail.com"
        md5sum = "2be502055b6c21ff470730beead2a998"

    it "generates the correct URL by hashing the given email" $
        gravatar def email `shouldBe`
            "https://www.gravatar.com/avatar/" <> md5sum

    it "works regardless of whitespace" $
        gravatar def (" " <> email <> "   ") `shouldBe`
            "https://www.gravatar.com/avatar/" <> md5sum

    it "works regardless of case" $
        gravatar def (T.toUpper email) `shouldBe`
            "https://www.gravatar.com/avatar/" <> md5sum

    it "can be configured via GravatarOptions" $ do
        let opts = GravatarOptions
                { gSize = Just $ Size 24
                , gDefault = Just NotFound
                , gForceDefault = ForceDefault True
                , gRating = Just G
                , gScheme = Http
                }
            query = "?s=24&d=404&f=y&r=g"

        gravatar opts email `shouldBe`
            "http://www.gravatar.com/avatar/" <> md5sum <> query
