{-# LANGUAGE OverloadedStrings #-}
module Network.GravatarSpec where

import Network.Gravatar

import Data.Monoid ((<>))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "gravatar" $
    it "generates the correct URL by hashing the given email" $ do
        let hsh = "2be502055b6c21ff470730beead2a998"
            url = "https://www.gravatar.com/avatar/" <> hsh

        gravatar def "pbrisbin@gmail.com" `shouldBe` url
