{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Test where

import Network.Gravatar
import Network.Wai.Handler.Warp (run)
import Yesod.Core

data App = App
mkYesod "App" [parseRoutes|/ RootR GET|]
instance Yesod App where

getRootR :: Handler Html
getRootR = defaultLayout
    [whamlet|<img src=#{gravatar def "pbrisbin@gmail.com"}>|]

main :: IO ()
main = run 3000 =<< toWaiApp App
