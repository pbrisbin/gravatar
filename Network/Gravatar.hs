-------------------------------------------------------------------------------
-- |
-- Module      :  Network.Gravatar
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- <http://en.gravatar.com/>.
--
-------------------------------------------------------------------------------
module Network.Gravatar
    ( gravatar

    -- * Options
    , GravatarOptions(..)
    , Size(..)
    , DefaultImg(..)
    , ForceDefault(..)
    , Rating(..)
    , defaultConfig
    ) where

import Data.Digest.Pure.MD5 (md5)
import Data.Default         (Default(..))
import Data.List            (intercalate)
import Data.Maybe           (catMaybes)
import Data.Text            (Text)
import Network.HTTP.Base    (urlEncode)

import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T

class GravatarParam a where
    toParam :: a -> Maybe (String, String)

-- | Size in pixels
newtype Size = Size Int

instance GravatarParam Size where
    toParam (Size i) = Just ("s", show i)

-- | Always show the default image
newtype ForceDefault = ForceDefault Bool

instance GravatarParam ForceDefault where
    toParam (ForceDefault b) = if b then Just ("f", "y") else Nothing

-- | Image to show when an avatar is not available
data DefaultImg = Custom String -- ^ supply your own url
                | NotFound      -- ^ do not load an image return a 404
                | MM            -- ^ mystery man
                | Identicon     -- ^ geometric pattern based on the hash
                | MonsterId     -- ^ a generated monster
                | Wavatar       -- ^ generated faces
                | Retro         -- ^ generated, 8-bit arcade style pixelated face

instance GravatarParam DefaultImg where
    toParam (Custom s) = Just ("d", urlEncode s)
    toParam NotFound   = Just ("d", "404"      )
    toParam MM         = Just ("d", "mm"       )
    toParam Identicon  = Just ("d", "identicon")
    toParam MonsterId  = Just ("d", "monsterid")
    toParam Wavatar    = Just ("d", "wavatar"  )
    toParam Retro      = Just ("d", "retro"    )

-- | Limit the returned images by rating
data Rating = G | PG | R | X

instance GravatarParam Rating where
    toParam G  = Just ("r", "g" )
    toParam PG = Just ("r", "pg")
    toParam R  = Just ("r", "r" )
    toParam X  = Just ("r", "x" )

data GravatarOptions = GravatarOptions
    { gSize         :: Maybe Size
    , gDefault      :: Maybe DefaultImg
    , gForceDefault :: ForceDefault
    , gRating       :: Maybe Rating
    }

instance Default GravatarOptions where
    def = defaultConfig

defaultConfig :: GravatarOptions
defaultConfig = GravatarOptions
    { gSize         = Nothing
    , gDefault      = Nothing
    , gForceDefault = ForceDefault False
    , gRating       = Nothing
    }

-- | Return the avatar for the given email using the provided options 
gravatar :: GravatarOptions -> Text -> String
gravatar opts e = "http://www.gravatar.com/avatar/" ++ hashEmail e `addParams` opts

-- | <http://en.gravatar.com/site/implement/hash/>
hashEmail :: Text -> String
hashEmail = md5sum . T.toLower . T.strip

    where
        md5sum :: Text -> String
        md5sum = show . md5 . C8.pack . T.unpack

addParams :: String -> GravatarOptions -> String
addParams url opts = helper url . map (\(k,v) -> k ++ "=" ++ v)
                   $ catMaybes [ toParam =<< gSize         opts
                               , toParam =<< gDefault      opts
                               , toParam   $ gForceDefault opts
                               , toParam =<< gRating       opts
                               ]
    where
        helper :: String -> [String] -> String
        helper u [] = u
        helper u l  = (++) u . (:) '?' $ intercalate "&" l
