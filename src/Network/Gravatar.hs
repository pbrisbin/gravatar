module Network.Gravatar
    ( gravatar

    -- * Options
    , GravatarOptions(..)
    , Size(..)
    , DefaultImg(..)
    , ForceDefault(..)
    , Rating(..)
    , Scheme(..)
    , def
    , defaultConfig
    ) where

import Data.Default (Default(..))
import Data.Digest.Pure.MD5 (md5)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Network.HTTP.Base (urlEncode)

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
    toParam (ForceDefault True) = Just ("f", "y")
    toParam (ForceDefault False) = Nothing

-- | Image to show when an avatar is not available
data DefaultImg
    = Custom String -- ^ supply your own url
    | NotFound      -- ^ do not load an image return a 404
    | MM            -- ^ mystery man
    | Identicon     -- ^ geometric pattern based on the hash
    | MonsterId     -- ^ a generated monster
    | Wavatar       -- ^ generated faces
    | Retro         -- ^ generated, 8-bit arcade style pixelated face

instance GravatarParam DefaultImg where
    toParam (Custom s) = Just ("d", urlEncode s)
    toParam NotFound = Just ("d", "404")
    toParam MM = Just ("d", "mm")
    toParam Identicon = Just ("d", "identicon")
    toParam MonsterId = Just ("d", "monsterid")
    toParam Wavatar = Just ("d", "wavatar")
    toParam Retro = Just ("d", "retro")

-- | Limit the returned images by rating
data Rating = G | PG | R | X

instance GravatarParam Rating where
    toParam G = Just ("r", "g")
    toParam PG = Just ("r", "pg")
    toParam R = Just ("r", "r")
    toParam X = Just ("r", "x")

data GravatarOptions = GravatarOptions
    { gSize :: Maybe Size           -- ^ default @Nothing@
    , gDefault :: Maybe DefaultImg  -- ^ default @Nothing@
    , gForceDefault :: ForceDefault -- ^ default @False@
    , gRating :: Maybe Rating       -- ^ default @Nothing@
    , gScheme :: Scheme             -- ^ default @Https@
    }

-- | Scheme to use for image URLs
data Scheme
    = Http  -- ^ @http://@
    | Https -- ^ @https://@
    | None  -- ^ @//@

instance Show Scheme where
  show Http = "http://"
  show Https = "https://"
  show None = "//"

instance Default GravatarOptions where
    def = defaultConfig

-- | Available for backwards compatability, using @def@ is advised
defaultConfig :: GravatarOptions
defaultConfig = GravatarOptions
    { gSize = Nothing
    , gDefault = Nothing
    , gForceDefault = ForceDefault False
    , gRating = Nothing
    , gScheme = Https
    }

-- | Return the avatar for the given email using the provided options
--
-- >>> gravatar def "pbrisbin@gmail.com"
-- "https://www.gravatar.com/avatar/2be502055b6c21ff470730beead2a998"
--
-- Whitespace is trimmed.
--
-- >>> gravatar def " pbrisbin@gmail.com   "
-- "https://www.gravatar.com/avatar/2be502055b6c21ff470730beead2a998"
--
-- Case is ignored.
--
-- >>> gravatar def "PBrisbin@GMAIL.com"
-- "https://www.gravatar.com/avatar/2be502055b6c21ff470730beead2a998"
--
-- Options are supported.
--
-- >>> :{
--   let opts = GravatarOptions
--           { gSize = Just $ Size 24
--           , gDefault = Just NotFound
--           , gForceDefault = ForceDefault True
--           , gRating = Just G
--           , gScheme = Http
--           }
--   in gravatar opts "pbrisbin@gmail.com"
-- :}
-- "http://www.gravatar.com/avatar/2be502055b6c21ff470730beead2a998?s=24&d=404&f=y&r=g"
--
gravatar :: GravatarOptions -> Text -> String
gravatar opts e = concat
    [ show $ gScheme opts
    , "www.gravatar.com/avatar/"
    , hashEmail e
    , queryString opts
    ]

-- | <http://en.gravatar.com/site/implement/hash/>
hashEmail :: Text -> String
hashEmail = show . md5 . C8.pack . T.unpack . T.toLower . T.strip

queryString :: GravatarOptions -> String
queryString opts = case queryParts of
    [] -> ""
    ps -> "?" ++ intercalate "&" (map queryPart ps)

  where
    queryParts :: [(String, String)]
    queryParts = catMaybes
        [ toParam =<< gSize opts
        , toParam =<< gDefault opts
        , toParam   $ gForceDefault opts
        , toParam =<< gRating opts
        ]

    queryPart :: (String, String) -> String
    queryPart (k, v) = k ++ "=" ++ v
