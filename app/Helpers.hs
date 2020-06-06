module Helpers
    ( expandPath
    , notify
    , sleep
    , capitalize
    , fromEither
    , fromMaybe
    , NotificationType(..)
    )
where

import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import DBus (methodCall, methodCallDestination, methodCallBody, toVariant, Variant)
import DBus.Client (callNoReply, Client)
import Data.Int (Int32)
import Data.Word (Word32)
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.Map as M

data NotificationType
    = Weather
    | Mpd
    | Twitch
    deriving (Show, Eq)

-- | Send a desktop notification via DBus
notify :: Client -> NotificationType -> Text -> Text -> Maybe Text -> NominalDiffTime -> IO ()
notify client nType title text icon timeout = callNoReply client params
  where
    params = (methodCall objectPath interface methodName)
        { methodCallDestination = Just "org.freedesktop.Notifications"
        , methodCallBody        = args
        }
    objectPath = "/org/freedesktop/Notifications"
    interface  = "org.freedesktop.Notifications"
    methodName = "Notify"
    appName    = "ntfd" :: Text
    appIcon    = fromMaybe icon
    replaceId Weather = 1
    replaceId Mpd     = 2
    replaceId Twitch  = 3
    args =
        [ toVariant appName
        , toVariant (replaceId nType :: Word32)
        , toVariant appIcon
        , toVariant title
        , toVariant text
        , toVariant ([] :: [String])
        , toVariant (M.fromList [] :: M.Map String Variant)
        , toVariant (toSeconds timeout :: Int32)
        ]
    toSeconds = (1000 *) . fromInteger . round

expandPath :: FilePath -> IO FilePath
expandPath ('~' : '/' : p) = do
    home <- getHomeDirectory
    pure $ joinPath [home, p]
expandPath p = pure p

-- | Wait for a given amount of time
sleep :: NominalDiffTime -> IO ()
sleep = threadDelay . toMicroSeconds where toMicroSeconds = (1000000 *) . fromInteger . round

-- | Capitalize a Text string, first character will be uppercased
-- and the rest will be lowercased
capitalize :: Text -> Text
capitalize text = case T.uncons text of
    Nothing     -> text
    Just (h, t) -> let lowered = T.toLower t in T.cons (C.toUpper h) lowered

-- The following functions help mapping our data types to work around DBus' lack of null type
-- Usefull to return empty strings
fromEither :: (Monoid t) => Either e t -> t
fromEither (Left  _) = mempty
fromEither (Right t) = t

fromMaybe :: (Monoid t) => Maybe t -> t
fromMaybe Nothing  = mempty
fromMaybe (Just t) = t
