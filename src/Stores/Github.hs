module Stores.Github
    ( Store(..)
    , Error(..)
    , GithubNotification(..)
    , RestNotification(..)
    , GithubClient
    )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent (newEmptyMVar, tryReadMVar, tryTakeMVar, putMVar, MVar)
import Control.Monad (when)
import Data.Aeson ((.=), object)
import Data.Bifunctor (first)
import Data.Either (rights)
import Data.List ((\\))
import Data.Text (Text)
import Data.Text.Lazy (toStrict, fromStrict)
import GHC.Natural (naturalFromInteger)
import Numeric.Natural (Natural)
import Text.Microstache (compileMustacheText, renderMustacheW, MustacheWarning)
import Text.Parsec (ParseError)

import Clients.Github (fetchNotifications, getAvatarPath, RestNotification(..))
import Config (GithubConfig(..))
import qualified Clients.Github as Gh

-- | Query and update Github data.
class Store s where
    -- | Initialize the store
    initGithubStore :: GithubConfig -> IO s
    -- | Synchronize local data with the Github API.
    -- Returns an array of notification IDs for which we should send a desktop notification
    syncGithub :: s ->  IO (Either Error [Text])
    -- | Get all notifications currently marked as unread from the inbox
    getNotifications :: s -> IO [GithubNotification]
    -- | Get the number of unread notifications
    -- (everything marked as unread in your inbox)
    getUnreadNotificationCount :: s -> IO (Maybe Natural)
    -- | Get the number of brand new notifications
    -- (notifications from an issue, PR ... that you have never seen so far)
    getFistTimeNotificationCount :: s -> IO (Maybe Natural)
    -- | Get a rendered version of the configured template
    getRenderedTemplate :: s -> IO (Either Error Text)

-- Main client, implements the Store typeclass
data GithubClient = GithubClient
    { config :: GithubConfig
    , internalState :: MVar InternalState
    }

-- Holds internal state: rendered template and notification data from Github
data InternalState = InternalState
    { githubData :: GithubData
    , renderedTemplate :: Either Error Text
    }

-- Actual github data
data GithubData = GithubData
    { unreadNotificationCount :: Natural
    , firstTimeNotificationCount :: Natural
    , notifications :: [GithubNotification]
    }

-- A Github notification, adds properties on top of the raw Rest structure
data GithubNotification = GithubNotification
    { restNotification :: RestNotification
    , avatarPath :: FilePath
    } deriving (Show)

instance Store GithubClient where
    initGithubStore config = do
        internalState <- newEmptyMVar
        pure GithubClient { .. }

    syncGithub s = do
        notifs <- fetchNotifications cfg
        case notifs of
            Right restNotifs -> do
                -- Build new state
                notifications <- toStoreNotifs restNotifs
                let unreadNotificationCount    = unreadCount restNotifs
                    firstTimeNotificationCount = firstTimeCount restNotifs
                    gData                      = GithubData { .. }
                    renderedTemplate           = renderTemplate gData template
                    state                      = InternalState { githubData = gData, .. }
                -- Update store
                oldState <- tryTakeMVar mvar
                putMVar mvar state
                pure $ Right $ shouldNotify oldState state
            -- Report failure otherwise
            Left e -> pure $ Left $ GithubApi e
      where
        cfg            = config s
        mvar           = internalState s
        template       = githubTemplate cfg
        unreadCount    = naturalFromInteger . fromIntegral . length
        firstTimeCount = naturalFromInteger . fromIntegral . length . filter isNew
        shouldNotify maybeOldState newState = case maybeOldState of
            Just oldState -> notificationIds newState \\ notificationIds oldState
            Nothing       -> notificationIds newState
        notificationIds ns =
            map (notificationId . restNotification) $ (notifications . githubData) ns
        toStoreNotifs ns = do
            notifications <- mapConcurrently toStoreNotif ns
            pure $ rights notifications
        toStoreNotif restNotification = do
            let name = repoFullName restNotification
            let url  = avatarUrl restNotification
            path <- getAvatarPath cfg name url
            pure $ path >>= \avatarPath -> Right GithubNotification { .. }

    getNotifications s = do
        state <- tryReadMVar $ internalState s
        pure $ maybe [] (notifications . githubData) state

    getUnreadNotificationCount s = do
        state <- tryReadMVar $ internalState s
        pure $ unreadNotificationCount . githubData <$> state

    getFistTimeNotificationCount s = do
        state <- tryReadMVar $ internalState s
        pure $ firstTimeNotificationCount . githubData <$> state

    getRenderedTemplate s = do
        state <- tryReadMVar $ internalState s
        pure $ case state of
            Nothing -> Left Unsynchronized
            Just st -> renderedTemplate st

-- brittany-disable-next-binding
renderTemplate :: GithubData -> Text -> Either Error Text
renderTemplate gh t = do
    -- Compile user provided template
    template <- first Parse $ compileMustacheText "github template" $ fromStrict t
    -- Make sure we have some unread notifications
    when (unreadNotificationCount gh == 0) $ Left NoUnread
    -- Render it
    case renderMustacheW template payload of
        ([]  , res) -> Right $ toStrict res
        (errs, _  ) -> Left $ Render errs
  where
    payload = object
        [ "unread_count" .= notifCount
        , "first_time_count" .= firstTimeCount
        ]
    notifCount = length $ notifications gh
    firstTimeCount = length $ filter isNew $ map restNotification (notifications gh)

-- | Error types the store might return.
data Error
    = Parse ParseError
    | NoUnread
    | Render [MustacheWarning]
    | Unsynchronized
    | GithubApi Gh.Error
    deriving (Show)
