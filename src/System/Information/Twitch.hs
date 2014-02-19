{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module System.Information.Twitch (
    getTwitch
    , getOnline
    , getName
    , getStream

    , Twitch(Twitch)
    , twitchStream
    , twitch_Links

    , Self(Self)
    , selfSelf
    , selfChannel

    , Stream(Stream)
    , stream_Id
    , streamGame
    , streamViewers
    , streamPreview
    , stream_Links
    , streamChannel

    , Channel(Channel)
    , channelStatus
    , channelGame
    , channelUrl
    , channelViews
    , channelFollowers

) where 

import Network.Curl
import Data.Aeson.TH
import Data.Aeson
import GHC.Generics
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char
import Data.Maybe

data Twitch = Twitch {
    twitch_Links :: Self
    , twitchStream :: Maybe Stream
    } deriving (Eq, Generic, Show)

data Self = Self {
    selfSelf :: String
    , selfChannel :: String
    } deriving (Eq, Generic, Show)

data Stream = Stream {
    stream_Id :: Int
    , streamGame :: String
    , streamViewers :: Int
    , streamPreview :: Preview
    , stream_Links :: Self2
    , streamChannel :: Channel
    } deriving (Eq, Generic, Show)

data Preview = Preview {
    _previewSmall :: String
    , _previewMedium :: String
    , _previewLarge :: String
    , _previewTemplate :: String
} deriving (Eq, Generic, Show)

data Channel = Channel {
    _channelMature :: Maybe Bool
    , _channelAbuse_reported :: Maybe Int
    , channelStatus :: String
    , _channelDisplay_name :: String
    , channelGame :: String
    , _channelDelay :: Int
    , _channel_Id :: Int
    , channelName :: String
    , _channelCreated_at :: String
    , _channelUpdated_at :: String
    , _channelPrimary_team_name :: Maybe String
    , _channelPrimary_team_display_name :: Maybe String
    , _channelLogo :: String
    , _channelBanner :: String
    , _channelVideo_banner :: String
    , _channelBackground :: String
    , _channelProfile_banner :: Maybe String
    , _channelProfile_banner_background_color :: Maybe String
    , channelUrl :: String
    , channelViews :: Int
    , channelFollowers :: Int
    , _channel_Links :: Links
} deriving (Eq, Generic, Show)

data Links = Links {
    _linksSelf :: String
    , _linksFollows :: String
    , _linksCommercial :: String
    , _linksStream_key :: String
    , _linksChat :: String
    , _linksFeatures :: String
    , _linksSubscriptions :: Maybe String
    , _linksEditors :: String
    , _linksTeams :: String
    , _linksVideos :: String
} deriving (Eq, Generic, Show)

data Self2 = Self2 {
    _self2Self :: String
} deriving (Eq, Generic, Show)

--defaultTwitchConfig = TwitchConfig $ (++) "livestreamer -p mpv "
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 6 . dropWhile ((==) '_')} ''Twitch)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 4 . dropWhile ((==) '_')} ''Self)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 6 . dropWhile ((==) '_')} ''Stream)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 7 . dropWhile ((==) '_')} ''Preview)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 5 . dropWhile ((==) '_')} ''Self2)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 5 . dropWhile ((==) '_')} ''Links)
$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 7 . dropWhile ((==) '_')} ''Channel)


getTwitch :: String -> IO (Maybe Twitch)
getTwitch url = do
    response <- curlGetString ("https://api.twitch.tv/kraken/streams/" ++ url) []
    case response of
        (CurlOK, str) ->
            let jm = decode $ pack str :: Maybe Twitch
            in return jm
        _ -> return Nothing

getOnline :: Maybe Twitch -> Bool
getOnline = maybe False ((/=) Nothing . twitchStream)

getName :: Maybe Twitch -> String
getName stream = maybe "" (channelName . streamChannel) $ stream >>= twitchStream

getStream :: Maybe Twitch -> String
getStream t =
    if getOnline t then channelUrl . streamChannel . fromJust . twitchStream . fromJust $ t else ""
