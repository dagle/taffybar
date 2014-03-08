module System.Taffybar.TwitchMonitor (
    Quality(Source, High, Medium, Low, Mobile)
    , TwitchMonitor(TwitchMonitor)
    , twitchTextMonitorNew
    , twitchIconMonitorNew
    , defaultTwitchConfig
    , quality
    , streams
    , wrapper
) where

import Graphics.UI.Gtk
import System.Information.Twitch
import System.Taffybar.Widgets.AttachedPopup
import System.Taffybar.Widgets.TextImage
import Control.Concurrent.STM

data Quality = Source | High | Medium | Low | Mobile deriving Show

data TwitchMonitor = TwitchMonitor {
    streams :: [String]
    , quality :: Quality
    , wrapper :: String -> String
}

defaultTwitchConfig :: TwitchMonitor
defaultTwitchConfig = TwitchMonitor ["twoeasy"] Medium $ id -- $ generateLink "http:" "twitch:"

twitchListCfg :: (TwitchMonitor -> (b, c) -> IO ()) -> TwitchMonitor -> PopupConfig TwitchMonitor b c
twitchListCfg f cfg = PopupConfig f
            (const . const $ return ()) (wrapper cfg) "Twitch" cfg

twitchTextMonitorNew :: TwitchMonitor -> String -> IO Widget
twitchTextMonitorNew cfg lab = do
    pollingPopupNew (twitchListCfg (getChannels lab) cfg)
        (labelNew Nothing) (labelNew Nothing) 300

twitchIconMonitorNew :: TwitchMonitor -> IO Widget
twitchIconMonitorNew cfg = do
    var <- newTVarIO ""
    pollingPopupNew (twitchListCfg (updateIcon var) cfg)
         (textImageNew "/home/dagle/twitch.png" (atomically $ readTVar var)) (labelNew Nothing) 300

getStrings :: TwitchMonitor -> IO (String, String)
getStrings cfg = do 
    channels <- mapM getTwitch $ streams cfg
    let online = filter getOnline channels
    return $ (show (length online), 
        flat $ map (\x -> linkRewrite $ app3 getName getStream getTopic x) online)

updateIcon :: TVar String -> TwitchMonitor -> (Widget, Label) -> IO ()
updateIcon var cfg (header, body) = do
    (h, l) <- getStrings cfg
    atomically $ writeTVar var h
    labelSetMarkup body l >> widgetQueueDraw header

getChannels :: String -> TwitchMonitor -> (Label, Label) -> IO ()
getChannels lab cfg (header, body) = do
    (h, l) <- getStrings cfg
    labelSetMarkup header (lab ++ h) >> labelSetMarkup body l
