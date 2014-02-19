module System.Taffybar.TwitchMonitor (
    Quality(Source, High, Medium, Low, Mobile)
    , TwitchMonitor
    , twitchMonitorNew
) where

import Graphics.UI.Gtk
import System.Information.Twitch
import System.Taffybar.Widgets.PollingList
import System.Cmd
import Data.Char

data Quality = Source | High | Medium | Low | Mobile deriving Show

data TwitchMonitor = TwitchMonitor {
    streams :: [String]
    , quality :: Quality
    , cmd :: String
    --, wapper :: String -> String
}

twitchMonitorNew :: TwitchMonitor -> IO Widget
twitchMonitorNew cfg = do
    let twitchListCfg = ListConfig 5 getChannels
            (const . const $ return ()) clicking "Twitch" cfg
    pollingListNew twitchListCfg 300

getChannels :: TwitchMonitor -> IO ([Char], [String])
getChannels cfg = do
    channels <- mapM getTwitch $ streams cfg
    let online = filter getOnline channels
    return ("Twitch: " ++ show (length online), map getName online)

clicking :: LabelClass self => TwitchMonitor -> self -> IO Bool
clicking cfg label = do
    str <- labelGetLabel label
    stream <- getTwitch str
    if getOnline stream
        then system ((cmd cfg) ++ " " ++ getStream stream ++ " " ++ map toLower (show $ quality cfg)) >> return True
        else return True

