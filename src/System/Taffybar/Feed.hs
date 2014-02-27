module System.Taffybar.Feed (
    FeedConfig(FeedConfig)
    , defaultFeedConfig
    , feedTextMonitorNew
    , feedIconMonitorNew
) where 

import Text.Feed.Import
import Text.Feed.Query
import Network.Curl
import Data.Maybe
import System.Taffybar.Widgets.AttachedPopup
import System.Taffybar.Widgets.TextImage
import Graphics.UI.Gtk
import Control.Concurrent.STM

type URI = String

data FeedConfig = FeedConfig {
    feeds :: URI -- start with one feed for now
    , wrapper :: String -> String
}

defaultFeedConfig :: FeedConfig
defaultFeedConfig = FeedConfig "http://planet.haskell.org/atom.xml" id

feedTextMonitorNew :: FeedConfig -> IO Widget
feedTextMonitorNew cfg = do
    let feedListCfg = PopupConfig getLabel
            (const . const $ return ()) id "Feed" cfg
    pollingPopupNew feedListCfg (labelNew Nothing) (labelNew Nothing) 300

feedIconMonitorNew :: FeedConfig -> IO Widget
feedIconMonitorNew cfg = do
    var <- newTVarIO ""
    let feedListCfg = PopupConfig (getIcon var)
            (const . const $ return ()) id "Feed" cfg
    pollingPopupNew feedListCfg (textImageNew "/home/dagle/rss.png" (atomically $ readTVar var)) 
            (labelNew Nothing) 300

parseFeed :: String -> IO [(String, String, String)]
parseFeed str = do
    let feed = parseFeedString str
        items = getFeedItems $ fromJust feed
        tri = map (\item -> (getItemTitle item, getItemLink item,  getItemDescription item)) items
        pairs = filter ((/= Nothing) . fst') tri
        r = map (\(x,y,z) -> (fromJust x, maybe "" id y, maybe "" id z)) pairs
    case feed of
        Nothing -> return []
        (Just _) -> return r

fst' :: (t, t1, t2) -> t
fst' (a,_,_) = a

getStrings :: FeedConfig -> IO (String, String)
getStrings cfg = do 
    feeds' <- fetch $ feeds cfg
    let hStr = show (length feeds')
    let lStr = flat $ take 5 $ map linkRewrite feeds'
    return (hStr, lStr)

-- Writes the header and the links to the labellist
getLabel :: FeedConfig -> (Label, Label) -> IO ()
getLabel cfg (header, body) = do
    (hStr, lStr) <- getStrings cfg
    postGUIAsync $ labelSetMarkup body lStr >> labelSetMarkup header hStr

getIcon :: (WidgetClass self1, LabelClass self) => 
        TVar String -> FeedConfig -> (self1, self) -> IO ()
getIcon tvar cfg (header, body) = do
    (hStr, lStr) <- getStrings cfg
    atomically $ writeTVar tvar hStr
    postGUIAsync $ labelSetMarkup body lStr >> widgetQueueDraw header

fetch :: URLString -> IO [(String, String, String)]
fetch feed = do
    feed' <- curlGetString feed []
    case feed' of
        (CurlOK, str) -> parseFeed str
        _ -> return []

