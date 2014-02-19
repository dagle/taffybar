module System.Taffybar.Widgets.PollingList (
    ListConfig(ListConfig)
    , pollingListNew
    , listBoxNew
    ) where

import Control.Concurrent ( forkIO, threadDelay )
import Control.Exception as E
import Control.Monad
import Graphics.UI.Gtk
import Control.Monad.Trans
import System.Taffybar.Widgets.Util

-- this "design" works, but it's not consistant and I can't tell whats nicer:
-- let this lib handle callbacks or exporting helping functions for implementing callbacks.

data ListConfig a = ListConfig {
    maxObjects :: Int
    , pollingAction :: a -> IO (String, [String]) -- action run while polling
    , showAction :: a -> [Label] -> IO () -- Action run when you click on the label
    , clickAction :: a -> Label -> IO Bool -- A action run when you click on a label
    , windowName :: String -- The name of the popup window
    , bulk :: a -- A holder for data that is used during callbacks
}

-- | Run a both polling and eventdriven list
pollingListNew :: ListConfig a
                  -> Double
                  -> IO Widget
pollingListNew cfg pollSeconds = do 
  let action = pollingAction cfg
  (w, header, list) <- listBoxNew cfg

  _ <- on w realize $ do
       _ <- forkIO $ forever $ do
         (h, l) <- action (bulk cfg)
         updateList header list h l
         threadDelay $ floor (pollSeconds * 1000000)
       return ()
  return $ toWidget w

updateList :: (LabelClass self, LabelClass a) => self -> [a] -> [Char] -> [Markup] -> IO ()
updateList header list h l = do
    let tryUpdate = do
            postGUIAsync $ labelSetMarkup header h
            postGUIAsync $ sequence_ $ zipWith labelSetMarkup list l
    E.catch tryUpdate ignoreIOException

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()

-- should pack these boxes
-- | Run a eventdriven list
listBoxNew :: ListConfig a -> IO (Widget, Label, [Label])
listBoxNew cfg = do
    let action = showAction cfg
    l <- labelNew Nothing
    (container, boxes) <- windowBoxNew cfg
    ebox <- eventBoxNew
    containerAdd ebox l
    eventBoxSetVisibleWindow ebox False
    _ <- on ebox buttonPressEvent $ onClick [SingleClick] $ toggle l container (windowName cfg)
    _ <- onShow container $ liftIO $ action (bulk cfg) boxes
    widgetShowAll ebox
    return (toWidget ebox, l, boxes)

-- run action when an list item is clicked
click :: ListConfig a -> Label -> IO Bool
click cfg label = (clickAction cfg) (bulk cfg) label

-- create a list of labels with actions connected
-- Does in really need to be this complex? It's ok for now
windowBoxNew :: ListConfig a -> IO (Window, [Label])
windowBoxNew cfg = do
    let maxboxs = maxObjects cfg
    container <- windowNew
    boxes <- mapM (const $ labelNew Nothing) [1..maxboxs]
    events <- mapM (const $ eventBoxNew) [1..maxboxs]
    zipWithM_ containerAdd events boxes
    mapM_ (containerAdd container) events
    zipWithM_ (\x y -> on x buttonPressEvent $ onClick [SingleClick] $ click cfg y) events boxes
    _ <- on container deleteEvent $ do
        liftIO (widgetHideAll container)
        return True
    return (container, boxes)

toggle :: WidgetClass w => w -> Window -> String -> IO ()
toggle w c s = do
    isVis <- get c widgetVisible
    if isVis
        then widgetHideAll c
        else do
            attachPopup w s c
            displayPopup w c
