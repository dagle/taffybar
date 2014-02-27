module System.Taffybar.Widgets.AttachedPopup (
    PopupConfig(PopupConfig)
    , pollingPopupNew
    , popupBoxNew
    , generateLink
    , linkRewrite
    , app3
    , flat
    ) where

import Control.Concurrent ( forkIO, threadDelay )
import Control.Exception as E
import Control.Monad
import Graphics.UI.Gtk
import Control.Monad.Trans
import System.Taffybar.Widgets.Util
import System.Cmd
import Text.Regex

data PopupConfig a b c = PopupConfig {
    pollingAction :: a -> (b, c) -> IO () -- action run while polling
    , showAction :: a -> (b, c) -> IO () -- Action run when you show the window
    , linkAction :: String -> String -- A rewring rule
    , windowName :: String -- The name of the popup window
    , bulk :: a -- A holder for data that is used during callbacks
}

-- | Run a both polling and eventdriven list
pollingPopupNew :: (WidgetClass b, WidgetClass c) => 
                  PopupConfig a b c
                  -> IO b
                  -> IO c
                  -> Double
                  -> IO Widget
pollingPopupNew cfg header body pollSeconds = do 
  let action = pollingAction cfg
  (w, head', list') <- popupBoxNew cfg header body

  _ <- on w realize $ do
       _ <- forkIO $ forever $ do
         E.catch (postGUIAsync $ action (bulk cfg) (head', list')) ignoreIOException
         threadDelay $ floor (pollSeconds * 1000000)
       return ()
  return $ toWidget w

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()

flat :: [String] -> String
flat [] = ""
flat [x] = x
flat (x:xs) = x ++ "\n" ++ flat xs

-- should pack these boxes
-- | Run a eventdriven list
popupBoxNew :: (WidgetClass b, WidgetClass c) => PopupConfig a b c -> IO b -> IO c -> IO (Widget, b, c)
popupBoxNew cfg header body = do
    let action = showAction cfg
    l <- header
    (container, box) <- windowBoxNew cfg body
    ebox <- eventBoxNew
    containerAdd ebox l
    eventBoxSetVisibleWindow ebox False
    _ <- on ebox buttonPressEvent $ onClick [SingleClick] $ toggle l container (windowName cfg)
    _ <- onShow container $ liftIO $ action (bulk cfg) (l, box)
    widgetShowAll ebox
    return (toWidget ebox, l, box)

windowBoxNew :: WidgetClass c => PopupConfig a  b  c -> IO c -> IO (Window, c)
windowBoxNew cfg body = do
    container <- windowNew
    box <- body
    widgetSetName box (windowName cfg)
    containerAdd container box
    -- _ <- onActiveLink box $ linkClicked cfg
    _ <- on container deleteEvent $ do
        liftIO (widgetHideAll container)
        return True
    return (container, box)

-- should use xgd-open or something but I hate xdg-open to much
linkClicked :: PopupConfig a b c -> t -> String -> IO ()
linkClicked cfg _ link = do
    _ <- system $ "plumb " ++ (linkAction cfg) link
    return ()

app3 :: (a -> String) -> (a -> String) -> (a -> String)
            -> a -> (String, String, String)
app3 f1 f2 f3 datum = 
    (f1 datum, f2 datum, f3 datum)

linkRewrite :: (String, String, String) -> String
linkRewrite (name, "", _) = name
linkRewrite (name, link, topic) = "<a href='" ++ escapeMarkup link ++ "' title='" ++ escapeMarkup topic ++ "'>" ++ escapeMarkup name ++ "</a>"

-- | Helper function for writing wrapper rules
-- Example rewriting http:// to twitch://
-- so xdg-open can open it with the correct program
generateLink :: String -> String -> String -> String
generateLink regex sub str = subRegex (mkRegex regex) str sub

toggle :: WidgetClass w => w -> Window -> String -> IO ()
toggle w c s = do
    isVis <- get c widgetVisible
    if isVis
        then widgetHideAll c
        else do
            attachPopup w s c
            displayPopup w c
