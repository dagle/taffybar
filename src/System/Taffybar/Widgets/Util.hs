-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widgets.Util
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Utility functions to facilitate building GTK interfaces.
--
-----------------------------------------------------------------------------

module System.Taffybar.Widgets.Util (
    onClick
    , attachPopup
    , displayPopup
    , expandPath
    ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk
import Data.List.Split
import Data.List
import System.Environment

-- | This function does tilde and environment variable expansion
-- you can then use glob on this or just return a single file
--expandPath :: FilePath -> IO FilePath
expandPath path = do
    let detilde = changeTilde path
        dirs = splitOn "/" $ detilde
        vars = map (splitter "$") dirs
    str <- mapM (mapM expand) vars
    return $ merge str

-- | Changes a tilde to $HOME
changeTilde :: String -> String
changeTilde s = concat $ map (\x -> if x == '~' then "$HOME" else [x]) s

merge x = let slashes = map (map (\x -> x ++ "/")) $ init x
          in concat $ concat $ slashes ++ [last x]

splitter :: Eq a => [a] -> [a] -> [[a]]
splitter = split . keepDelimsL . onSublist

expand [] = return []
expand x = case x of
    ('$':'{':xs) -> do str <- getEnv $ takeWhile ((/=) '}') xs
                       let rest = tail $ dropWhile ((/=) '}') xs
                       return $ str ++ rest
    ('$':xs) -> getEnv xs
    _ -> return x

-- | Execute the given action as a response to any of the given types
-- of mouse button clicks.
onClick :: [Click] -- ^ Types of button clicks to listen to.
        -> IO a    -- ^ Action to execute.
        -> EventM EButton Bool
onClick triggers action = tryEvent $ do
  click <- eventClick
  when (click `elem` triggers) $ liftIO action >> return ()

-- | Attach the given widget as a popup with the given title to the
-- given window. The newly attached popup is not shown initially. Use
-- the 'displayPopup' function to display it.
attachPopup :: (WidgetClass w) => w -- ^ The widget to set as popup.
            -> String         -- ^ The title of the popup.
            -> Window         -- ^ The window to attach the popup to.
            -> IO ()
attachPopup widget title window = do
  set window [ windowTitle := title
             , windowTypeHint := WindowTypeHintTooltip
             , windowSkipTaskbarHint := True
             ]
  windowSetSkipPagerHint window True
  windowSetKeepAbove window True
  windowStick window
  Just topLevel <- widgetGetAncestor widget gTypeWindow
  let topLevelWindow = castToWindow topLevel
  windowSetTransientFor window topLevelWindow

-- | Display the given popup widget (previously prepared using the
-- 'attachPopup' function) immediately beneath (or above) the given
-- window.
displayPopup :: (WidgetClass w) => w -- ^ The popup widget.
             -> Window -- ^ The window the widget was attached to.
             -> IO ()
displayPopup widget window = do
  windowSetPosition window WinPosMouse
  (x, y ) <- windowGetPosition window
  (_, y') <- widgetGetSize widget
  widgetShowAll window
  if y > y'
    then windowMove window x (y - y')
    else windowMove window x y'
