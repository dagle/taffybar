-----------------------------------------------------------------------------
-- |
-- Module      : System.Information.EWMHDesktopInfo
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Functions to access data provided by the X11 desktop via EWHM hints. This
-- module requires that the EwmhDesktops hook from the XMonadContrib project
-- be installed in your @~\/.xmonad\/xmonad.hs@ configuration:
--
-- > import XMonad
-- > import XMonad.Hooks.EwmhDesktops (ewmh)
-- >
-- > main = xmonad $ ewmh $ ...
--
-----------------------------------------------------------------------------

module System.Information.EWMHDesktopInfo
  ( X11Window      -- re-exported from X11DesktopInfo
  , withDefaultCtx -- re-exported from X11DesktopInfo
  , isWindowUrgent -- re-exported from X11DesktopInfo
  , getCurrentWorkspace
  , getCurrentScreen
  , getVisibleWorkspaces
  , getWorkspaceNames
  , switchToWorkspace
  , getWindowTitle
  , getWindowClass
  , getActiveWindowTitle
  , getWindows
  , getWindowHandles
  , getWorkspace
  , focusWindow
  ) where

import Data.List (elemIndex)
import Data.Maybe (listToMaybe, mapMaybe)
import System.Information.X11DesktopInfo
import Control.Monad

noFocus :: String
noFocus = "..."

-- | Retrieve the index of the current workspace in the desktop,
-- starting from 0.
getCurrentWorkspace :: X11Property Int
getCurrentWorkspace = readAsInt Nothing "_NET_CURRENT_DESKTOP"

getCurrentScreen :: X11Property Int
getCurrentScreen = readAsInt Nothing "_XMONAD_ACTIVE_SCREEN"

-- | Retrieve the indexes of all currently visible workspaces
-- with the active workspace at the head of the list.
getVisibleWorkspaces :: ([String] -> [String]) -> X11Property [Int]
getVisibleWorkspaces f = do
  vis <- getVisibleTags
  allNames <- liftM f getWorkspaceNames
  cur <- getCurrentWorkspace
  return $ cur : mapMaybe (`elemIndex` allNames) vis

-- | Return a list with the names of all the workspaces currently
-- available.
getWorkspaceNames :: X11Property [String]
getWorkspaceNames = readAsListOfString Nothing "_NET_DESKTOP_NAMES"

-- | Ask the window manager to switch to the workspace with the given
-- index, starting from 0.
switchToWorkspace :: Int -> X11Property ()
switchToWorkspace idx = do
  cmd <- getAtom "_NET_CURRENT_DESKTOP"
  sendCommandEvent cmd (fromIntegral idx)

-- | Get the title of the given X11 window.
getWindowTitle :: X11Window -> X11Property String
getWindowTitle window = do
  let w = Just window
  prop <- readAsString w "_NET_WM_NAME"
  case prop of
    "" -> readAsString w "WM_NAME"
    _  -> return prop

-- | Get the class of the given X11 window.
getWindowClass :: X11Window -> X11Property String
getWindowClass window = readAsString (Just window) "WM_CLASS"

withActiveWindow :: (X11Window -> X11Property String) -> X11Property String
withActiveWindow getProp = do
  awt <- readAsListOfWindow Nothing "_NET_ACTIVE_WINDOW"
  let w = listToMaybe $ filter (>0) awt
  maybe (return noFocus) getProp w

-- | Get the title of the currently focused window.
getActiveWindowTitle :: X11Property String
getActiveWindowTitle = withActiveWindow getWindowTitle

-- | Return a list of all windows
getWindows :: X11Property [X11Window]
getWindows = readAsListOfWindow Nothing "_NET_CLIENT_LIST"

-- | Return a list of pairs of (props, window) for all the windows open.
-- props is a pair of (window title, window class),
-- and window is the internal ID of one window.
getWindowHandles :: X11Property [((Int, String, String), X11Window)]
getWindowHandles = do
  windows <- getWindows
  workspaces <- mapM getWorkspace windows
  wtitles <- mapM getWindowTitle windows
  wclasses <- mapM getWindowClass windows
  return $ zip (zip3 workspaces wtitles wclasses) windows

-- | Return the index (starting from 0) of the workspace on which the
-- given window is being displayed.
getWorkspace :: X11Window -> X11Property Int
getWorkspace window = readAsInt (Just window) "_NET_WM_DESKTOP"

-- | Ask the window manager to give focus to the given window.
focusWindow :: X11Window -> X11Property ()
focusWindow wh = do
  cmd <- getAtom "_NET_ACTIVE_WINDOW"
  sendWindowEvent cmd (fromIntegral wh)
