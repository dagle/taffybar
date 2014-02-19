module System.Taffybar.MailMonitor (
    mailNew
    , MailConfig
    ) where

import Control.Monad
import Graphics.UI.Gtk
import System.Information.Mail
import Control.Concurrent
import Control.Concurrent.STM
import System.Taffybar.Widgets.PollingList
import Control.Exception as E
import qualified Data.Set as S
import Data.Maybe

-- Creates a mail widget with status label and a text area
mailNew :: MailConfig -> IO Widget
mailNew cfg = do
    c <- initMail cfg
    let listMailcfg = ListConfig 5 (undefined) displayMail (const . const $ return True) "Mails" c
    (w, header, _) <- listBoxNew listMailcfg
    _ <- forkIO $ hookMail cfg (countMail header) c
    return w

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()

-- should maybe show one label per indox? 
countMail :: LabelClass self => self -> [a] -> IO ()
countMail label mails = do
    labelSetMarkup label $ "Mails: " ++ (show $ length mails)

-- Needs to improve, looks really ugly atm
displayMail :: [TVar (S.Set String)] -> [Label] -> IO ()
displayMail c boxes = do
    m <- liftM concat $ getMail c
    content <- mapM unpackMail m
    let titles = map (liftM snd . getSubject) content
    let parsed = map fromJust $ filter ((/=) Nothing) titles
    let update = do
            postGUIAsync $ zipWithM_ (labelSetMarkup) boxes parsed
    E.catch update ignoreIOException
