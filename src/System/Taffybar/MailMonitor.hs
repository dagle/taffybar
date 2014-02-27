module System.Taffybar.MailMonitor (
    mailIconNew
    , mailTextNew
    , MailConfig
    ) where

import Control.Monad
import Graphics.UI.Gtk
import System.Information.Mail
import Control.Concurrent
import Control.Concurrent.STM
import System.Taffybar.Widgets.AttachedPopup
import Control.Exception as E
import qualified Data.Set as S
import Data.Maybe
import System.Taffybar.Widgets.TextImage

-- Creates a mail widget with status label and a text area
mailIconNew :: MailConfig -> IO Widget
mailIconNew cfg = do
    c <- initMail cfg
    let listMailcfg = PopupConfig undefined displayMail id "Mails" c
    (w, header, _) <- popupBoxNew listMailcfg (textImageNew "/home/dagle/mail.png" (count c)) (labelNew Nothing)
    _ <- forkIO $ hookMail cfg (const $ widgetQueueDraw header) c
    return w

mailTextNew :: MailConfig -> IO Widget
mailTextNew cfg = do
    c <- initMail cfg
    let listMailcfg = PopupConfig undefined displayMail id "Mails" c
    (w, header, _) <- popupBoxNew listMailcfg (labelNew Nothing) (labelNew Nothing)
    _ <- forkIO $ hookMail cfg (countMail header) c
    return w

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()

count :: [TVar (S.Set String)] -> IO String
count c = getMail c >>= (\m -> return $ show . length . concat $ m)

--countMail :: [TVar (S.Set String)] -> Label -> IO ()
countMail :: LabelClass self =>
        self -> [(a1, [a])] -> IO ()
countMail h mails = do
    let str = show . length . concat . map snd $ mails
    E.catch (postGUIAsync $ labelSetMarkup h $ "Mails: " ++ str) ignoreIOException

-- Needs to improve, looks really ugly atm
displayMail :: [TVar (S.Set String)] -> (a, Label) -> IO ()
displayMail c (_,box) = do
    m <- liftM concat $ getMail c
    content <- mapM unpackMail m
    let titles = map (liftM snd . getSubject) content
    let parsed = map fromJust $ filter ((/=) Nothing) titles
    let update = do
            postGUIAsync $ labelSetMarkup box $ unlines $ take 5 parsed
    E.catch update ignoreIOException
