module System.Taffybar.MailMonitor (
    mailNew
    ) where 

import Control.Monad.Trans 
import Control.Monad
import Graphics.UI.Gtk
import System.Information.Mail
import System.Taffybar.Widgets.Util
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Set as S

-- make a mailwindow and returns the window and textarea
makeMail :: [TVar (S.Set String)] -> IO (Window, TextBuffer)
makeMail c = do
    container <- windowNew
    text <- textBufferNew Nothing
    view <- textViewNew
    textViewSetBuffer view text
    containerAdd container view
    _ <- onShow container $ liftIO $ displayMail text c
    _ <- on container deleteEvent $ do
        liftIO (widgetHideAll container)
        return True
    return (container, text)

-- Creates a mail widget with status label and a text area
mailNew :: MailConfig -> IO Widget
mailNew cfg = do
    c <- initMail cfg
    l <- labelNew Nothing
    labelSetMarkup l ""
    ebox <- eventBoxNew
    containerAdd ebox l
    eventBoxSetVisibleWindow ebox False
    (label, text) <- makeMail c
    _ <- on ebox buttonPressEvent $ onClick [SingleClick] (toggleMail l label text c)
    _ <- forkIO $ hookMail cfg (countMail l) c
    widgetShowAll ebox
    return $ toWidget ebox

-- should maybe show one label per indox? 
countMail :: LabelClass self => self -> [a] -> IO ()
countMail label mails = do
    labelSetMarkup label $ "Mails: " ++ (show $ length mails)

-- currently just display the subject and not the inbox
displayMail :: TextBufferClass self => self -> [TVar (S.Set String)] -> IO ()
displayMail text c = do
    m <- getMail c
    let m2 = concat m
    i <- mapM unpackMail m2
    let titles = map (liftM snd . getSubject) i 
    textBufferSetText text $ show $ take 10 titles

-- shows the textarea
toggleMail :: (WidgetClass w, TextBufferClass self) => w -> Window -> self -> [TVar (S.Set String)] -> IO Bool
toggleMail w c t m = do
    isVis <- get c widgetVisible
    if isVis
        then widgetHideAll c
        else do
            displayMail t m
            attachPopup w "Mail" c
            displayPopup w c
    return True
