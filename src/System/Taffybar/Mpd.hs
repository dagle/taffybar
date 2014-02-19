module System.Taffybar.Mpd (
    mpdNew
) where

import qualified Network.MPD as M
import Control.Monad
import System.Taffybar.Widgets.PollingLabel
import Graphics.UI.Gtk

-- | Simple mpd module
discardError :: (b -> [Char]) -> Either a b -> [Char]
discardError = either (\_ -> "")
-- doing head might be unsafe but I think it shouldn't return an empty list in that case
-- and just a Nothing
fetchInfo :: Either a (Maybe (Maybe [M.Value])) -> [Char]
fetchInfo = discardError (maybe "" $ maybe "" $ M.toString . head )

showStatus :: M.State -> String
showStatus M.Playing = "▶"
showStatus M.Stopped = "■"
showStatus M.Paused = "■"

mpdgetSong :: IO String
mpdgetSong = do
    song <- M.withMPD M.currentSong
    stat <- M.withMPD M.status
    let song' = fetchInfo $ liftM (liftM (M.sgGetTag M.Title)) song
        artist' = fetchInfo $ liftM (liftM (M.sgGetTag M.Artist)) song
        stat' = discardError (showStatus) $ liftM M.stState stat
    return $ stat' ++ " " ++ artist' ++ " - " ++ song'

mpdNew :: IO Widget
mpdNew = do
    l <-  pollingLabelNew "" 2 mpdgetSong
    widgetShowAll l
    return $ toWidget l
