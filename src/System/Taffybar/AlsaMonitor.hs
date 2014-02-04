module System.Taffybar.AlsaMonitor (volumeMonitorNew, defaultVolumeConfig) where 

import Graphics.UI.Gtk
import Control.Monad
import Control.Exception as E
import Control.Concurrent ( forkIO, threadDelay )
import System.Information.Alsa

volumeMonitorNew :: Double  -- Interval in seconds how often the volume should update
                 -> VolumeConfig  -- config telling what mixer and how the value should be displayed
                 -> IO Widget -- Widget to add to your widgetbar
volumeMonitorNew interval conf = do
    but <- pollingVolume interval conf
    widgetShowAll but
    return $ toWidget but

pollingVolume :: Double -> VolumeConfig -> IO Widget
pollingVolume interval conf  = do
  b <- volumeButtonNew
  _ <- on b scaleButtonValueChanged $ \x -> setGenericVolume x conf

  _ <- on b realize $ do
    _ <- forkIO $ forever $ do
      let tryUpdate = do
            postGUIAsync $ setVolumeButton b conf
      E.catch tryUpdate ignoreIOException
      threadDelay $ floor (interval * 1000000)
    return ()

  return (toWidget b)

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()

setVolumeButton :: ScaleButtonClass o => o -> VolumeConfig -> IO ()
setVolumeButton vol conf =
    case (displayType conf) of
    Normalized -> do
        getGenericVolume conf >>= maybe (return ()) (\(volume,_,_) 
            -> set vol [scaleButtonValue := volume])
    _ -> do
        getGenericVolume conf >>= maybe (return ()) (\(volume,high,low) 
            -> set vol [scaleButtonValue := (percent volume high low)])
