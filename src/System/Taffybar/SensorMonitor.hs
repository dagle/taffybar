module System.Taffybar.SensorMonitor (
    sensorTextNew
    , sensorBarNew
) where 

import System.Information.Sensors
import System.Taffybar.Widgets.PollingLabel
import System.Taffybar.Widgets.PollingBar
import Graphics.UI.Gtk
import Control.Monad

sensorTextNew timer = do
    l <- pollingLabelNew "" timer getCpuTemp
    widgetShowAll l
    return $ toWidget l
   
numToDevice num dev = "/sys/class/hwmon/hwmon0/device/temp" ++ (show num) ++ "_" ++ dev

sensorBarNew :: Int -> BarConfig -> IO Widget
sensorBarNew num barconf = do
    max <- liftM read $ readFile $ numToDevice num "max"
    b <- pollingBarNew barconf 10 (showDevice num max)
    widgetShowAll b
    return $ toWidget b
    
showDevice :: Int -> Double -> IO Double
showDevice num max = do 
    input <- liftM read $ readFile $ numToDevice num "input"
    return $ input / max
