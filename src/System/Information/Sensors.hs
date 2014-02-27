module System.Information.Sensors (
    getCpuTemp
    , getCpuTempDevice
    , getNvidiaTemp
) where 
import System.Process
import Control.Exception as E

getCpuTemp :: IO String
getCpuTemp = getCpuTempDevice "/sys/class/hwmon/hwmon0/device/temp1_input"

getCpuTempDevice :: String -> IO String
getCpuTempDevice dev = do
    let action = do sensor <- readFile dev
                    let d = read sensor :: Double
                    return $ show $ d / 1000
    E.catch action ignoreIOException

getNvidiaTemp :: IO String
getNvidiaTemp = do
    let action = do sensor <- readProcess "nvidia-settings" ["-q", "gpucoretemp"] ""
                    let line = (lines sensor) !! 1
                    let word = (words line) !! 3
                    return $ init word
    E.catch action ignoreIOException

ignoreIOException :: IOException -> IO String
ignoreIOException _ = return ""
