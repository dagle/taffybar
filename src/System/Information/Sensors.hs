module System.Information.Sensors (
    getCpuTemp
    , getNvidiaTemp
) where 
import System.Process
import Control.Exception as E

-- This module is a bit ugly and I'm not sure if portable in the cpu case, I guess I should wrap lm-sensors 
-- with chs bindings but that is for the future

getCpuTemp :: IO String
getCpuTemp = do
    let action = do sensor <- readProcess "sensors" [] ""
                    let line = (lines sensor) !! 2
                    let word = (words line) !! 3
                    return $ init . init . tail $ word
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
