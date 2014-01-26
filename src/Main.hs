-- | This is just a stub executable that uses dyre to read the config
-- file and recompile itself.
module Main ( main ) where

import System.Taffybar
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args == 0 
    then defaultTaffybar defaultTaffybarConfig
    else taffybarSetDir (args !! 0) defaultTaffybarConfig
