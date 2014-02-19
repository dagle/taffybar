module System.Information.Alsa (
    DisplayType(Raw, Db, Normalized)
    , VolumeConfig
    , showVolume
    , getMute
    , getGenericVolume
    , getRawVolume
    , getDbVolume
    , getNormalizedVolume
    , setGenericVolume
    , setVolume
    , setRawVolume
    , setDbVolume
    , setNormalizedVolume
    , unpercent
    , percent
    , mixer
    , control
    , displayType
    , defaultVolumeConfig
    ) where

import Data.Traversable (sequenceA)
import Control.Applicative ((<$>))
import Sound.ALSA.Mixer
import Data.Maybe (fromJust)
import Control.Monad.Trans
import Control.Monad

data DisplayType = Raw | Db | Normalized

data VolumeConfig = 
    VolumeConfig { mixer :: String -- Name of the alsamixer
                 , control :: String -- Name of the control
                 , displayType :: DisplayType -- Type used to convert the volume value. 
                                              -- Raw | Db | Normalized
                 , button :: Maybe (IO ()) -- The button, we use this intrenaly to set update values
    }

-- default vaule for Alsa, for PulseAudio use: mixer = pulse, displayType = Raw
defaultVolumeConfig :: VolumeConfig
defaultVolumeConfig = VolumeConfig {
    mixer = "default"
    , control = "Master"
    , displayType = Normalized
    , button = Nothing
}

{- some helper functions for getting and setting volume -}
(<+>) :: MonadPlus m => m a -> m a -> m a
a <+> b = mplus a b

-- moving to percent from a linjar scale (raw, dB) (linjar towards alsa)
percent :: Double -> Double -> Double -> Double
percent val high low = (val - low) / (high - low)

-- moving from to a linjar scale (raw, dB) (linjar towards alsa)
unpercent :: Double -> Double -> Double -> Integer
unpercent val high low = floor $ (val * high) - low

-- fetch volume for either output or input
bothChannels :: Maybe Control -> Maybe Volume
bothChannels c = join $ (playback . volume <$> c) <+> (common . volume <$> c)

-- Lifts Either and Maybe
liftEM :: Maybe (IO (a,b)) -> IO (Maybe a, Maybe b)
liftEM = fmap (liftM2 (,) (fmap fst) (fmap snd)) . sequenceA

-- fetch mute-switch for either output or input
switchControl :: Maybe Control -> Maybe Switch
switchControl c = join $ (playback . switch <$> c) <+> (common . switch <$> c)

-- safe get and set functions 
getSw :: Maybe (PerChannel a) -> IO (Maybe a)
getSw Nothing = return Nothing
getSw (Just s) = liftIO $ getChannel FrontLeft s

getVal :: (t -> PerChannel a) -> Maybe t -> IO (Maybe a)
getVal _ Nothing = return Nothing
getVal fetch (Just v) = liftIO $ getChannel FrontLeft $ fetch v

setVal :: (t -> PerChannel x) -> Maybe t -> x -> IO ()
setVal _ Nothing _ = return ()
setVal fetch (Just v) i = do
    setChannel FrontLeft (fetch v) i
    setChannel FrontRight (fetch v) i

-- merges a triplet of Maybes to a Maybe of triplets
mergeM :: (Integral a, Integral a1, Integral a2, Num t, Num t1, Num t2) =>
     (Maybe a, Maybe a1, Maybe a2) -> Maybe (t, t1, t2)
mergeM trip= case trip of
    (Just a, Just b, Just c) -> Just (fromIntegral a,fromIntegral b, fromIntegral c)
    _ -> Nothing

-- show the volume as a string, if you want a textversion
showVolume :: VolumeConfig -> IO String
showVolume conf = do
    val <- getGenericVolume conf
    return $ show val

getMute :: VolumeConfig -> IO (Maybe Bool)
getMute (VolumeConfig mix controler _ _) = do
    control' <- liftIO $ getControlByName mix controler
    getSw $ switchControl control'

getGenericVolume :: VolumeConfig -> IO (Maybe (Double, Double, Double))
getGenericVolume conf@(VolumeConfig _ _ v _) =
    case v of
       Raw -> getRawVolume conf 
       Db -> getDbVolume conf
       Normalized -> getNormalizedVolume conf

getVolume :: (Volume -> IO (Integer, Integer)) 
    -> (Volume -> PerChannel Integer) -> VolumeConfig -> IO (Maybe (Double, Double, Double))
getVolume range fetch (VolumeConfig mix controler _ _) = do
    control' <- liftIO $ getControlByName mix controler
    (low, high) <- liftIO $ liftEM $ range <$> bothChannels control'
    vol <- getVal fetch $ bothChannels control'
    return $ mergeM (vol, high, low)

getRawVolume :: VolumeConfig -> IO (Maybe (Double, Double, Double))
getRawVolume conf = getVolume getRange value conf

getDbVolume :: VolumeConfig -> IO (Maybe (Double, Double, Double))
getDbVolume conf = getVolume getRangeDb dB conf

sndCtlTlvDbGainMute :: Double
sndCtlTlvDbGainMute = -9999999

getNormalizedVolume :: VolumeConfig -> IO (Maybe (Double, Double, Double))
getNormalizedVolume conf = do
    result <- getDbVolume conf 
    case result of
        (Just (vol', high', low')) -> do 
            let (vol, high, low) = (vol', high', low')
                normalized = 10 ** ((vol - high) / 6000.0)
                min_norm = 10 ** ((low - high) / 6000.0)
                normalized2 = (normalized - min_norm) / (1 - min_norm)
            if low == sndCtlTlvDbGainMute 
                    then return (Just (normalized, high, low)) 
                    else return (Just (normalized2, high, low))
        Nothing -> return Nothing

setGenericVolume :: Double -> VolumeConfig -> IO ()
setGenericVolume vol conf@(VolumeConfig _ _ v _) =
    case v of
        Raw -> setRawVolume vol conf
        Db -> setDbVolume vol conf
        Normalized -> setNormalizedVolume vol conf

-- never ever use fromJust like this
setVolume :: Double -> (Volume -> IO (Integer, Integer)) -> (Volume -> PerChannel Integer) -> VolumeConfig -> IO ()
setVolume vol range fetch (VolumeConfig mix controler _ _) = do
    control' <- liftIO $ getControlByName mix controler
    (low, high) <- liftIO $ liftEM $ range <$> bothChannels control'
    when (low /= Nothing && high /= Nothing) $ do
        let i = unpercent vol (fromIntegral $ fromJust high) (fromIntegral $ fromJust low)
        setVal fetch (bothChannels control') i

setRawVolume :: Double -> VolumeConfig -> IO ()
setRawVolume vol = setVolume vol getRange value

setDbVolume :: Double -> VolumeConfig -> IO ()
setDbVolume vol = setVolume vol getRangeDb dB

setNormalizedVolume :: Double -> VolumeConfig -> IO ()
setNormalizedVolume vol (VolumeConfig mix controler _ _) = do
    control' <- liftIO $ getControlByName mix controler
    (low', high') <- liftIO $ liftEM $ getRangeDb <$> bothChannels control'
    when (low' /= Nothing && high' /= Nothing) $ do
        let (low, high) = (fromIntegral (fromJust low'), fromIntegral (fromJust high'))
            min_norm = 10 ** ((low - high) / 6000.0)
            vol' = vol * (1 - min_norm) + min_norm
            normalize v = 6000.0 * logBase 10 v + high
        if low == sndCtlTlvDbGainMute then setVal dB (bothChannels control') (floor $ normalize vol) 
                                      else setVal dB (bothChannels control') (floor $ normalize vol') 



