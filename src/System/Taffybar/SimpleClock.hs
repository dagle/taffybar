-- | This module implements a very simple text-based clock widget.
-- The widget also toggles a calendar widget when clicked.  This
-- calendar is not fancy at all and has no data backend.
module System.Taffybar.SimpleClock (
  textClockNew,
  textClockNewWith,
  defaultClockConfig,
  ClockConfig(..)
  ) where

import Data.Maybe ( fromMaybe )
import Data.Time.Calendar ( toGregorian )
import qualified Data.Time.Clock as Clock
import Data.Time.Format
import Data.Time.LocalTime
import Graphics.UI.Gtk
import System.Locale
import System.Taffybar.Widgets.AttachedPopup

resetCalendarDate :: Calendar -> IO ()
resetCalendarDate cal = do
  (y,m,d) <- Clock.getCurrentTime >>= return . toGregorian . Clock.utctDay
  calendarSelectMonth cal (fromIntegral m - 1) (fromIntegral y)
  calendarSelectDay cal (fromIntegral d)

-- | Create the widget.  I recommend passing @Nothing@ for the
-- TimeLocale parameter.  The format string can include Pango markup
-- (http://developer.gnome.org/pango/stable/PangoMarkupFormat.html).
textClockNew :: Maybe TimeLocale -> String -> Double -> IO Widget
textClockNew userLocale fmt updateSeconds =
  textClockNewWith cfg fmt updateSeconds
  where
    cfg = defaultClockConfig { clockTimeLocale = userLocale }

data ClockConfig = ClockConfig { clockTimeZone :: Maybe TimeZone
                               , clockTimeLocale :: Maybe TimeLocale
                               }
                               deriving (Eq, Ord, Show)

-- | A clock configuration that defaults to the current locale
defaultClockConfig :: ClockConfig
defaultClockConfig = ClockConfig Nothing Nothing

-- | A configurable text-based clock widget.  It currently allows for
-- a configurable time zone through the 'ClockConfig'.
--
-- See also 'textClockNew'.
textClockNewWith :: ClockConfig -> String -> Double -> IO Widget
textClockNewWith cfg fmt updateSeconds = do
  defaultTimeZone <- getCurrentTimeZone
  let timeLocale = fromMaybe defaultTimeLocale userLocale
      timeZone = fromMaybe defaultTimeZone userZone
  let clockListCfg = PopupConfig
        (\_ (h,_) -> getCurrentTime' timeLocale fmt timeZone >>= labelSetMarkup h) 
        (\_ (_, cal) -> resetCalendarDate cal)
        id "Calendar" ()
  pollingPopupNew clockListCfg (labelNew Nothing) calendarNew updateSeconds
  where
    userZone = clockTimeZone cfg
    userLocale = clockTimeLocale cfg
    -- alternate getCurrentTime that takes a specific TZ
    getCurrentTime' :: TimeLocale -> String -> TimeZone -> IO String
    getCurrentTime' l f z =
      return . formatTime l f . utcToZonedTime z =<< Clock.getCurrentTime
