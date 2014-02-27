module System.Taffybar.Widgets.TextImage (
    textImageNew
) where

import Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import Control.Monad.IO.Class

onExpose' :: (MonadIO m, WidgetClass widget) =>
                widget -> Surface -> IO [Char] -> m Bool
onExpose' window img cmd = liftIO $ do
  cr <- widgetGetDrawWindow window
  str <- cmd

  renderWithDrawable cr $ do
    C.setSourceSurface img 0 0
    C.paint
    case str of
        "" -> do
             C.setSourceRGB 0.5 0.5 0.5
             C.rectangle 0 0 23 15
             C.paintWithAlpha 0.7
        str' -> do
             C.setFontSize 12
             C.setSourceRGB 0.8 0.2 0
             C.selectFontFace "Courier" FontSlantNormal FontWeightBold
             C.moveTo 5 10
             C.textPath str'
             C.clip
             C.paintWithAlpha 1

  return True

textImageNew :: FilePath -> IO [Char] -> IO Widget
textImageNew path cmd = do
  canvas <- drawingAreaNew
  widgetSetSizeRequest canvas 23 (-1)
  img <- imageSurfaceCreateFromPNG path
  box <- hBoxNew False 1
  _ <- canvas `on` exposeEvent $ onExpose' canvas img cmd
  boxPackStart box canvas PackGrow 0
  widgetShowAll box
  return $ toWidget box

