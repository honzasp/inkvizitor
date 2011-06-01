module Inkvizitor.UI.StatusBar
  ( makeStatusBar
  , setStatus
  )
where

import Graphics.UI.WX
import Graphics.UI.WXCore

import Inkvizitor.UI.Gui

makeStatusBar :: Gui -> IO ()
makeStatusBar g = do
  set (gStatusField g) [text := "Hello!"]
  set (gFrame g) [statusBar := [gStatusField g]]

-- | Sets text in the status bar.
setStatus :: Gui -> String -> IO ()
setStatus g msg = 
  set (gStatusField g) [text := msg]

