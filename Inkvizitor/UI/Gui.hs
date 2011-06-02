module Inkvizitor.UI.Gui 
  ( Gui(..)
  )
where

import Graphics.UI.WX
import Graphics.UI.WXCore

import Inkvizitor.Debtor

data Gui = Gui
  { gFrame :: Frame ()
  , gTree :: TreeCtrl ()
  , gMenuBar :: MenuBar ()
  , gStatusField :: StatusField
  }

