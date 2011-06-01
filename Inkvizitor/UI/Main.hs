module Inkvizitor.UI.Main 
  ( Gui
  , main
  ) where

import qualified Data.Map as Map
import Graphics.UI.WX
import Graphics.UI.WXCore

import Inkvizitor.Debtor
import Inkvizitor.UI.File
import Inkvizitor.UI.Gui
import Inkvizitor.UI.MenuBar
import Inkvizitor.UI.StatusBar
import Inkvizitor.UI.Tree

main = start gui

gui :: IO ()
gui = do
  f <- frame [text := "Inkvizitor"] 
  t <- treeCtrl f []
  mb <- menuBarCreate 0
  dv <- varCreate Map.empty
  sf <- statusField []

  let g = Gui {
      gFrame = f
    , gTree = t
    , gMenuBar = mb
    , gDebtorsVar = dv
    , gStatusField = sf
    }

  makeTree g
  makeMenuBar g
  makeStatusBar g

