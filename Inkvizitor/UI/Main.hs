module Inkvizitor.UI.Main 
  ( Gui
  , main
  ) where

import qualified Data.Map as Map
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO

import Inkvizitor.Debtor
import Inkvizitor.UI.File
import Inkvizitor.UI.Gui
import Inkvizitor.UI.MenuBar
import Inkvizitor.UI.StatusBar
import Inkvizitor.UI.Tree

main = start gui

gui :: IO ()
gui = do
  mainFrame <- frame [text := "Inkvizitor"] 
  tree <- treeCtrl mainFrame []
  fileName <- varCreate Nothing
  menuBar <- menuBarCreate 0
  statusField <- statusField []

  let g = Gui {
      gFrame = mainFrame
    , gTree = tree
    , gFileName = fileName
    , gMenuBar = menuBar
    , gStatusField = statusField
    }

  makeTree g
  makeMenuBar g
  makeStatusBar g

