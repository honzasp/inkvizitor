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
  mainFrame <- frame [text := "Inkvizitor", clientSize := sz 600 400] 
  tree <- treeCtrl mainFrame []
  fileName <- variable [value := Nothing]
  modified <- variable [value := False]
  menuBar <- menuBarCreate 0
  statusField <- statusField []

  let g = Gui {
      gFrame = mainFrame
    , gTree = tree
    , gFileName = fileName
    , gModified = modified
    , gMenuBar = menuBar
    , gStatusField = statusField
    }

  set (gFrame g) [on closing := onAppClose g]

  makeTree g
  makeMenuBar g
  makeStatusBar g

  where 

    onAppClose :: Gui -> IO ()
    onAppClose g = do
      modified <- get (gModified g) value
      if modified
        then do
          save <- confirmDialog (gFrame g) "Save the file?"
            "The debtors were modified. Do you want to save changes?" True
          if save
            then onFileSave g
            else return ()
        else
          return ()
      propagateEvent

