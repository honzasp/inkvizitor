module Inkvizitor.UI.MenuBar
  ( makeMenuBar
  , onFileSave
  ) 
where

import Graphics.UI.WX
import Graphics.UI.WXCore

import Inkvizitor.UI.File
import Inkvizitor.UI.Gui
import Inkvizitor.UI.Tree

makeMenuBar :: Gui -> IO ()
makeMenuBar g = do
  frameSetMenuBar (gFrame g) (gMenuBar g)
  fileMenu g
  insertMenu g

-- | Creates 'File' menu
fileMenu :: Gui -> IO ()
fileMenu g = do
  file <- menuCreate "" 0
  menuBarAppend (gMenuBar g) file "&File"

  open <- menuItem file   [text := "&Open...", help := "Open a file"]
  save <- menuItem file   [text := "&Save", help := "Save a file"]
  saveAs <- menuItem file [text := "Save as...", help := "Save to a new file"]
  quit <- menuQuit file   [text := "&Quit", help := "Close the program"]

  set (gFrame g)
    [ on (menu open) := onFileOpen g
    , on (menu save) := onFileSave g
    , on (menu saveAs) := onFileSaveAs g
    , on (menu quit) := onFileQuit g
    ]

onFileOpen :: Gui -> IO ()
onFileOpen g = do
  mbPath <- fileOpenDialog (gFrame g) True True "Open..." 
    [("Debtors JSON file (*.json, *.js)", ["*.json", "*.js"]), ("Any file", ["*.*"])] "" ""
  case mbPath of
    Just path -> 
      loadDebtorsFile g path
    Nothing -> 
      return ()

onFileSave :: Gui -> IO ()
onFileSave g = do
  mbFileName <- getFileName g
  case mbFileName of
    Just fileName ->
      saveDebtorsFile g fileName
    Nothing ->
      onFileSaveAs g

onFileSaveAs :: Gui -> IO ()
onFileSaveAs g = do
  mbFileName <- fileSaveDialog (gFrame g) True True "Save debtors as..." 
    [("Debtors JSON file (*.json, *.js)", ["*.json", "*.js"]), ("Any file", ["*.*"])] "" ""
  case mbFileName of
    Just fileName -> do
      saveDebtorsFile g fileName
    Nothing ->
      return ()

onFileQuit :: Gui -> IO ()
onFileQuit g = return ()

-- | Creates 'Insert' menu
insertMenu :: Gui -> IO ()
insertMenu g = do
  insert <- menuCreate "" 0
  menuBarAppend (gMenuBar g) insert "&Insert"

  debtor <- menuItem insert [text := "&Debtor...", help := "Insert a new debtor"]
  folder <- menuItem insert [text := "&Folder...", help := "Insert a new folder"]

  set (gFrame g)
    [ on (menu debtor) := onInsertDebtor g
    , on (menu folder) := onInsertFolder g
    ]

onInsertDebtor :: Gui -> IO ()
onInsertDebtor = insertDebtor

onInsertFolder :: Gui -> IO ()
onInsertFolder = insertFolder

