import Data.Bits
import qualified Data.Map as Map
import Graphics.UI.WX
import Graphics.UI.WXCore

import Debtor

data Gui = Gui
  { gFrame :: Frame ()
  , gTree :: TreeCtrl ()
  , gMenuBar :: MenuBar ()
  }

main = start gui

gui :: IO ()
gui = do
  f <- frame [text := "Geocode"] -- TODO: change title
  t <- treeCtrl f [style := wxTR_HIDE_ROOT .|. wxTR_HAS_BUTTONS .|. wxTR_MULTIPLE .|. wxTR_NO_LINES]
  mb <- menuBarCreate 0

  let g = Gui {
      gFrame = f
    , gTree = t
    , gMenuBar = mb
    }

  treeCtrlAddRoot (gTree g) "/" (-1) (-1) objectNull
  frameSetMenuBar (gFrame g) (gMenuBar g)

  fm <- menuCreate "" 0
  menuBarAppend (gMenuBar g) fm "&File"

  fOpen <- menuItem fm   [text := "&Open...", help := "Open a file"]
  fSave <- menuItem fm   [text := "&Save", help := "Save a file"]
  fSaveAs <- menuItem fm [text := "Save as...", help := "Save to a new file"]
  fQuit <- menuItem fm   [text := "&Quit", help := "Close the program"]

  set (gFrame g)
    [ on (menu fOpen) := onFileOpen g
    , on (menu fSave) := onFileSave g
    , on (menu fSaveAs) := onFileSaveAs g
    , on (menu fQuit) := onFileQuit g
    ]
  
  im <- menuCreate "" 0
  menuBarAppend (gMenuBar g) im "&Insert"

  iDebtor <- menuItem im [text := "&Debtor...", help := "Insert a new debtor"]
  iFolder <- menuItem im [text := "&Folder", help := "Insert a new folder"]

  return ()

onFileOpen :: Gui -> IO ()
onFileOpen g = do
  mbPath <- fileOpenDialog (gFrame g) True True "Open..." [("Any file", ["*.*"])] "" ""
  case mbPath of
    Just path -> 
      loadDebtorsFile g path
    Nothing -> 
      return ()

onFileSave :: Gui -> IO ()
onFileSave g = do
  putStrLn "File -> Save"

onFileSaveAs :: Gui -> IO ()
onFileSaveAs g = do
  putStrLn "File -> Save as"

onFileQuit :: Gui -> IO ()
onFileQuit g = do
  putStrLn "File -> Quit"


setItemDebtor :: TreeCtrl a -> TreeItem -> Debtor -> IO ()
setItemDebtor t item debtor =
  treeCtrlSetItemClientData t item (return ()) debtor

getItemDebtor :: TreeCtrl a -> TreeItem -> IO (Maybe Debtor)
getItemDebtor t item =
  unsafeTreeCtrlGetItemClientData t item


loadDebtorsFile :: Gui -> FilePath -> IO ()
loadDebtorsFile g path = do
  mbDebtorMap <- do
      contents <- readFile path
      result <- getDebtorsFromJSON contents
      case result of
        Left err ->
          fail err
        Right debtors ->
          return $ Just debtors
    `catch` \err -> do
      putStrLn $ "Error when loading debtors file:"
      putStrLn $ show err
      return Nothing

  case mbDebtorMap of
    Just debtorMap -> do
      root <- treeCtrlGetRootItem (gTree g)
      treeCtrlDeleteChildren (gTree g) root
      sequence_ $ Map.foldWithKey (\f ds a -> (addFolder root f ds) : a) [] debtorMap
      treeCtrlExpand (gTree g) root
    Nothing ->
      return ()

  where addFolder :: TreeItem -> String -> [Debtor] -> IO ()
        addFolder parent folder debtorList = do
          folderItem <- treeCtrlAppendItem (gTree g) parent folder (-1) (-1) objectNull
          mapM_ (addDebtor folderItem) debtorList
          treeCtrlSetItemHasChildren (gTree g) folderItem (not (null debtorList))
          treeCtrlExpand (gTree g) folderItem

        addDebtor :: TreeItem -> Debtor -> IO ()
        addDebtor parent debtor = do
          debtorItem <- treeCtrlAppendItem (gTree g) parent (getName debtor) (-1) (-1) objectNull
          setItemDebtor (gTree g) debtorItem debtor
