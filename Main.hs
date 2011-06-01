import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.Map as Map
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WXCore.WxcTypes

import Debtor

data Gui = Gui
  { gFrame :: Frame ()
  , gTree :: TreeCtrl ()
  , gMenuBar :: MenuBar ()
  , gDebtorsVar :: Var DebtorMap
  , gStatusField :: StatusField
  }

main = start gui

gui :: IO ()
gui = do
  f <- frame [text := "Inkvizitor"] 
  t <- treeCtrl f [style := wxTR_HIDE_ROOT .|. wxTR_HAS_BUTTONS .|. wxTR_MULTIPLE .|. wxTR_NO_LINES]
  mb <- menuBarCreate 0
  dv <- varCreate Map.empty
  sf <- statusField [text := "Hello!"]

  let g = Gui {
      gFrame = f
    , gTree = t
    , gMenuBar = mb
    , gDebtorsVar = dv
    , gStatusField = sf
    }

  set (gFrame g) [statusBar := [sf]]
  treeCtrlAddRoot (gTree g) "/" (-1) (-1) objectNull
  set (gTree g) [on treeEvent := onTreeEvent g]

  frameSetMenuBar (gFrame g) (gMenuBar g)

  -- File menu
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
  
  -- Insert menu
  im <- menuCreate "" 0
  menuBarAppend (gMenuBar g) im "&Insert"

  iDebtor <- menuItem im [text := "&Debtor...", help := "Insert a new debtor"]
  iFolder <- menuItem im [text := "&Folder", help := "Insert a new folder"]

  return ()

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
  putStrLn "File -> Save"

onFileSaveAs :: Gui -> IO ()
onFileSaveAs g = do
  putStrLn "File -> Save as"

onFileQuit :: Gui -> IO ()
onFileQuit g = do
  putStrLn "File -> Quit"


-- | Event on the treeCtrl with debtors.
onTreeEvent :: Gui -> EventTree -> IO ()
onTreeEvent g event = 
  case event of
    TreeSelChanged item olditem | treeItemIsOk item -> do
      selected <- map treeItemFromInt <$> treeCtrlGetSelections (gTree g)
      case selected of
        [item] -> do
          mbDeb <- getItemDebtor (gTree g) item
          case mbDeb of
            Just debtor ->
              setStatus g $ "Selected: " ++ getName debtor
            Nothing ->
              return ()
        items ->
          setStatus g $ show (length items) ++ " debtors selected"
      propagateEvent
    other ->
      propagateEvent

-- | Set the debtor to client data of the tree item.
setItemDebtor :: TreeCtrl a -> TreeItem -> Debtor -> IO ()
setItemDebtor t item debtor =
  treeCtrlSetItemClientData t item (return ()) debtor

-- | Maybe gets a debtor set as client data of the tree item.
getItemDebtor :: TreeCtrl a -> TreeItem -> IO (Maybe Debtor)
getItemDebtor t item =
  unsafeTreeCtrlGetItemClientData t item

-- | Sets text in the status bar.
setStatus :: Gui -> String -> IO ()
setStatus g msg = 
  set (gStatusField g) [text := msg]

-- | Opens a file with debtors and loads it.
loadDebtorsFile :: Gui -> FilePath -> IO ()
loadDebtorsFile g path = do
  mbDebtorMap <- do
      setStatus g "Opening..."
      wxcBeginBusyCursor
      contents <- readFile path
      result <- getDebtorsFromJSON contents
      case result of
        Left err ->
          fail err
        Right debtors ->
          return $ Just debtors
    `catch` \err -> do
      wxcEndBusyCursor
      setStatus g "Error when loading debtors file"
      errorDialog (gFrame g) "Error" ("Error when loading debtors file:\n" ++ show err)
      return Nothing

  case mbDebtorMap of
    Just debtorMap -> do
      varSet (gDebtorsVar g) debtorMap
      updateTreeCtrl g
      setStatus g "Done"
    Nothing ->
      return ()

  wxcEndBusyCursor

-- | Update treeCtrl to reflect debtors map in the Gui
updateTreeCtrl :: Gui -> IO ()
updateTreeCtrl g = do
  root <- treeCtrlGetRootItem (gTree g)
  treeCtrlDeleteChildren (gTree g) root

  debtorMap <- varGet $ gDebtorsVar g
  sequence_ $ Map.foldWithKey (\f ds a -> (addFolder root f ds) : a) [] debtorMap
  treeCtrlExpand (gTree g) root

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
