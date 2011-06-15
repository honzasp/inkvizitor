module Inkvizitor.UI.Tree
  ( makeTree
  , updateTree
  , loadDebtorMap
  , makeDebtorMap
  )
where

import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.Map as Map
import Foreign
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WXCore.WxcTypes

import Inkvizitor.Debtor
import Inkvizitor.UI.Debtor
import Inkvizitor.UI.Gui
import Inkvizitor.UI.StatusBar

data ItemData 
  = FolderItem String
  | DebtorItem Debtor
  | OtherItem
  deriving (Eq, Show)

makeTree :: Gui -> IO ()
makeTree g = do
  root <- treeCtrlAddRoot (gTree g) "/" (-1) (-1) objectNull
  setItemData g root OtherItem
  set (gTree g) 
    [ on treeEvent := onTreeEvent
    , on doubleClick := onDoubleClick
    , on keyboard := onKeyboard
    , style := 
          wxTR_EDIT_LABELS 
      .|. wxTR_HIDE_ROOT 
      .|. wxTR_HAS_BUTTONS 
      .|. wxTR_MULTIPLE 
      .|. wxTR_NO_LINES
    ]

  where

    onTreeEvent :: EventTree -> IO ()
    onTreeEvent event = 
      case event of
        TreeSelChanged item olditem | treeItemIsOk item -> do
          selected <- getSelectedItems g
          case selected of
            [item] -> do
              idata <- getItemData g item
              case idata of
                FolderItem name ->
                  setStatus g $ "Folder " ++ name
                DebtorItem debtor ->
                  setStatus g $ "Debtor " ++ getName debtor
                OtherItem ->
                  setStatus g $ "?"
            items ->
              setStatus g $ show (length items) ++ " items"
          propagateEvent
        other -> do
          propagateEvent

    onDoubleClick :: Point -> IO ()
    onDoubleClick point =
      hitTest point >>= maybe (return ()) editItem

    onKeyboard :: EventKey -> IO ()
    onKeyboard eventKey = do
      case keyKey eventKey of
        KeyReturn ->
          hitTest (keyPos eventKey) >>= maybe (return ()) editItem
        _ ->
          return ()
      propagateEvent

    hitTest :: Point -> IO (Maybe TreeItem)
    hitTest point = do
      alloca $ \flagsPtr -> do
        item <- treeCtrlHitTest (gTree g) point flagsPtr
        if treeItemIsOk item
          then return (Just item)
          else return Nothing

    -- | Edit an item in the tree (e.g. double-click or Enter key)
    editItem :: TreeItem -> IO ()
    editItem item = do
      idata <- getItemData g item
      case idata of
        DebtorItem debtor ->
          showDebtorForm g debtor $ \result -> do
            setItemData g item $ DebtorItem result
            updateItem g item
        FolderItem folderName ->
          showFolderForm g folderName $ \result -> do
            setItemData g item $ FolderItem result
            updateItem g item
        _ ->
          return ()

-- | Shows a form to change name of folder
showFolderForm :: Gui -> String -> (String -> IO ()) -> IO ()
showFolderForm g oldName onResult = do
  newName <- textDialog (gFrame g) "Name of the folder:" "Edit folder" oldName
  if null newName
    then onResult oldName
    else onResult newName

-- | Set the debtor to client data of the tree item.
setItemData :: Gui -> TreeItem -> ItemData -> IO ()
setItemData g item idata =
  treeCtrlSetItemClientData (gTree g) item (return ()) idata

getItemData :: Gui -> TreeItem -> IO ItemData
getItemData g item = do
  clientData <- unsafeTreeCtrlGetItemClientData (gTree g) item
  case clientData of
    Just idata -> return idata
    Nothing -> return OtherItem

-- | Gets all selected items in the tree.
getSelectedItems :: Gui -> IO [TreeItem]
getSelectedItems g = 
  map treeItemFromInt <$> treeCtrlGetSelections (gTree g)

-- | Loads debtors from debtorMap to the tree.
loadDebtorMap :: Gui -> DebtorMap -> IO ()
loadDebtorMap g debtorMap = do
  root <- treeCtrlGetRootItem (gTree g)
  treeCtrlDeleteChildren (gTree g) root

  sequence_ $ Map.foldWithKey (\f ds a -> (addFolder root f ds) : a) [] debtorMap
  treeCtrlExpand (gTree g) root

  where

    addFolder :: TreeItem -> String -> [Debtor] -> IO ()
    addFolder parent folder debtorList = do
      folderItem <- createItem g parent (FolderItem folder)
      mapM_ (addDebtor folderItem) debtorList
      treeCtrlExpand (gTree g) folderItem

    addDebtor :: TreeItem -> Debtor -> IO ()
    addDebtor parent debtor = do
      createItem g parent (DebtorItem debtor)
      return ()

-- | Makes a debtorMap from the tree.
--   Throws a userError when the tree is corrupted (that is bug).
makeDebtorMap :: Gui -> IO DebtorMap
makeDebtorMap g = do
  root <- treeCtrlGetRootItem (gTree g)
  folders <- treeCtrlGetItemChildren (gTree g) root
  assoc <- forM folders getFolder
  return $ Map.fromList assoc

  where

    getFolder :: TreeItem -> IO (String, [Debtor])
    getFolder folderItem = do
      idata <- getItemData g folderItem
      case idata of
        FolderItem name -> do
          children <- treeCtrlGetItemChildren (gTree g) folderItem
          debtors <- forM children getDebtor
          return (name, debtors)
        other ->
          fail $ "in getFolder: expected FolderItem, got " ++ show other

    getDebtor :: TreeItem -> IO Debtor
    getDebtor debtorItem = do
      idata <- getItemData g debtorItem
      case idata of
        DebtorItem debtor ->
          return debtor
        other ->
          fail $ "in getDebtor: expected DebtorItem, got " ++ show other
  
-- | Update tree items to reflect changes in their associated ItemData.
updateTree :: Gui -> IO ()
updateTree g = do
  root <- treeCtrlGetRootItem (gTree g)
  update root

  where
    update :: TreeItem -> IO ()
    update item = do
      updateItem g item
      children <- treeCtrlGetItemChildren (gTree g) item
      mapM_ update children

treeCtrlGetItemChildren :: TreeCtrl a -> TreeItem -> IO [TreeItem]
treeCtrlGetItemChildren tree item = do
  first <- treeCtrlGetLastChild tree item
  siblings first

  where
    siblings :: TreeItem -> IO [TreeItem]
    siblings i =
      if treeItemIsOk i
        then do
          sibling <- treeCtrlGetPrevSibling tree i
          next <- siblings sibling
          return (i : next)
        else
          return []

-- | Update one tree item to reflect changes in ItemData.
updateItem :: Gui -> TreeItem -> IO ()
updateItem g item = do
  idata <- getItemData g item
  case idata of
    FolderItem name -> do
      treeCtrlSetItemText (gTree g) item name
      treeCtrlSetItemHasChildren (gTree g) item True
    DebtorItem debtor ->
      treeCtrlSetItemText (gTree g) item (getName debtor)
    OtherItem ->
      return ()

-- | Creates a tree item with given ItemData.
createItem :: Gui -> TreeItem -> ItemData -> IO TreeItem
createItem g parent idata = do
  item <- treeCtrlAppendItem (gTree g) parent "" (-1) (-1) objectNull
  setItemData g item idata
  updateItem g item
  return item

