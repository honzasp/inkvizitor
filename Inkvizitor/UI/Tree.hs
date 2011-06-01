module Inkvizitor.UI.Tree
  ( makeTree
  , updateTree
  )
where

import Control.Applicative
import Data.Bits
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WXCore.WxcTypes
import qualified Data.Map as Map

import Inkvizitor.Debtor
import Inkvizitor.UI.Gui
import Inkvizitor.UI.StatusBar

makeTree :: Gui -> IO ()
makeTree g = do
  treeCtrlAddRoot (gTree g) "/" (-1) (-1) objectNull
  set (gTree g) 
    [ on treeEvent := onTreeEvent
    , style := wxTR_HIDE_ROOT .|. wxTR_HAS_BUTTONS .|. wxTR_MULTIPLE .|. wxTR_NO_LINES
    ]

  where

    onTreeEvent :: EventTree -> IO ()
    onTreeEvent event = 
      case event of
        TreeSelChanged item olditem | treeItemIsOk item -> do
          selected <- getSelectedItems g
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

-- | Gets all selected items in the tree.
getSelectedItems :: Gui -> IO [TreeItem]
getSelectedItems g = 
  map treeItemFromInt <$> treeCtrlGetSelections (gTree g)
  
-- | Update the tree to reflect debtors map in the Gui
updateTree :: Gui -> IO ()
updateTree g = do
  root <- treeCtrlGetRootItem (gTree g)
  treeCtrlDeleteChildren (gTree g) root

  debtorMap <- varGet $ gDebtorsVar g
  sequence_ $ Map.foldWithKey (\f ds a -> (addFolder root f ds) : a) [] debtorMap
  treeCtrlExpand (gTree g) root

  where

    addFolder :: TreeItem -> String -> [Debtor] -> IO ()
    addFolder parent folder debtorList = do
      folderItem <- treeCtrlAppendItem (gTree g) parent folder (-1) (-1) objectNull
      mapM_ (addDebtor folderItem) debtorList
      treeCtrlSetItemHasChildren (gTree g) folderItem (not (null debtorList))
      treeCtrlExpand (gTree g) folderItem

    addDebtor :: TreeItem -> Debtor -> IO ()
    addDebtor parent debtor = do
      debtorItem <- treeCtrlAppendItem (gTree g) parent (getName debtor) (-1) (-1) objectNull
      setItemDebtor (gTree g) debtorItem debtor
