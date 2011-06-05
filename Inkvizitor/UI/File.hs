module Inkvizitor.UI.File 
  ( loadDebtorsFile
  , saveDebtorsFile
  )
where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Prelude hiding (catch)
import System.IO
import System.IO.Error

import Inkvizitor.Debtor
import Inkvizitor.UI.Gui
import Inkvizitor.UI.StatusBar
import Inkvizitor.UI.Tree

-- | Opens a file with debtors and loads it.
loadDebtorsFile :: Gui -> FilePath -> IO ()
loadDebtorsFile g path = do
  mbDebtorMap <- do
      setStatus g "Opening..."
      wxcBeginBusyCursor
      contents <- readFile path
      case getDebtorsFromJSON contents of
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
      loadDebtorMap g debtorMap
      setFileName g (Just path)
      updateTree g
      setStatus g "Done"
    Nothing ->
      return ()

  wxcEndBusyCursor

-- | Saves tree to debtors file
saveDebtorsFile :: Gui -> FilePath -> IO ()
saveDebtorsFile g path = do
  wxcBeginBusyCursor
  debtors <- makeDebtorMap g
  result <- try $ writeFile path $ getJSONFromDebtors debtors 

  wxcEndBusyCursor
  case result of
    Right () ->
      setStatus g "Done"
    Left err -> do
      wxcEndBusyCursor
      setStatus g "Error when saving debtors file"
      errorDialog (gFrame g) "Error" ("Error when saving debtors file:\n" ++ show err)
  
