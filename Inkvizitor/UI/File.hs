module Inkvizitor.UI.File 
  ( loadDebtorsFile
  )
where

import Graphics.UI.WX
import Graphics.UI.WXCore

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
      updateTree g
      setStatus g "Done"
    Nothing ->
      return ()

  wxcEndBusyCursor

