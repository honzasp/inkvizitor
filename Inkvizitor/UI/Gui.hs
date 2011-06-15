module Inkvizitor.UI.Gui 
  ( Gui(..)
  , setFileName
  , getFileName
  )
where

import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO
import System.FilePath

import Inkvizitor.Debtor

data Gui = Gui
  { gFrame :: Frame ()
  , gTree :: TreeCtrl ()
  , gFileName :: Var (Maybe FilePath)
  , gModified :: Var Bool
  , gMenuBar :: MenuBar ()
  , gStatusField :: StatusField
  }

setFileName :: Gui -> Maybe FilePath -> IO ()
setFileName g mbname = do
  varSet (gFileName g) mbname
  case mbname of
    Just fileName ->
      frameSetTitle (gFrame g) $ takeFileName fileName ++ " - Inkvizitor"
    Nothing ->
      frameSetTitle (gFrame g) "Inkvizitor"

getFileName :: Gui -> IO (Maybe FilePath)
getFileName g =
  varGet (gFileName g)
