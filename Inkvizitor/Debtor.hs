module Inkvizitor.Debtor
  ( Debtor(..)
  , DebtorMap(..)
  , getDebtorsFromJSON
  , getJSONFromDebtors
  )
where

import Control.Applicative
import Data.Fixed
import Data.Time
import qualified Data.Map as Map
import Text.JSON
import Text.JSON.Pretty
import qualified Text.PrettyPrint.HughesPJ as PP

data Debtor = Debtor
  { getName :: String
  , getPhoneNum :: String
  , getExecutionNum :: String
  , getAddresses :: [String]
  , getAmount :: Int
  , getExecutionTime :: Maybe LocalTime
  , getComment :: String
  } deriving (Eq, Show)

type DebtorMap = Map.Map String [Debtor]

getDebtorsFromJSON :: String -> Either String DebtorMap
getDebtorsFromJSON txt = 
  case decode txt of
    Ok debtorMap ->
      Right debtorMap
    Error msg ->
      Left msg
      
getJSONFromDebtors :: DebtorMap -> String
getJSONFromDebtors = (++"\n") . PP.render . pp_value . showJSON

instance JSON Day where
  readJSON val = do
    (year,month,day) <- readJSON val
    return $ fromGregorian year month day
  showJSON =
    showJSON . toGregorian

instance JSON TimeOfDay where
  readJSON val = do
    (hour, min, sec) <- readJSON val
    case makeTimeOfDayValid hour min (fromIntegral (sec :: Int)) of
      Just tod -> return tod
      Nothing -> fail "invalid time of day"
  showJSON tod =
    showJSON (todHour tod, todMin tod, (truncate $ todSec tod) :: Int)

instance JSON LocalTime where
  readJSON val = do
    (day, tod) <- readJSON val
    LocalTime <$> readJSON day <*> readJSON tod
  showJSON loc =
    showJSON (showJSON $ localDay loc, showJSON $ localTimeOfDay loc)

instance JSON Debtor where

  readJSON (JSObject jsobj) = do
    name <- field "name"
    phoneNum <- field "phone_num"
    exNum <- field "ex_num"
    addresses <- field "addresses"
    amount <- field "amount"
    exTime <- field "ex_time"
    comment <- field "comment"

    return Debtor
      { getName = name
      , getPhoneNum = phoneNum
      , getExecutionNum = exNum
      , getAddresses = addresses
      , getAmount = amount
      , getExecutionTime = exTime
      , getComment = comment
      }

    where obj = fromJSObject jsobj
          field :: JSON a => String -> Result a
          field key = 
            case lookup key obj of
              Just x -> readJSON x
              Nothing -> Error $ "expected field " ++ show key

  showJSON debtor =
    makeObj $ map (\(key,fun) -> (key, fun $ debtor)) fields
    where fields :: [(String, Debtor -> JSValue)]
          fields =
            [ ("name",      showJSON . getName)
            , ("phone_num", showJSON . getPhoneNum)
            , ("ex_num",    showJSON . getExecutionNum)
            , ("addresses", showJSON . map toJSString . getAddresses)
            , ("amount",    showJSON . getAmount)
            , ("ex_time",   showJSON . getExecutionTime) 
            , ("comment",   showJSON . getComment)
            ]
