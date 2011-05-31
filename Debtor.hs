module Debtor
  ( Debtor(..)
  , DebtorMap(..)
  , getDebtorsFromJSON
  )
where

import Data.Time
import qualified Data.Map as Map
import Text.JSON

data Debtor = Debtor
  { getName :: String
  , getPhoneNum :: String
  , getExecutionNum :: String
  , getAddresses :: [String]
  , getAmount :: Int
  , getExecutionTime :: Maybe UTCTime
  , getComment :: String
  } deriving (Eq, Show)

type DebtorMap = Map.Map String [Debtor]

getDebtorsFromJSON :: String -> IO (Either String DebtorMap)
getDebtorsFromJSON txt = do
  case decode txt of
    Ok debtorMap ->
      return $ Right debtorMap
    Error msg ->
      return $ Left msg

instance JSON Debtor where

  readJSON (JSObject jsobj) = do
    name <- field "name"
    phoneNum <- field "phone_num"
    exNum <- field "ex_num"
    addresses <- field "addresses"
    amount <- field "amount"
    exTime <- return Nothing -- TODO
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
    makeObj $ map (\(key,val) -> (key, val debtor)) fields
    where fields :: [(String, Debtor -> JSValue)]
          fields =
            [ ("name",      showJSON . getName)
            , ("phone_num", showJSON . getPhoneNum)
            , ("ex_num",    showJSON . getExecutionNum)
            , ("addresses", showJSON . map toJSString . getAddresses)
            , ("amount",    showJSON . getAmount)
            , ("ex_time",   showJSON . const (Nothing :: Maybe JSValue)) -- TODO
            , ("comment",   showJSON . getComment)
            ]
