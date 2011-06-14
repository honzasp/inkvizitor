module Inkvizitor.UI.Debtor
  ( showDebtorForm
  )
where

import Control.Applicative
import Data.Bits
import Graphics.UI.WX
import Graphics.UI.WXCore

import Inkvizitor.Debtor
import Inkvizitor.UI.Gui

showDebtorForm :: Gui -> Debtor -> (Debtor -> IO ()) -> IO ()
showDebtorForm g debtor onResult = do
  f <- frame [text := "Edit debtor"]
  p <- panel f []

  ok <- button p [text := "Ok"]
  cancel <- button p [text := "Cancel"]

  nameInput  <- textEntry p [text := getName debtor]
  phoneInput <- textEntry p [text := getPhoneNum debtor]
  exNumInput <- textEntry p [text := getExecutionNum debtor]
  amountInput <- spinCtrl p 0 maxBound [selection := getAmount debtor]
  commentInput <- textCtrl p [text := getComment debtor]

  addrList <- singleListBox p 
    [ style := wxLB_NEEDED_SB
    , items := getAddresses debtor
    ]

  addrAdd <- button p [text := "Add", tooltip := "Adds a new addres"]
  addrRemove <- button p [text := "Remove", tooltip := "Removes selected address"]

  addrText <- textCtrl p [enabled := False]
  addrSet <- button p [text := "Set", tooltip := "Sets the address", enabled := False]

  set addrAdd [on command := do
    itemAppend addrList "New address..."
    ]

  set addrRemove [on command := do
    id <- get addrList selection
    if id /= (-1)
      then itemDelete addrList id
      else return ()
    ]

  set addrList [on select := do
    id <- get addrList selection
    if id /= (-1)
      then do
        address <- get addrList (item id)
        set addrText [text := address , enabled := True]
        set addrSet [enabled := True]
      else do
        set addrText [text := "", enabled := False]
        set addrSet [enabled := False]
    propagateEvent
    ]

  set addrSet [on command := do
    id <- get addrList selection
    if id /= (-1)
      then do
        address <- get addrText text
        set addrList [item id := address]
      else
        return ()
    ]

  let
    getResult :: IO Debtor
    getResult = do
      name <- get nameInput text
      phone <- get phoneInput text
      exNum <- get exNumInput text
      addresses <- get addrList items
      amount <- get amountInput selection 
      comment <- get commentInput text

      return $ debtor
        { getName = name
        , getPhoneNum = phone
        , getExecutionNum = exNum
        , getAddresses = addresses
        , getAmount = amount
        , getComment = comment
        }

  set ok [on command := do
    result <- getResult
    onResult result
    close f
    ]
  set cancel [on command := close f]

  set f 
    [ defaultButton := ok
    , layout := container p $ margin 10 $ minsize (sz 400 100) $ column 5
        [ grid 5 5 
          [ [alignLeft $ label "Name: ", hfill $ widget nameInput]
          , [alignLeft $ label "Phone number: ", hfill $ widget phoneInput]
          , [alignLeft $ label "Execution number: ", hfill $ widget exNumInput]
          , [alignLeft $ label "Addresses: ", fill . minsize (sz (-1) 150) $ widget addrList]
          , [glue, hfloatCenter $ row 5 [widget addrAdd, widget addrRemove]]
          , [glue, fill $ widget addrText]
          , [glue, hfloatRight $ widget addrSet]
          , [alignLeft $ label "Amount: ", hfill $ widget amountInput]
          , [alignLeft $ label "Comment: ", hfill $ widget commentInput]
          ]
        , hfloatRight $ row 5
          [ widget ok, widget cancel ]
        ]
    ]

