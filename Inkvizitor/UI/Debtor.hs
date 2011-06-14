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

  addressesList <- listCtrl p 
    [ style := wxLC_EDIT_LABELS .|. wxLC_REPORT .|. wxLC_NO_HEADER .|. wxLC_SINGLE_SEL
    , columns := [("Address", AlignLeft, 200)]
    , items := (:[]) <$> getAddresses debtor
    ]
  addrAdd <- button p [text := "Add", tooltip := "Adds a new addres"]
  addrRemove <- button p [text := "Remove", tooltip := "Removes selected address"]

  selectedVar <- variable [value := (Nothing :: Maybe ListIndex)]

  set addressesList [on listEvent := \event -> 
    case event of
      ListItemSelected item ->
        set selectedVar [value := Just item]
      ListItemDeselected item ->
        set selectedVar [value := Nothing]
      other ->
        propagateEvent
    ]

  set addrAdd [on command := do
    itemAppend addressesList ["new address..."]
    appended <- (+(-1)) <$> get addressesList itemCount
    listCtrlEditLabel addressesList appended
    ]

  set addrRemove [on command := do
    selected <- get selectedVar value
    case selected of
      Just item ->
        itemDelete addressesList item
      Nothing ->
        return ()
    ]

  let
    getResult :: IO Debtor
    getResult = do
      name <- get nameInput text
      phone <- get phoneInput text
      exNum <- get exNumInput text
      addresses <- map head <$> get addressesList items
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
          , [alignLeft $ label "Addresses: ", hfill $ widget addressesList]
          , [glue, floatCenter $ row 5 [widget addrAdd, widget addrRemove]]
          , [alignLeft $ label "Amount: ", hfill $ widget amountInput]
          , [alignLeft $ label "Comment: ", hfill $ widget commentInput]
          ]
        , floatBottomRight $ row 5
          [ widget ok, widget cancel ]
        ]
    ]

