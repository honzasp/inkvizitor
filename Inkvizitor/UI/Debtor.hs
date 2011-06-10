module Inkvizitor.UI.Debtor
  ( showDebtorForm
  )
where

import Graphics.UI.WX
import Graphics.UI.WXCore

import Inkvizitor.Debtor
import Inkvizitor.UI.Gui

showDebtorForm :: Gui -> Debtor -> (Debtor -> IO ()) -> IO ()
showDebtorForm g debtor onResult = do
  f <- frame [text := "Edit debtor"]
  p <- panel f []

  ok <- button p [text := "Ok"]
  cancel <- button p [text := "Cancel", on command := do
    close f
    ]

  nameInput  <- textEntry p [text := getName debtor]
  phoneInput <- textEntry p [text := getPhoneNum debtor]
  exNumInput <- textEntry p [text := getExecutionNum debtor]
  -- TODO: addresses!
  amountInput <- spinCtrl p 0 maxBound [selection := getAmount debtor]
  commentInput <- textCtrl p [text := getComment debtor]

  let
    getResult :: IO Debtor
    getResult = do
      name <- get nameInput text
      phone <- get phoneInput text
      exNum <- get exNumInput text
      amount <- get amountInput selection 
      comment <- get commentInput text

      return $ debtor
        { getName = name
        , getPhoneNum = phone
        , getExecutionNum = exNum
        , getAmount = amount
        , getComment = comment
        }

  set ok [on command := do
    result <- getResult
    onResult result
    close f
    ]

  set f 
    [ defaultButton := ok
    , layout := container p $ margin 10 $ minsize (sz 400 100) $ column 5
        [ grid 5 5 $ map (\(name,input) -> [alignLeft $ label name, hfill input])
          [ ("Name: ", widget nameInput)
          , ("Phone number: ", widget phoneInput)
          , ("Execution number: ", widget exNumInput)
          , ("Amount: ", widget amountInput)
          , ("Comment: ", widget commentInput)
          ]
        , floatBottomRight $ row 5
          [ widget ok, widget cancel ]
        ]
    ]

