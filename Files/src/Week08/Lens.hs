{-# LANGUAGE TemplateHaskell #-}

module Week08.Lens where

import Control.Lens

-- Convention to use and underscore when naming fields for use in Lens'
newtype Company = Company {_staff :: [Person]} deriving Show


data Person  = Person
    { _name    :: String
    , _address :: Address
    } deriving Show

newtype Address = Address {_city :: String} deriving Show

alejandro, lars :: Person
alejandro = Person
  {  _name    = "Alejandro"
  ,  _address = Address {_city = "Zacateca"}
  }
lars = Person
  {  _name    = "Lars"
  ,  _address = Address {_city = "Regensburg"}
  }

iohk :: Company
iohk = Company { _staff = [alejandro, lars] }

goTo :: String -> Company -> Company
goTo there c = c {_staff = map movePerson (_staff c)}
  where
    movePerson p = p {_address = (_address p) {_city = there}}


-- Create a lens to access nested data fields.
-- The names of the lenses do not contain the underscore
-- They are access through the . operator
-- The strength in lens' is that you can compose them
makeLenses ''Company
makeLenses ''Person
makeLenses ''Address


-- This willb update the city with the given string
-- & is to modify, and ^. is used to view
goTo' :: String -> Company -> Company
goTo' there c = c & staff . each . address . city .~ there
