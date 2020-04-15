module Wizard.Database.BSON.BookReference.BookReference where

import Data.Bson.Generic

import Wizard.Model.BookReference.BookReference

instance ToBSON BookReference

instance FromBSON BookReference
