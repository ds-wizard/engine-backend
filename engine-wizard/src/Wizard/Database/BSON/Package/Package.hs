module Wizard.Database.BSON.Package.Package where

import Data.Bson.Generic

import Shared.Model.Package.Package

instance ToBSON Package

instance FromBSON Package
