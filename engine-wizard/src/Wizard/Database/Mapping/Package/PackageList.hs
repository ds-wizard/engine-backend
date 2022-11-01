module Wizard.Database.Mapping.Package.PackageList where

import Database.PostgreSQL.Simple

import Wizard.Database.Mapping.Package.PackageState ()
import Wizard.Model.Package.PackageList

instance FromRow PackageList
