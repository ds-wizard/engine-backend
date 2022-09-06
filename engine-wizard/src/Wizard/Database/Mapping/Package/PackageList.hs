module Wizard.Database.Mapping.Package.PackageList where

import Database.PostgreSQL.Simple

import Wizard.Model.Package.PackageList

instance FromRow PackageList
