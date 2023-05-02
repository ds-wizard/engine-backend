module Wizard.Database.Mapping.BookReference.BookReference where

import Database.PostgreSQL.Simple

import Wizard.Model.BookReference.BookReference

instance ToRow BookReference

instance FromRow BookReference
