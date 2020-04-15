module Wizard.Database.BSON.Document.Document where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Database.BSON.Document.DocumentType ()
import Wizard.Model.Document.Document

instance ToBSON DocumentMetadata

instance FromBSON DocumentMetadata

instance ToBSON Document

instance FromBSON Document
