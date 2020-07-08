module Shared.Database.BSON.Template.Template where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Shared.Model.Template.Template

instance FromBSON Template

instance ToBSON Template

instance FromBSON TemplateAllowedPackage

instance ToBSON TemplateAllowedPackage

instance FromBSON TemplateFormat

instance ToBSON TemplateFormat

instance FromBSON TemplateFormatStep

instance ToBSON TemplateFormatStep

instance FromBSON TemplateFile

instance ToBSON TemplateFile

instance FromBSON TemplateAsset

instance ToBSON TemplateAsset
