module Shared.Database.BSON.Template.TemplateGroup where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Shared.Database.BSON.Template.Template ()
import Shared.Model.Template.TemplateGroup

instance ToBSON TemplateGroup

instance FromBSON TemplateGroup
