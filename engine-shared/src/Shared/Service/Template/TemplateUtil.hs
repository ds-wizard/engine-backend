module Shared.Service.Template.TemplateUtil where

import Control.Lens ((^.))
import qualified Data.List as L

import LensesConfig
import Shared.Model.Template.Template
import Shared.Util.List (groupBy)

groupTemplates :: [Template] -> [[Template]]
groupTemplates =
  groupBy (\t1 t2 -> (t1 ^. organizationId) == (t2 ^. organizationId) && (t1 ^. templateId) == (t2 ^. templateId))
