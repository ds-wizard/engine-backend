module Database.Migration.Development.Template.Data.Templates where

import Data.Maybe (fromJust)
import qualified Data.UUID as U

import Api.Resource.Template.TemplateDTO

commonDSWTemplate =
  TemplateDTO
  { _templateDTOUuid = fromJust $ U.fromString "43a3fdd1-8535-42e0-81a7-5edbff296e65"
  , _templateDTOName = "Common DSW Template"
  , _templateDTORootFile = "root.html.j2"
  }
