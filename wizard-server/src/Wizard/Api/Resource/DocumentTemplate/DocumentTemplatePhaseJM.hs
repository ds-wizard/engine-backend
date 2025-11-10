module Wizard.Api.Resource.DocumentTemplate.DocumentTemplatePhaseJM where

import qualified Data.Text as T
import Servant.API
import Text.Read (readMaybe)

import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

instance FromHttpApiData DocumentTemplatePhase where
  parseQueryParam a =
    case readMaybe (T.unpack a) of
      Just phase -> Right phase
      Nothing -> Left "Unable to parse DocumentTemplatePhase"
