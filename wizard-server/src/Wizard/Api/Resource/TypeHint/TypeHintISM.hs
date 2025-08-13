module Wizard.Api.Resource.TypeHint.TypeHintISM where

import Data.Swagger

import Shared.Common.Api.Resource.Common.AesonSM ()
import Shared.Common.Util.Swagger
import Wizard.Api.Resource.TypeHint.TypeHintIJM ()
import Wizard.Database.Migration.Development.TypeHint.Data.TypeHints
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO

instance ToSchema TypeHintLegacyIDTO where
  declareNamedSchema = toSwagger lifeScienceLegacyTypeHint

instance ToSchema TypeHintIDTO where
  declareNamedSchema = toSwagger genomicDatasetTypeHint
