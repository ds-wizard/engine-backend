module Wizard.Service.Config.SimpleFeatureMapper where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Config.SimpleFeatureDTO
import Wizard.Model.Config.SimpleFeature

toSimpleFeatureDTO :: HasEnabled feature Bool => feature -> SimpleFeatureDTO
toSimpleFeatureDTO simpleFeature = SimpleFeatureDTO {_simpleFeatureDTOEnabled = simpleFeature ^. enabled}

fromSimpleFeatureDTO :: SimpleFeatureDTO -> SimpleFeature
fromSimpleFeatureDTO dto = SimpleFeature {_simpleFeatureEnabled = dto ^. enabled}
