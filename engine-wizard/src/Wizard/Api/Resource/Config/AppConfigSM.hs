module Wizard.Api.Resource.Config.AppConfigSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Api.Resource.Config.SimpleFeatureSM ()
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Service.Config.AppConfigMapper

instance ToSchema AppConfigFeaturesDTO where
  declareNamedSchema = simpleToSchema "_appConfigFeaturesDTO" (toFeaturesDTO defaultFeatures)

instance ToSchema AppConfigClientDTO where
  declareNamedSchema = simpleToSchema "_appConfigClientDTO" (toClientDTO defaultClient)

instance ToSchema AppConfigClientDashboardDTO where
  declareNamedSchema = simpleToSchema "_appConfigDashboardDTO" (toClientDashboardDTO defaultClientDashboard)

instance ToSchema AppConfigClientCustomMenuLinkDTO where
  declareNamedSchema =
    simpleToSchema "_appConfigClientCustomMenuLinkDTO" (toClientCustomMenuLinkDTO defaultClientCustomLink)

instance ToSchema AppConfigInfoDTO where
  declareNamedSchema = simpleToSchema "_appConfigInfoDTO" (toInfoDTO defaultInfo)

instance ToSchema AppConfigAffiliationDTO where
  declareNamedSchema = simpleToSchema "_appConfigAffiliationDTO" (toAffiliationDTO defaultAffiliation)
