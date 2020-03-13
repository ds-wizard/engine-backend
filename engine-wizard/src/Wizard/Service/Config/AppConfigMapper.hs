module Wizard.Service.Config.AppConfigMapper where

import Control.Lens ((^.))
import Data.Time

import LensesConfig
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Model.Config.AppConfig
import Wizard.Service.Config.SimpleFeatureMapper

toDTO :: AppConfig -> AppConfigDTO
toDTO appConfig =
  AppConfigDTO
    { _appConfigDTOFeatures = toFeaturesDTO $ appConfig ^. features
    , _appConfigDTOClient = toClientDTO $ appConfig ^. client
    , _appConfigDTOCreatedAt = appConfig ^. createdAt
    , _appConfigDTOUpdatedAt = appConfig ^. updatedAt
    }

toFeaturesDTO :: AppConfigFeatures -> AppConfigFeaturesDTO
toFeaturesDTO featuresConfig =
  AppConfigFeaturesDTO
    { _appConfigFeaturesDTORegistration = toSimpleFeatureDTO $ featuresConfig ^. registration
    , _appConfigFeaturesDTOPublicQuestionnaire = toSimpleFeatureDTO $ featuresConfig ^. publicQuestionnaire
    , _appConfigFeaturesDTOLevels = toSimpleFeatureDTO $ featuresConfig ^. levels
    , _appConfigFeaturesDTOQuestionnaireAccessibility =
        toSimpleFeatureDTO $ featuresConfig ^. questionnaireAccessibility
    }

toClientDTO :: AppConfigClient -> AppConfigClientDTO
toClientDTO clientConfig =
  AppConfigClientDTO
    { _appConfigClientDTOPrivacyUrl = clientConfig ^. privacyUrl
    , _appConfigClientDTOAppTitle = clientConfig ^. appTitle
    , _appConfigClientDTOAppTitleShort = clientConfig ^. appTitleShort
    , _appConfigClientDTOWelcomeWarning = clientConfig ^. welcomeWarning
    , _appConfigClientDTOWelcomeInfo = clientConfig ^. welcomeInfo
    , _appConfigClientDTOLoginInfo = clientConfig ^. loginInfo
    , _appConfigClientDTOSupportEmail = clientConfig ^. supportEmail
    , _appConfigClientDTOSupportRepositoryName = clientConfig ^. supportRepositoryName
    , _appConfigClientDTOSupportRepositoryUrl = clientConfig ^. supportRepositoryUrl
    , _appConfigClientDTODashboard = toClientDashboardDTO <$> clientConfig ^. dashboard
    , _appConfigClientDTOCustomMenuLinks = toClientCustomMenuLinkDTO <$> clientConfig ^. customMenuLinks
    }

toClientDashboardDTO :: AppConfigClientDashboard -> AppConfigClientDashboardDTO
toClientDashboardDTO dashboardConfig =
  AppConfigClientDashboardDTO
    { _appConfigClientDashboardDTOAdmin = dashboardConfig ^. admin
    , _appConfigClientDashboardDTODataSteward = dashboardConfig ^. dataSteward
    , _appConfigClientDashboardDTOResearcher = dashboardConfig ^. researcher
    }

toClientCustomMenuLinkDTO :: AppConfigClientCustomMenuLink -> AppConfigClientCustomMenuLinkDTO
toClientCustomMenuLinkDTO customMenuLinkConfig =
  AppConfigClientCustomMenuLinkDTO
    { _appConfigClientCustomMenuLinkDTOIcon = customMenuLinkConfig ^. icon
    , _appConfigClientCustomMenuLinkDTOTitle = customMenuLinkConfig ^. title
    , _appConfigClientCustomMenuLinkDTOUrl = customMenuLinkConfig ^. url
    , _appConfigClientCustomMenuLinkDTONewWindow = customMenuLinkConfig ^. newWindow
    }

toChangeDTO :: AppConfig -> AppConfigChangeDTO
toChangeDTO appConfig =
  AppConfigChangeDTO
    { _appConfigChangeDTOFeatures = toFeaturesDTO $ appConfig ^. features
    , _appConfigChangeDTOClient = toClientDTO $ appConfig ^. client
    }

fromDTO :: AppConfigChangeDTO -> AppConfig -> UTCTime -> AppConfig
fromDTO dto appConfig now =
  AppConfig
    { _appConfigFeatures = fromFeaturesDTO $ dto ^. features
    , _appConfigClient = fromClientDTO $ dto ^. client
    , _appConfigCreatedAt = appConfig ^. createdAt
    , _appConfigUpdatedAt = now
    }

fromFeaturesDTO :: AppConfigFeaturesDTO -> AppConfigFeatures
fromFeaturesDTO dto =
  AppConfigFeatures
    { _appConfigFeaturesRegistration = fromSimpleFeatureDTO $ dto ^. registration
    , _appConfigFeaturesPublicQuestionnaire = fromSimpleFeatureDTO $ dto ^. publicQuestionnaire
    , _appConfigFeaturesLevels = fromSimpleFeatureDTO $ dto ^. levels
    , _appConfigFeaturesQuestionnaireAccessibility = fromSimpleFeatureDTO $ dto ^. questionnaireAccessibility
    }

fromClientDTO :: AppConfigClientDTO -> AppConfigClient
fromClientDTO dto =
  AppConfigClient
    { _appConfigClientPrivacyUrl = dto ^. privacyUrl
    , _appConfigClientAppTitle = dto ^. appTitle
    , _appConfigClientAppTitleShort = dto ^. appTitleShort
    , _appConfigClientWelcomeWarning = dto ^. welcomeWarning
    , _appConfigClientWelcomeInfo = dto ^. welcomeInfo
    , _appConfigClientLoginInfo = dto ^. loginInfo
    , _appConfigClientSupportEmail = dto ^. supportEmail
    , _appConfigClientSupportRepositoryName = dto ^. supportRepositoryName
    , _appConfigClientSupportRepositoryUrl = dto ^. supportRepositoryUrl
    , _appConfigClientDashboard = fromClientDashboardDTO <$> dto ^. dashboard
    , _appConfigClientCustomMenuLinks = fromClientCustomMenuLinkDTO <$> dto ^. customMenuLinks
    }

fromClientDashboardDTO :: AppConfigClientDashboardDTO -> AppConfigClientDashboard
fromClientDashboardDTO dto =
  AppConfigClientDashboard
    { _appConfigClientDashboardAdmin = dto ^. admin
    , _appConfigClientDashboardDataSteward = dto ^. dataSteward
    , _appConfigClientDashboardResearcher = dto ^. researcher
    }

fromClientCustomMenuLinkDTO :: AppConfigClientCustomMenuLinkDTO -> AppConfigClientCustomMenuLink
fromClientCustomMenuLinkDTO dto =
  AppConfigClientCustomMenuLink
    { _appConfigClientCustomMenuLinkIcon = dto ^. icon
    , _appConfigClientCustomMenuLinkTitle = dto ^. title
    , _appConfigClientCustomMenuLinkUrl = dto ^. url
    , _appConfigClientCustomMenuLinkNewWindow = dto ^. newWindow
    }
