module Wizard.Service.Config.AppConfigMapper where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Model.Config.AppConfig
import Wizard.Service.Config.SimpleFeatureMapper

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

toInfoDTO :: AppConfigInfo -> AppConfigInfoDTO
toInfoDTO infoConfig =
  AppConfigInfoDTO
    { _appConfigInfoDTOWelcomeWarning = infoConfig ^. welcomeWarning
    , _appConfigInfoDTOWelcomeInfo = infoConfig ^. welcomeInfo
    , _appConfigInfoDTOLoginInfo = infoConfig ^. loginInfo
    }

toAffiliationDTO :: AppConfigAffiliation -> AppConfigAffiliationDTO
toAffiliationDTO affiliationConfig =
  AppConfigAffiliationDTO {_appConfigAffiliationDTOAffiliations = affiliationConfig ^. affiliations}

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

fromInfoDTO :: AppConfigInfoDTO -> AppConfigInfo
fromInfoDTO dto =
  AppConfigInfo
    { _appConfigInfoWelcomeWarning = dto ^. welcomeWarning
    , _appConfigInfoWelcomeInfo = dto ^. welcomeInfo
    , _appConfigInfoLoginInfo = dto ^. loginInfo
    }

fromAffiliationDTO :: AppConfigAffiliationDTO -> AppConfigAffiliation
fromAffiliationDTO dto = AppConfigAffiliation {_appConfigAffiliationAffiliations = dto ^. affiliations}
