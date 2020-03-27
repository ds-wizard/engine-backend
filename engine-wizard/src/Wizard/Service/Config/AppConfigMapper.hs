module Wizard.Service.Config.AppConfigMapper where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Model.Config.AppConfig
import Wizard.Service.Config.SimpleFeatureMapper

toFeaturesDTO :: AppConfigFeatures -> AppConfigFeaturesDTO
toFeaturesDTO config =
  AppConfigFeaturesDTO
    { _appConfigFeaturesDTOPublicQuestionnaire = toSimpleFeatureDTO $ config ^. publicQuestionnaire
    , _appConfigFeaturesDTOLevels = toSimpleFeatureDTO $ config ^. levels
    , _appConfigFeaturesDTOQuestionnaireAccessibility = toSimpleFeatureDTO $ config ^. questionnaireAccessibility
    }

toClientDTO :: AppConfigClient -> AppConfigClientDTO
toClientDTO config =
  AppConfigClientDTO
    { _appConfigClientDTOPrivacyUrl = config ^. privacyUrl
    , _appConfigClientDTOAppTitle = config ^. appTitle
    , _appConfigClientDTOAppTitleShort = config ^. appTitleShort
    , _appConfigClientDTOSupportEmail = config ^. supportEmail
    , _appConfigClientDTOSupportRepositoryName = config ^. supportRepositoryName
    , _appConfigClientDTOSupportRepositoryUrl = config ^. supportRepositoryUrl
    , _appConfigClientDTODashboard = toClientDashboardDTO <$> config ^. dashboard
    , _appConfigClientDTOCustomMenuLinks = toClientCustomMenuLinkDTO <$> config ^. customMenuLinks
    }

toClientDashboardDTO :: AppConfigClientDashboard -> AppConfigClientDashboardDTO
toClientDashboardDTO config =
  AppConfigClientDashboardDTO
    { _appConfigClientDashboardDTOAdmin = config ^. admin
    , _appConfigClientDashboardDTODataSteward = config ^. dataSteward
    , _appConfigClientDashboardDTOResearcher = config ^. researcher
    }

toClientCustomMenuLinkDTO :: AppConfigClientCustomMenuLink -> AppConfigClientCustomMenuLinkDTO
toClientCustomMenuLinkDTO config =
  AppConfigClientCustomMenuLinkDTO
    { _appConfigClientCustomMenuLinkDTOIcon = config ^. icon
    , _appConfigClientCustomMenuLinkDTOTitle = config ^. title
    , _appConfigClientCustomMenuLinkDTOUrl = config ^. url
    , _appConfigClientCustomMenuLinkDTONewWindow = config ^. newWindow
    }

toInfoDTO :: AppConfigInfo -> AppConfigInfoDTO
toInfoDTO config =
  AppConfigInfoDTO
    { _appConfigInfoDTOWelcomeWarning = config ^. welcomeWarning
    , _appConfigInfoDTOWelcomeInfo = config ^. welcomeInfo
    , _appConfigInfoDTOLoginInfo = config ^. loginInfo
    }

toAffiliationDTO :: AppConfigAffiliation -> AppConfigAffiliationDTO
toAffiliationDTO config = AppConfigAffiliationDTO {_appConfigAffiliationDTOAffiliations = config ^. affiliations}

toAuthDTO :: AppConfigAuth -> AppConfigAuthDTO
toAuthDTO config =
  AppConfigAuthDTO
    { _appConfigAuthDTOInternal = toAuthInternalDTO $ config ^. internal
    , _appConfigAuthDTOExternal = toAuthExternalDTO $ config ^. external
    }

toAuthInternalDTO :: AppConfigAuthInternal -> AppConfigAuthInternalDTO
toAuthInternalDTO config =
  AppConfigAuthInternalDTO {_appConfigAuthInternalDTORegistration = toSimpleFeatureDTO $ config ^. registration}

toAuthExternalDTO :: AppConfigAuthExternal -> AppConfigAuthExternalDTO
toAuthExternalDTO config =
  AppConfigAuthExternalDTO {_appConfigAuthExternalDTOServices = toAuthExternalServiceDTO <$> config ^. services}

toAuthExternalServiceDTO :: AppConfigAuthExternalService -> AppConfigAuthExternalServiceDTO
toAuthExternalServiceDTO config =
  AppConfigAuthExternalServiceDTO
    { _appConfigAuthExternalServiceDTOAId = config ^. aId
    , _appConfigAuthExternalServiceDTOName = config ^. name
    , _appConfigAuthExternalServiceDTOUrl = config ^. url
    , _appConfigAuthExternalServiceDTOClientId = config ^. clientId
    , _appConfigAuthExternalServiceDTOClientSecret = config ^. clientSecret
    , _appConfigAuthExternalServiceDTOParameters = toAppConfigAuthExternalServiceParameterDTO <$> config ^. parameters
    , _appConfigAuthExternalServiceDTOStyle = toAppConfigAuthExternalServiceStyleDTO $ config ^. style
    }

toAppConfigAuthExternalServiceParameterDTO ::
     AppConfigAuthExternalServiceParameter -> AppConfigAuthExternalServiceParameterDTO
toAppConfigAuthExternalServiceParameterDTO config =
  AppConfigAuthExternalServiceParameterDTO
    { _appConfigAuthExternalServiceParameterDTOName = config ^. name
    , _appConfigAuthExternalServiceParameterDTOValue = config ^. value
    }

toAppConfigAuthExternalServiceStyleDTO :: AppConfigAuthExternalServiceStyle -> AppConfigAuthExternalServiceStyleDTO
toAppConfigAuthExternalServiceStyleDTO config =
  AppConfigAuthExternalServiceStyleDTO
    { _appConfigAuthExternalServiceStyleDTOIcon = config ^. icon
    , _appConfigAuthExternalServiceStyleDTOBackground = config ^. background
    , _appConfigAuthExternalServiceStyleDTOColor = config ^. color
    }

fromFeaturesDTO :: AppConfigFeaturesDTO -> AppConfigFeatures
fromFeaturesDTO dto =
  AppConfigFeatures
    { _appConfigFeaturesPublicQuestionnaire = fromSimpleFeatureDTO $ dto ^. publicQuestionnaire
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

fromAuthDTO :: AppConfigAuthDTO -> AppConfigAuth
fromAuthDTO dto =
  AppConfigAuth
    { _appConfigAuthInternal = fromAuthInternalDTO $ dto ^. internal
    , _appConfigAuthExternal = fromAuthExternalDTO $ dto ^. external
    }

fromAuthInternalDTO :: AppConfigAuthInternalDTO -> AppConfigAuthInternal
fromAuthInternalDTO dto =
  AppConfigAuthInternal {_appConfigAuthInternalRegistration = fromSimpleFeatureDTO $ dto ^. registration}

fromAuthExternalDTO :: AppConfigAuthExternalDTO -> AppConfigAuthExternal
fromAuthExternalDTO dto =
  AppConfigAuthExternal {_appConfigAuthExternalServices = fromAuthExternalServiceDTO <$> dto ^. services}

fromAuthExternalServiceDTO :: AppConfigAuthExternalServiceDTO -> AppConfigAuthExternalService
fromAuthExternalServiceDTO dto =
  AppConfigAuthExternalService
    { _appConfigAuthExternalServiceAId = dto ^. aId
    , _appConfigAuthExternalServiceName = dto ^. name
    , _appConfigAuthExternalServiceUrl = dto ^. url
    , _appConfigAuthExternalServiceClientId = dto ^. clientId
    , _appConfigAuthExternalServiceClientSecret = dto ^. clientSecret
    , _appConfigAuthExternalServiceParameters = fromAppConfigAuthExternalServiceParameterDTO <$> dto ^. parameters
    , _appConfigAuthExternalServiceStyle = fromAppConfigAuthExternalServiceStyleDTO $ dto ^. style
    }

fromAppConfigAuthExternalServiceParameterDTO ::
     AppConfigAuthExternalServiceParameterDTO -> AppConfigAuthExternalServiceParameter
fromAppConfigAuthExternalServiceParameterDTO dto =
  AppConfigAuthExternalServiceParameter
    { _appConfigAuthExternalServiceParameterName = dto ^. name
    , _appConfigAuthExternalServiceParameterValue = dto ^. value
    }

fromAppConfigAuthExternalServiceStyleDTO :: AppConfigAuthExternalServiceStyleDTO -> AppConfigAuthExternalServiceStyle
fromAppConfigAuthExternalServiceStyleDTO dto =
  AppConfigAuthExternalServiceStyle
    { _appConfigAuthExternalServiceStyleIcon = dto ^. icon
    , _appConfigAuthExternalServiceStyleBackground = dto ^. background
    , _appConfigAuthExternalServiceStyleColor = dto ^. color
    }
