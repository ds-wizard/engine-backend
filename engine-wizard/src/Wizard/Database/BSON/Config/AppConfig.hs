module Wizard.Database.BSON.Config.AppConfig where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Wizard.Database.BSON.Common ()
import Wizard.Database.BSON.Config.SimpleFeature ()
import Wizard.Model.Config.AppConfig

instance ToBSON AppConfig where
  toBSON AppConfig {..} =
    [ "organization" BSON.=: _appConfigOrganization
    , "authentication" BSON.=: _appConfigAuthentication
    , "privacyAndSupport" BSON.=: _appConfigPrivacyAndSupport
    , "dashboard" BSON.=: _appConfigDashboard
    , "lookAndFeel" BSON.=: _appConfigLookAndFeel
    , "knowledgeModelRegistry" BSON.=: _appConfigKnowledgeModelRegistry
    , "questionnaire" BSON.=: _appConfigQuestionnaire
    , "createdAt" BSON.=: _appConfigCreatedAt
    , "updatedAt" BSON.=: _appConfigUpdatedAt
    ]

instance FromBSON AppConfig where
  fromBSON doc = do
    _appConfigOrganization <- BSON.lookup "organization" doc
    _appConfigAuthentication <- BSON.lookup "authentication" doc
    _appConfigPrivacyAndSupport <- BSON.lookup "privacyAndSupport" doc
    _appConfigDashboard <- BSON.lookup "dashboard" doc
    _appConfigLookAndFeel <- BSON.lookup "lookAndFeel" doc
    _appConfigKnowledgeModelRegistry <- BSON.lookup "knowledgeModelRegistry" doc
    _appConfigQuestionnaire <- BSON.lookup "questionnaire" doc
    _appConfigCreatedAt <- BSON.lookup "createdAt" doc
    _appConfigUpdatedAt <- BSON.lookup "updatedAt" doc
    return AppConfig {..}

instance ToBSON AppConfigOrganization where
  toBSON AppConfigOrganization {..} =
    [ "name" BSON.=: _appConfigOrganizationName
    , "organizationId" BSON.=: _appConfigOrganizationOrganizationId
    , "affiliations" BSON.=: _appConfigOrganizationAffiliations
    ]

instance FromBSON AppConfigOrganization where
  fromBSON doc = do
    _appConfigOrganizationName <- BSON.lookup "name" doc
    _appConfigOrganizationOrganizationId <- BSON.lookup "organizationId" doc
    _appConfigOrganizationAffiliations <- BSON.lookup "affiliations" doc
    return AppConfigOrganization {..}

instance ToBSON AppConfigAuth where
  toBSON AppConfigAuth {..} =
    [ "defaultRole" BSON.=: _appConfigAuthDefaultRole
    , "internal" BSON.=: _appConfigAuthInternal
    , "external" BSON.=: _appConfigAuthExternal
    ]

instance FromBSON AppConfigAuth where
  fromBSON doc = do
    _appConfigAuthDefaultRole <- BSON.lookup "defaultRole" doc
    _appConfigAuthInternal <- BSON.lookup "internal" doc
    _appConfigAuthExternal <- BSON.lookup "external" doc
    return AppConfigAuth {..}

instance ToBSON AppConfigAuthInternal where
  toBSON AppConfigAuthInternal {..} = ["registration" BSON.=: _appConfigAuthInternalRegistration]

instance FromBSON AppConfigAuthInternal where
  fromBSON doc = do
    _appConfigAuthInternalRegistration <- BSON.lookup "registration" doc
    return AppConfigAuthInternal {..}

instance ToBSON AppConfigAuthExternal where
  toBSON AppConfigAuthExternal {..} = ["services" BSON.=: _appConfigAuthExternalServices]

instance FromBSON AppConfigAuthExternal where
  fromBSON doc = do
    _appConfigAuthExternalServices <- BSON.lookup "services" doc
    return AppConfigAuthExternal {..}

instance ToBSON AppConfigAuthExternalService where
  toBSON AppConfigAuthExternalService {..} =
    [ "id" BSON.=: _appConfigAuthExternalServiceAId
    , "name" BSON.=: _appConfigAuthExternalServiceName
    , "url" BSON.=: _appConfigAuthExternalServiceUrl
    , "clientId" BSON.=: _appConfigAuthExternalServiceClientId
    , "clientSecret" BSON.=: _appConfigAuthExternalServiceClientSecret
    , "parameteres" BSON.=: _appConfigAuthExternalServiceParameters
    , "style" BSON.=: _appConfigAuthExternalServiceStyle
    ]

instance FromBSON AppConfigAuthExternalService where
  fromBSON doc = do
    _appConfigAuthExternalServiceAId <- BSON.lookup "id" doc
    _appConfigAuthExternalServiceName <- BSON.lookup "name" doc
    _appConfigAuthExternalServiceUrl <- BSON.lookup "url" doc
    _appConfigAuthExternalServiceClientId <- BSON.lookup "clientId" doc
    _appConfigAuthExternalServiceClientSecret <- BSON.lookup "clientSecret" doc
    _appConfigAuthExternalServiceParameters <- BSON.lookup "parameteres" doc
    _appConfigAuthExternalServiceStyle <- BSON.lookup "style" doc
    return AppConfigAuthExternalService {..}

instance ToBSON AppConfigAuthExternalServiceParameter where
  toBSON AppConfigAuthExternalServiceParameter {..} =
    [ "name" BSON.=: _appConfigAuthExternalServiceParameterName
    , "value" BSON.=: _appConfigAuthExternalServiceParameterValue
    ]

instance FromBSON AppConfigAuthExternalServiceParameter where
  fromBSON doc = do
    _appConfigAuthExternalServiceParameterName <- BSON.lookup "name" doc
    _appConfigAuthExternalServiceParameterValue <- BSON.lookup "value" doc
    return AppConfigAuthExternalServiceParameter {..}

instance ToBSON AppConfigAuthExternalServiceStyle where
  toBSON AppConfigAuthExternalServiceStyle {..} =
    [ "icon" BSON.=: _appConfigAuthExternalServiceStyleIcon
    , "background" BSON.=: _appConfigAuthExternalServiceStyleBackground
    , "color" BSON.=: _appConfigAuthExternalServiceStyleColor
    ]

instance FromBSON AppConfigAuthExternalServiceStyle where
  fromBSON doc = do
    _appConfigAuthExternalServiceStyleIcon <- BSON.lookup "icon" doc
    _appConfigAuthExternalServiceStyleBackground <- BSON.lookup "background" doc
    _appConfigAuthExternalServiceStyleColor <- BSON.lookup "color" doc
    return AppConfigAuthExternalServiceStyle {..}

instance ToBSON AppConfigPrivacyAndSupport where
  toBSON AppConfigPrivacyAndSupport {..} =
    [ "privacyUrl" BSON.=: _appConfigPrivacyAndSupportPrivacyUrl
    , "supportEmail" BSON.=: _appConfigPrivacyAndSupportSupportEmail
    , "supportRepositoryName" BSON.=: _appConfigPrivacyAndSupportSupportRepositoryName
    , "supportRepositoryUrl" BSON.=: _appConfigPrivacyAndSupportSupportRepositoryUrl
    ]

instance FromBSON AppConfigPrivacyAndSupport where
  fromBSON doc = do
    _appConfigPrivacyAndSupportPrivacyUrl <- BSON.lookup "privacyUrl" doc
    _appConfigPrivacyAndSupportSupportEmail <- BSON.lookup "supportEmail" doc
    _appConfigPrivacyAndSupportSupportRepositoryName <- BSON.lookup "supportRepositoryName" doc
    _appConfigPrivacyAndSupportSupportRepositoryUrl <- BSON.lookup "supportRepositoryUrl" doc
    return AppConfigPrivacyAndSupport {..}

instance ToBSON AppConfigDashboard where
  toBSON AppConfigDashboard {..} =
    [ "widgets" BSON.=: _appConfigDashboardWidgets
    , "welcomeWarning" BSON.=: _appConfigDashboardWelcomeWarning
    , "welcomeInfo" BSON.=: _appConfigDashboardWelcomeInfo
    ]

instance FromBSON AppConfigDashboard where
  fromBSON doc = do
    _appConfigDashboardWidgets <- BSON.lookup "widgets" doc
    _appConfigDashboardWelcomeWarning <- BSON.lookup "welcomeWarning" doc
    _appConfigDashboardWelcomeInfo <- BSON.lookup "welcomeInfo" doc
    return AppConfigDashboard {..}

instance ToBSON AppConfigDashboardWidgets where
  toBSON AppConfigDashboardWidgets {..} =
    [ "admin" BSON.=: _appConfigDashboardWidgetsAdmin
    , "dataSteward" BSON.=: _appConfigDashboardWidgetsDataSteward
    , "researcher" BSON.=: _appConfigDashboardWidgetsResearcher
    ]

instance FromBSON AppConfigDashboardWidgets where
  fromBSON doc = do
    _appConfigDashboardWidgetsAdmin <- BSON.lookup "admin" doc
    _appConfigDashboardWidgetsDataSteward <- BSON.lookup "dataSteward" doc
    _appConfigDashboardWidgetsResearcher <- BSON.lookup "researcher" doc
    return AppConfigDashboardWidgets {..}

instance ToBSON AppConfigLookAndFeel where
  toBSON AppConfigLookAndFeel {..} =
    [ "appTitle" BSON.=: _appConfigLookAndFeelAppTitle
    , "appTitleShort" BSON.=: _appConfigLookAndFeelAppTitleShort
    , "customMenuLinks" BSON.=: _appConfigLookAndFeelCustomMenuLinks
    , "loginInfo" BSON.=: _appConfigLookAndFeelLoginInfo
    ]

instance FromBSON AppConfigLookAndFeel where
  fromBSON doc = do
    _appConfigLookAndFeelAppTitle <- BSON.lookup "appTitle" doc
    _appConfigLookAndFeelAppTitleShort <- BSON.lookup "appTitleShort" doc
    _appConfigLookAndFeelCustomMenuLinks <- BSON.lookup "customMenuLinks" doc
    _appConfigLookAndFeelLoginInfo <- BSON.lookup "loginInfo" doc
    return AppConfigLookAndFeel {..}

instance ToBSON AppConfigLookAndFeelCustomMenuLink where
  toBSON AppConfigLookAndFeelCustomMenuLink {..} =
    [ "icon" BSON.=: _appConfigLookAndFeelCustomMenuLinkIcon
    , "title" BSON.=: _appConfigLookAndFeelCustomMenuLinkTitle
    , "url" BSON.=: _appConfigLookAndFeelCustomMenuLinkUrl
    , "newWindow" BSON.=: _appConfigLookAndFeelCustomMenuLinkNewWindow
    ]

instance FromBSON AppConfigLookAndFeelCustomMenuLink where
  fromBSON doc = do
    _appConfigLookAndFeelCustomMenuLinkIcon <- BSON.lookup "icon" doc
    _appConfigLookAndFeelCustomMenuLinkTitle <- BSON.lookup "title" doc
    _appConfigLookAndFeelCustomMenuLinkUrl <- BSON.lookup "url" doc
    _appConfigLookAndFeelCustomMenuLinkNewWindow <- BSON.lookup "newWindow" doc
    return AppConfigLookAndFeelCustomMenuLink {..}

instance ToBSON AppConfigRegistry where
  toBSON AppConfigRegistry {..} = ["enabled" BSON.=: _appConfigRegistryEnabled, "token" BSON.=: _appConfigRegistryToken]

instance FromBSON AppConfigRegistry where
  fromBSON doc = do
    _appConfigRegistryEnabled <- BSON.lookup "enabled" doc
    _appConfigRegistryToken <- BSON.lookup "token" doc
    return AppConfigRegistry {..}

instance ToBSON AppConfigQuestionnaire where
  toBSON AppConfigQuestionnaire {..} =
    [ "questionnaireAccessibility" BSON.=: _appConfigQuestionnaireQuestionnaireAccessibility
    , "levels" BSON.=: _appConfigQuestionnaireLevels
    , "feedback" BSON.=: _appConfigQuestionnaireFeedback
    , "publicQuestionnaire" BSON.=: _appConfigQuestionnairePublicQuestionnaire
    ]

instance FromBSON AppConfigQuestionnaire where
  fromBSON doc = do
    _appConfigQuestionnaireQuestionnaireAccessibility <- BSON.lookup "questionnaireAccessibility" doc
    _appConfigQuestionnaireLevels <- BSON.lookup "levels" doc
    _appConfigQuestionnaireFeedback <- BSON.lookup "feedback" doc
    _appConfigQuestionnairePublicQuestionnaire <- BSON.lookup "publicQuestionnaire" doc
    return AppConfigQuestionnaire {..}

instance ToBSON AppConfigQuestionnaireFeedback where
  toBSON AppConfigQuestionnaireFeedback {..} =
    [ "enabled" BSON.=: _appConfigQuestionnaireFeedbackEnabled
    , "token" BSON.=: _appConfigQuestionnaireFeedbackToken
    , "owner" BSON.=: _appConfigQuestionnaireFeedbackOwner
    , "repo" BSON.=: _appConfigQuestionnaireFeedbackRepo
    ]

instance FromBSON AppConfigQuestionnaireFeedback where
  fromBSON doc = do
    _appConfigQuestionnaireFeedbackEnabled <- BSON.lookup "enabled" doc
    _appConfigQuestionnaireFeedbackToken <- BSON.lookup "token" doc
    _appConfigQuestionnaireFeedbackOwner <- BSON.lookup "owner" doc
    _appConfigQuestionnaireFeedbackRepo <- BSON.lookup "repo" doc
    return AppConfigQuestionnaireFeedback {..}
