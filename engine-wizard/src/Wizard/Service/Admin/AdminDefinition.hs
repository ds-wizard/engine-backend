module Wizard.Service.Admin.AdminDefinition where

import Control.Lens ((^.))

import LensesConfig hiding (action, cache, feedback)
import Wizard.Api.Resource.Admin.AdminExecutionDTO
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Model.Admin.Admin
import Wizard.Model.Context.AppContext
import Wizard.Service.App.AppService
import Wizard.Service.Branch.Event.BranchEventService
import Wizard.Service.Cache.CacheService
import qualified Wizard.Service.Cache.KnowledgeModelCache as KnowledgeModelCache
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Feedback.FeedbackService

-- ---------------------------------------------------------------------------------------------------------------------
-- APP
-- ---------------------------------------------------------------------------------------------------------------------
app :: AdminSection
app =
  AdminSection
    {_adminSectionName = "App", _adminSectionDescription = Nothing, _adminSectionOperations = [app_createApp]}

-- ---------------------------------------------------------------------------------------------------------------------
app_createApp :: AdminOperation
app_createApp =
  AdminOperation
    { _adminOperationName = "Create an application"
    , _adminOperationDescription = Nothing
    , _adminOperationParameters =
        [ AdminOperationParameter
            {_adminOperationParameterName = "appId", _adminOperationParameterAType = StringAdminOperationParameterType}
        , AdminOperationParameter
            {_adminOperationParameterName = "name", _adminOperationParameterAType = StringAdminOperationParameterType}
        , AdminOperationParameter
            { _adminOperationParameterName = "serverDomain"
            , _adminOperationParameterAType = StringAdminOperationParameterType
            }
        , AdminOperationParameter
            { _adminOperationParameterName = "serverUrl"
            , _adminOperationParameterAType = StringAdminOperationParameterType
            }
        , AdminOperationParameter
            { _adminOperationParameterName = "clientUrl"
            , _adminOperationParameterAType = StringAdminOperationParameterType
            }
        ]
    }

app_createAppFn :: AdminExecutionDTO -> AppContextM String
app_createAppFn reqDto = do
  let reqDto' =
        AppCreateDTO
          { _appCreateDTOAppId = head (reqDto ^. parameters)
          , _appCreateDTOName = (reqDto ^. parameters) !! 1
          , _appCreateDTOServerDomain = (reqDto ^. parameters) !! 2
          , _appCreateDTOServerUrl = (reqDto ^. parameters) !! 3
          , _appCreateDTOClientUrl = (reqDto ^. parameters) !! 4
          }
  createApp reqDto'
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- BRANCH
-- ---------------------------------------------------------------------------------------------------------------------
branch :: AdminSection
branch =
  AdminSection
    { _adminSectionName = "Branch"
    , _adminSectionDescription = Nothing
    , _adminSectionOperations = [branch_squashAllEvents, branch_squashEventsForBranch]
    }

-- ---------------------------------------------------------------------------------------------------------------------
branch_squashAllEvents :: AdminOperation
branch_squashAllEvents =
  AdminOperation
    {_adminOperationName = "Squash All Events", _adminOperationDescription = Nothing, _adminOperationParameters = []}

branch_squashAllEventsFn :: AdminExecutionDTO -> AppContextM String
branch_squashAllEventsFn reqDto = do
  squashEvents
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
branch_squashEventsForBranch :: AdminOperation
branch_squashEventsForBranch =
  AdminOperation
    { _adminOperationName = "Squash Events for Branch"
    , _adminOperationDescription = Nothing
    , _adminOperationParameters =
        [ AdminOperationParameter
            { _adminOperationParameterName = "branchUuid"
            , _adminOperationParameterAType = StringAdminOperationParameterType
            }
        ]
    }

branch_squashEventsForBranchFn :: AdminExecutionDTO -> AppContextM String
branch_squashEventsForBranchFn reqDto = do
  squashEventsForBranch (head (reqDto ^. parameters))
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- CACHE
-- ---------------------------------------------------------------------------------------------------------------------
cache :: AdminSection
cache =
  AdminSection
    { _adminSectionName = "Cache"
    , _adminSectionDescription = Nothing
    , _adminSectionOperations = [cache_purgeCache, cache_KnowledgeModelCache_deleteFromCache']
    }

-- ---------------------------------------------------------------------------------------------------------------------
cache_purgeCache :: AdminOperation
cache_purgeCache =
  AdminOperation
    {_adminOperationName = "Purge All Caches", _adminOperationDescription = Nothing, _adminOperationParameters = []}

cache_purgeCacheFn :: AdminExecutionDTO -> AppContextM String
cache_purgeCacheFn reqDto = do
  purgeCache
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
cache_KnowledgeModelCache_deleteFromCache' :: AdminOperation
cache_KnowledgeModelCache_deleteFromCache' =
  AdminOperation
    { _adminOperationName = "Purge Knowledge Model Cache"
    , _adminOperationDescription = Nothing
    , _adminOperationParameters =
        [ AdminOperationParameter
            {_adminOperationParameterName = "pkgId", _adminOperationParameterAType = StringAdminOperationParameterType}
        ]
    }

cache_KnowledgeModelCache_deleteFromCacheFn' :: AdminExecutionDTO -> AppContextM String
cache_KnowledgeModelCache_deleteFromCacheFn' reqDto = do
  KnowledgeModelCache.deleteFromCache' (head (reqDto ^. parameters))
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- CONFIG
-- ---------------------------------------------------------------------------------------------------------------------
config :: AdminSection
config =
  AdminSection
    { _adminSectionName = "Config"
    , _adminSectionDescription = Nothing
    , _adminSectionOperations = [config_switchClientCustomizationOn, config_switchClientCustomizationOff]
    }

-- ---------------------------------------------------------------------------------------------------------------------
config_switchClientCustomizationOn :: AdminOperation
config_switchClientCustomizationOn =
  AdminOperation
    { _adminOperationName = "Enable Client Customization in Settings"
    , _adminOperationDescription = Nothing
    , _adminOperationParameters = []
    }

config_switchClientCustomizationOnFn :: AdminExecutionDTO -> AppContextM String
config_switchClientCustomizationOnFn reqDto = do
  modifyClientCustomization True
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
config_switchClientCustomizationOff :: AdminOperation
config_switchClientCustomizationOff =
  AdminOperation
    { _adminOperationName = "Disable Client Customization in Settings"
    , _adminOperationDescription = Nothing
    , _adminOperationParameters = []
    }

config_switchClientCustomizationOffFn :: AdminExecutionDTO -> AppContextM String
config_switchClientCustomizationOffFn reqDto = do
  modifyClientCustomization False
  return "Done"

-- ---------------------------------------------------------------------------------------------------------------------
-- FEEDBACK
-- ---------------------------------------------------------------------------------------------------------------------
feedback :: AdminSection
feedback =
  AdminSection
    { _adminSectionName = "Feedback"
    , _adminSectionDescription = Nothing
    , _adminSectionOperations = [feedback_synchronizeFeedbacks]
    }

-- ---------------------------------------------------------------------------------------------------------------------
feedback_synchronizeFeedbacks :: AdminOperation
feedback_synchronizeFeedbacks =
  AdminOperation
    { _adminOperationName = "Synchronize Feedbacks"
    , _adminOperationDescription = Nothing
    , _adminOperationParameters = []
    }

feedback_synchronizeFeedbacksFn :: AdminExecutionDTO -> AppContextM String
feedback_synchronizeFeedbacksFn reqDto = do
  synchronizeFeedbacks
  return "Done"
