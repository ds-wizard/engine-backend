module Wizard.Database.Migration.Development.QuestionnaireAction.Data.QuestionnaireActions where

import qualified Data.Aeson.KeyMap as KM

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionDTO
import Wizard.Model.QuestionnaireAction.QuestionnaireAction
import Wizard.Service.QuestionnaireAction.QuestionnaireActionMapper
import WizardLib.Common.Constant.QuestionnaireAction
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

questionnaireActionFtp1 :: QuestionnaireAction
questionnaireActionFtp1 =
  QuestionnaireAction
    { qaId = "global:questionnaire-action-ftp:1.0.0"
    , name = "Questionnaire Action FTP"
    , organizationId = "global"
    , actionId = "questionnaire-action-ftp"
    , version = "1.0.0"
    , metamodelVersion = questionnaireActionMetamodelVersion
    , description = "Uploading questionnaire to FTP"
    , readme = "# Questionnaire Action FTP"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternAll]
    , url = "http://example.com/questionnaire-action-ftp"
    , config = KM.empty
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

questionnaireActionFtp2 :: QuestionnaireAction
questionnaireActionFtp2 =
  QuestionnaireAction
    { qaId = "global:questionnaire-action-ftp:2.0.0"
    , name = "Questionnaire Action FTP"
    , organizationId = "global"
    , actionId = "questionnaire-action-ftp"
    , version = "2.0.0"
    , metamodelVersion = questionnaireActionMetamodelVersion
    , description = "Uploading questionnaire to FTP"
    , readme = "# Questionnaire Action FTP"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternAll]
    , url = "http://example.com/questionnaire-action-ftp"
    , config = KM.empty
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

questionnaireActionFtp3 :: QuestionnaireAction
questionnaireActionFtp3 =
  QuestionnaireAction
    { qaId = "global:questionnaire-action-ftp:3.0.0"
    , name = "Questionnaire Action FTP"
    , organizationId = "global"
    , actionId = "questionnaire-action-ftp"
    , version = "3.0.0"
    , metamodelVersion = questionnaireActionMetamodelVersion
    , description = "Uploading questionnaire to FTP"
    , readme = "# Questionnaire Action FTP"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternAll]
    , url = "http://example.com/questionnaire-action-ftp"
    , config = KM.empty
    , enabled = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

questionnaireActionFtp3Edited :: QuestionnaireAction
questionnaireActionFtp3Edited = questionnaireActionFtp3 {enabled = True}

questionnaireActionFtp3Dto :: QuestionnaireActionDTO
questionnaireActionFtp3Dto = toDTO questionnaireActionFtp3

questionnaireActionMail1 :: QuestionnaireAction
questionnaireActionMail1 =
  QuestionnaireAction
    { qaId = "global:questionnaire-action-mail:1.0.0"
    , name = "Questionnaire Action Mail"
    , organizationId = "global"
    , actionId = "questionnaire-action-mail"
    , version = "1.0.0"
    , metamodelVersion = questionnaireActionMetamodelVersion
    , description = "Sending questionnaire via mail"
    , readme = "# Questionnaire Action Mail"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternGlobal]
    , url = "http://example.com/questionnaire-action-mail"
    , config = KM.empty
    , enabled = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

questionnaireActionScp1 :: QuestionnaireAction
questionnaireActionScp1 =
  QuestionnaireAction
    { qaId = "global:questionnaire-action-onto:1.0.0"
    , name = "Questionnaire Action SCP"
    , organizationId = "global"
    , actionId = "questionnaire-action-onto"
    , version = "1.0.0"
    , metamodelVersion = questionnaireActionMetamodelVersion
    , description = "Uploading questionnaire via SCP"
    , readme = "# Questionnaire Action SCP"
    , license = "Apache-2.0"
    , allowedPackages = [packagePatternGlobal]
    , url = "http://example.com/questionnaire-action-onto"
    , config = KM.empty
    , enabled = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
