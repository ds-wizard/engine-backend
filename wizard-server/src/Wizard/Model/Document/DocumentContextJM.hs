module Wizard.Model.Document.DocumentContextJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.File.QuestionnaireFileSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadListJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionListJM ()
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Api.Resource.Tenant.Config.TenantConfigJM ()
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Model.Document.DocumentContext
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import WizardLib.Public.Api.Resource.User.Group.UserGroupDetailJM ()

instance ToJSON DocumentContext where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextConfig where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextPackage where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextQuestionnaire where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextDocument where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextUserPerm where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentContextUserGroupPerm where
  toJSON = genericToJSON jsonOptions
