module Wizard.Model.Document.DocumentContextJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Api.Resource.Project.Comment.ProjectCommentThreadListJM ()
import Wizard.Api.Resource.Project.File.ProjectFileSimpleJM ()
import Wizard.Api.Resource.Project.ProjectReplyJM ()
import Wizard.Api.Resource.Project.Version.ProjectVersionListJM ()
import Wizard.Api.Resource.Report.ReportJM ()
import Wizard.Api.Resource.Tenant.Config.TenantConfigJM ()
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Model.Document.DocumentContext
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
