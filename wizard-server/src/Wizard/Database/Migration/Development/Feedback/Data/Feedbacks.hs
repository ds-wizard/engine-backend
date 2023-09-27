module Wizard.Database.Migration.Development.Feedback.Data.Feedbacks where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Feedback.FeedbackCreateDTO
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Feedback.Feedback
import Wizard.Model.Tenant.Tenant
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

feedback1 :: Feedback
feedback1 =
  Feedback
    { uuid = u' "c44c06d1-ad9f-4f73-9c05-2aa9eddacae1"
    , issueId = 1
    , questionUuid = question1.uuid
    , packageId = germanyPackage.pId
    , title = "Provide more descriptive content"
    , content = "I'm not very satisfied with a description of this question"
    , tenantUuid = defaultTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

feedback1Create :: FeedbackCreateDTO
feedback1Create =
  FeedbackCreateDTO
    { questionUuid = feedback1.questionUuid
    , packageId = feedback1.packageId
    , title = feedback1.title
    , content = feedback1.content
    }

feedback2 :: Feedback
feedback2 =
  Feedback
    { uuid = u' "22e24917-7443-40f7-a3f2-4ea9f69ceebb"
    , issueId = 99999
    , questionUuid = question1.uuid
    , packageId = germanyPackage.pId
    , title = "Non-existing issue"
    , content = "There is no issue like that"
    , tenantUuid = defaultTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

differentFeedback :: Feedback
differentFeedback =
  Feedback
    { uuid = u' "e1a4a72c-1fca-4d8c-95d5-446bea876959"
    , issueId = 99998
    , questionUuid = question1.uuid
    , packageId = differentPackage.pId
    , title = "Non-existing issue"
    , content = "There is no issue like that"
    , tenantUuid = differentTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
