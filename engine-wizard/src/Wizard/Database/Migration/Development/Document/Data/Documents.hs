module Wizard.Database.Migration.Development.Document.Data.Documents where

import Control.Lens ((^.))
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.Metric.Data.Metrics
import Wizard.Database.Migration.Development.Level.Data.Levels
import Wizard.Database.Migration.Development.Organization.Data.Organizations
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Document.DocumentContext
import Wizard.Service.Package.PackageMapper

dmp1 :: DocumentContext
dmp1 =
  DocumentContext
    { _documentContextUuid = fromJust (U.fromString "d87941ae-7725-4d22-b5c7-45dabc125199")
    , _documentContextConfig = DocumentContextConfig {_documentContextConfigLevelsEnabled = True}
    , _documentContextQuestionnaireUuid = U.toString $ questionnaire1 ^. uuid
    , _documentContextQuestionnaireName = questionnaire1 ^. name
    , _documentContextQuestionnaireReplies = questionnaire1 ^. replies
    , _documentContextLevel = questionnaire1 ^. level
    , _documentContextKnowledgeModel = km1WithQ4
    , _documentContextMetrics = [metricF, metricA, metricI, metricR, metricG, metricO]
    , _documentContextLevels = [level1, level2, level3]
    , _documentContextReport = report1
    , _documentContextPackage = toPackage germanyPackage
    , _documentContextOrganization = org1
    , _documentContextCreatedBy = Just userAlbert
    , _documentContextCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _documentContextUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }
