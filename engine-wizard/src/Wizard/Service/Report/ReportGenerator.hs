module Wizard.Service.Report.ReportGenerator where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelAccessors
import Shared.Util.Uuid
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Report.Report
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Report.Evaluator.Indication
import Wizard.Service.Report.Evaluator.Metric

computeChapterReport :: Bool -> U.UUID -> KnowledgeModel -> [ReplyTuple] -> Chapter -> ChapterReport
computeChapterReport phasesEnabled requiredPhaseUuid km replies ch =
  ChapterReport
    { _chapterReportChapterUuid = ch ^. uuid
    , _chapterReportIndications = computeIndications phasesEnabled requiredPhaseUuid km replies ch
    , _chapterReportMetrics = computeMetrics km replies (Just ch)
    }

computeTotalReport :: Bool -> U.UUID -> KnowledgeModel -> [ReplyTuple] -> TotalReport
computeTotalReport phasesEnabled requiredPhaseUuid km replies =
  TotalReport
    { _totalReportIndications = computeTotalReportIndications phasesEnabled requiredPhaseUuid km replies
    , _totalReportMetrics = computeMetrics km replies Nothing
    }

computeTotalReportIndications :: Bool -> U.UUID -> KnowledgeModel -> [ReplyTuple] -> [Indication]
computeTotalReportIndications phasesEnabled requiredPhaseUuid km replies =
  let chapterIndications =
        fmap (computeIndications phasesEnabled requiredPhaseUuid km replies) (getChaptersForKmUuid km)
      mergeIndications [LevelsAnsweredIndication' (LevelsAnsweredIndication a1 b1), AnsweredIndication' (AnsweredIndication c1 d1)] [LevelsAnsweredIndication' (LevelsAnsweredIndication a2 b2), AnsweredIndication' (AnsweredIndication c2 d2)] =
        [ LevelsAnsweredIndication' (LevelsAnsweredIndication (a1 + a2) (b1 + b2))
        , AnsweredIndication' (AnsweredIndication (c1 + c2) (d1 + d2))
        ]
      mergeIndications [AnsweredIndication' (AnsweredIndication c1 d1)] [AnsweredIndication' (AnsweredIndication c2 d2)] =
        [AnsweredIndication' (AnsweredIndication (c1 + c2) (d1 + d2))]
   in if phasesEnabled
        then foldl
               mergeIndications
               [LevelsAnsweredIndication' (LevelsAnsweredIndication 0 0), AnsweredIndication' (AnsweredIndication 0 0)]
               chapterIndications
        else foldl mergeIndications [AnsweredIndication' (AnsweredIndication 0 0)] chapterIndications

generateReport :: U.UUID -> KnowledgeModel -> [ReplyTuple] -> AppContextM Report
generateReport requiredPhaseUuid km replies = do
  rUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  appConfig <- getAppConfig
  let _phasesEnabled = appConfig ^. questionnaire . phases . enabled
  return
    Report
      { _reportUuid = rUuid
      , _reportTotalReport = computeTotalReport _phasesEnabled requiredPhaseUuid km replies
      , _reportChapterReports =
          computeChapterReport _phasesEnabled requiredPhaseUuid km replies <$> getChaptersForKmUuid km
      , _reportCreatedAt = now
      , _reportUpdatedAt = now
      }
