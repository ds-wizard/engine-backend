module Wizard.Service.Report.ReportGenerator where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

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

computeChapterReport :: Bool -> Int -> [Metric] -> KnowledgeModel -> [ReplyTuple] -> Chapter -> ChapterReport
computeChapterReport levelsEnabled requiredLevel metrics km replies ch =
  ChapterReport
    { _chapterReportChapterUuid = ch ^. uuid
    , _chapterReportIndications = computeIndications levelsEnabled requiredLevel km replies ch
    , _chapterReportMetrics = computeMetrics metrics km replies (Just ch)
    }

computeTotalReport :: Bool -> Int -> [Metric] -> KnowledgeModel -> [ReplyTuple] -> TotalReport
computeTotalReport levelsEnabled requiredLevel metrics km replies =
  TotalReport
    { _totalReportIndications = computeTotalReportIndications levelsEnabled requiredLevel km replies
    , _totalReportMetrics = computeMetrics metrics km replies Nothing
    }

computeTotalReportIndications :: Bool -> Int -> KnowledgeModel -> [ReplyTuple] -> [Indication]
computeTotalReportIndications levelsEnabled requiredLevel km replies =
  let chapterIndications = fmap (computeIndications levelsEnabled requiredLevel km replies) (getChaptersForKmUuid km)
      mergeIndications [LevelsAnsweredIndication' (LevelsAnsweredIndication a1 b1), AnsweredIndication' (AnsweredIndication c1 d1)] [LevelsAnsweredIndication' (LevelsAnsweredIndication a2 b2), AnsweredIndication' (AnsweredIndication c2 d2)] =
        [ LevelsAnsweredIndication' (LevelsAnsweredIndication (a1 + a2) (b1 + b2))
        , AnsweredIndication' (AnsweredIndication (c1 + c2) (d1 + d2))
        ]
      mergeIndications [AnsweredIndication' (AnsweredIndication c1 d1)] [AnsweredIndication' (AnsweredIndication c2 d2)] =
        [AnsweredIndication' (AnsweredIndication (c1 + c2) (d1 + d2))]
   in if levelsEnabled
        then foldl
               mergeIndications
               [LevelsAnsweredIndication' (LevelsAnsweredIndication 0 0), AnsweredIndication' (AnsweredIndication 0 0)]
               chapterIndications
        else foldl mergeIndications [AnsweredIndication' (AnsweredIndication 0 0)] chapterIndications

generateReport :: Int -> [Metric] -> KnowledgeModel -> [ReplyTuple] -> AppContextM Report
generateReport requiredLevel metrics km replies = do
  rUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  appConfig <- getAppConfig
  let _levelsEnabled = appConfig ^. questionnaire . levels . enabled
  return
    Report
      { _reportUuid = rUuid
      , _reportTotalReport = computeTotalReport _levelsEnabled requiredLevel metrics km replies
      , _reportChapterReports =
          computeChapterReport _levelsEnabled requiredLevel metrics km replies <$> getChaptersForKmUuid km
      , _reportCreatedAt = now
      , _reportUpdatedAt = now
      }
