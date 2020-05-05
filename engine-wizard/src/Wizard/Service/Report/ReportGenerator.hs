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

computeChapterReport :: Bool -> Int -> [Metric] -> KnowledgeModel -> [Reply] -> Chapter -> ChapterReport
computeChapterReport levelsEnabled requiredLevel metrics km replies ch =
  ChapterReport
    { _uuid = ch ^. uuid
    , _indications = computeIndications levelsEnabled requiredLevel km replies ch
    , _metrics = computeMetrics metrics km replies (Just ch)
    }

computeTotalReport :: Bool -> Int -> [Metric] -> KnowledgeModel -> [Reply] -> TotalReport
computeTotalReport levelsEnabled requiredLevel metrics km replies =
  let chapterIndications = fmap (computeIndications levelsEnabled requiredLevel km replies) (getChaptersForKmUuid km)
      mergeIndications [LevelsAnsweredIndication a1 b1, AnsweredIndication c1 d1] [LevelsAnsweredIndication a2 b2, AnsweredIndication c2 d2] =
        [LevelsAnsweredIndication (a1 + a2) (b1 + b2), AnsweredIndication (c1 + c2) (d1 + d2)]
      mergeIndications [AnsweredIndication c1 d1] [AnsweredIndication c2 d2] = [AnsweredIndication (c1 + c2) (d1 + d2)]
   in TotalReport
        { _indications =
            if levelsEnabled
              then foldl mergeIndications [LevelsAnsweredIndication 0 0, AnsweredIndication 0 0] chapterIndications
              else foldl mergeIndications [AnsweredIndication 0 0] chapterIndications
        , _metrics = computeMetrics metrics km replies Nothing
        }

generateReport :: Int -> [Metric] -> KnowledgeModel -> [Reply] -> AppContextM Report
generateReport requiredLevel metrics km replies = do
  rUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  appConfig <- getAppConfig
  let _levelsEnabled = appConfig ^. questionnaire . levels . enabled
  return
    Report
      { _uuid = rUuid
      , _totalReport = computeTotalReport _levelsEnabled requiredLevel metrics km replies
      , _reports =
          computeChapterReport _levelsEnabled requiredLevel metrics km replies <$> getChaptersForKmUuid km
      , _createdAt = now
      , _updatedAt = now
      }
