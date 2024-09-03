module Wizard.Database.Migration.Development.Report.Data.Reports where

import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Model.Report.Report
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Chapters
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Metrics
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

report1 :: Report
report1 =
  Report
    { uuid = fromJust (U.fromString "921bcb7e-e15f-49e4-b176-dbbe2f573af0")
    , totalReport = report1_total
    , chapterReports = [report1_ch1, report1_ch2, report1_ch3]
    , chapters = [chapter1, chapter2, chapter3]
    , metrics = [metricF, metricA, metricI, metricR]
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

report1_total :: TotalReport
report1_total =
  TotalReport
    { indications =
        [ PhasesAnsweredIndication' $
            PhasesAnsweredIndication
              { answeredQuestions = 3
              , unansweredQuestions = 1
              }
        , AnsweredIndication' $
            AnsweredIndication {answeredQuestions = 13, unansweredQuestions = 3}
        ]
    , metrics =
        [ MetricSummary {metricUuid = metricF.uuid, measure = Just 1.0}
        , MetricSummary {metricUuid = metricA.uuid, measure = Just 1.0}
        , MetricSummary {metricUuid = metricI.uuid, measure = Just 1.0}
        , MetricSummary {metricUuid = metricR.uuid, measure = Just 1.0}
        ]
    }

report1_total_full :: TotalReport
report1_total_full =
  TotalReport
    { indications =
        [ PhasesAnsweredIndication' $
            PhasesAnsweredIndication
              { answeredQuestions = 3
              , unansweredQuestions = 1
              }
        , AnsweredIndication' $
            AnsweredIndication {answeredQuestions = 13, unansweredQuestions = 3}
        ]
    , metrics =
        [ MetricSummary {metricUuid = metricF.uuid, measure = Just 1.0}
        , MetricSummary {metricUuid = metricA.uuid, measure = Just 1.0}
        , MetricSummary {metricUuid = metricI.uuid, measure = Just 1.0}
        , MetricSummary {metricUuid = metricR.uuid, measure = Just 1.0}
        ]
    }

report1_ch1 :: ChapterReport
report1_ch1 =
  ChapterReport
    { chapterUuid = chapter1.uuid
    , indications =
        [ PhasesAnsweredIndication' $
            PhasesAnsweredIndication
              { answeredQuestions = 1
              , unansweredQuestions = 0
              }
        , AnsweredIndication' $
            AnsweredIndication {answeredQuestions = 3, unansweredQuestions = 0}
        ]
    , metrics =
        [ MetricSummary {metricUuid = metricI.uuid, measure = Just 1.0}
        , MetricSummary {metricUuid = metricR.uuid, measure = Just 1.0}
        ]
    }

report1_ch1_full :: ChapterReport
report1_ch1_full =
  ChapterReport
    { chapterUuid = report1_ch1.chapterUuid
    , indications = report1_ch1.indications
    , metrics = report1_ch1.metrics
    }

report1_ch2 :: ChapterReport
report1_ch2 =
  ChapterReport
    { chapterUuid = chapter2.uuid
    , indications =
        [ PhasesAnsweredIndication' $
            PhasesAnsweredIndication
              { answeredQuestions = 1
              , unansweredQuestions = 1
              }
        , AnsweredIndication' $
            AnsweredIndication {answeredQuestions = 7, unansweredQuestions = 1}
        ]
    , metrics =
        [ MetricSummary {metricUuid = metricF.uuid, measure = Just 1.0}
        , MetricSummary {metricUuid = metricA.uuid, measure = Just 1.0}
        ]
    }

report1_ch2_full :: ChapterReport
report1_ch2_full =
  ChapterReport
    { chapterUuid = report1_ch2.chapterUuid
    , indications = report1_ch2.indications
    , metrics = report1_ch2.metrics
    }

report1_ch3 :: ChapterReport
report1_ch3 =
  ChapterReport
    { chapterUuid = chapter3.uuid
    , indications =
        [ PhasesAnsweredIndication' $
            PhasesAnsweredIndication
              { answeredQuestions = 1
              , unansweredQuestions = 0
              }
        , AnsweredIndication' $
            AnsweredIndication {answeredQuestions = 3, unansweredQuestions = 2}
        ]
    , metrics = []
    }

report1_ch3_full :: ChapterReport
report1_ch3_full =
  ChapterReport
    { chapterUuid = report1_ch3.chapterUuid
    , indications = report1_ch3.indications
    , metrics = []
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaireReport :: QuestionnaireReportDTO
questionnaireReport =
  QuestionnaireReportDTO
    { indications =
        [ PhasesAnsweredIndication' $
            PhasesAnsweredIndication
              { answeredQuestions = 3
              , unansweredQuestions = 1
              }
        , AnsweredIndication' $
            AnsweredIndication {answeredQuestions = 13, unansweredQuestions = 3}
        ]
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
samplePhasesAnsweredIndication :: PhasesAnsweredIndication
samplePhasesAnsweredIndication =
  PhasesAnsweredIndication
    { answeredQuestions = 3
    , unansweredQuestions = 1
    }
