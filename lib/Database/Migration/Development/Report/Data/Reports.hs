module Database.Migration.Development.Report.Data.Reports where

import Control.Lens ((^.))
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import Database.Migration.Development.KnowledgeModel.Data.Chapters
import Database.Migration.Development.Metric.Data.Metrics
import LensesConfig
import Model.Report.Report

report1 :: Report
report1 =
  Report
  { _reportUuid = fromJust (U.fromString "a92637os-8200-2p99-w3l2-90shhj982829")
  , _reportChapterReports =
      [ ChapterReport
        { _chapterReportChapterUuid = chapter1 ^. uuid
        , _chapterReportIndications =
            [ AnsweredIndication' $
              AnsweredIndication {_answeredIndicationAnsweredQuestions = 3, _answeredIndicationUnansweredQuestions = 0}
            ]
        , _chapterReportMetrics =
            [ MetricSummary {_metricSummaryMetricUuid = metricI ^. uuid, _metricSummaryMeasure = Just 1.0}
            , MetricSummary {_metricSummaryMetricUuid = metricR ^. uuid, _metricSummaryMeasure = Just 1.0}
            ]
        }
      , ChapterReport
        { _chapterReportChapterUuid = chapter2 ^. uuid
        , _chapterReportIndications =
            [ AnsweredIndication' $
              AnsweredIndication {_answeredIndicationAnsweredQuestions = 10, _answeredIndicationUnansweredQuestions = 1}
            ]
        , _chapterReportMetrics =
            [ MetricSummary {_metricSummaryMetricUuid = metricF ^. uuid, _metricSummaryMeasure = Just 1.0}
            , MetricSummary {_metricSummaryMetricUuid = metricA ^. uuid, _metricSummaryMeasure = Just 1.0}
            ]
        }
      , ChapterReport
        { _chapterReportChapterUuid = chapter3 ^. uuid
        , _chapterReportIndications =
            [ AnsweredIndication' $
              AnsweredIndication {_answeredIndicationAnsweredQuestions = 2, _answeredIndicationUnansweredQuestions = 0}
            ]
        , _chapterReportMetrics = []
        }
      ]
  , _reportCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _reportUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }
