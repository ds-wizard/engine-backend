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
  { _reportUuid = fromJust (U.fromString "921bcb7e-e15f-49e4-b176-dbbe2f573af0")
  , _reportChapterReports = [report1_ch2, report1_ch3, report1_ch1]
  , _reportCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _reportUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

report1_ch1 :: ChapterReport
report1_ch1 =
  ChapterReport
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

report1_ch1_full :: ChapterReport
report1_ch1_full =
  ChapterReport
  { _chapterReportChapterUuid = report1_ch1 ^. chapterUuid
  , _chapterReportIndications = report1_ch1 ^. indications
  , _chapterReportMetrics =
      [ MetricSummary {_metricSummaryMetricUuid = metricF ^. uuid, _metricSummaryMeasure = Nothing}
      , MetricSummary {_metricSummaryMetricUuid = metricA ^. uuid, _metricSummaryMeasure = Nothing}
      ] ++
      (report1_ch1 ^. metrics) ++
      [ MetricSummary {_metricSummaryMetricUuid = metricG ^. uuid, _metricSummaryMeasure = Nothing}
      , MetricSummary {_metricSummaryMetricUuid = metricO ^. uuid, _metricSummaryMeasure = Nothing}
      ]
  }

report1_ch2 :: ChapterReport
report1_ch2 =
  ChapterReport
  { _chapterReportChapterUuid = chapter2 ^. uuid
  , _chapterReportIndications =
      [ AnsweredIndication' $
        AnsweredIndication {_answeredIndicationAnsweredQuestions = 7, _answeredIndicationUnansweredQuestions = 1}
      ]
  , _chapterReportMetrics =
      [ MetricSummary {_metricSummaryMetricUuid = metricF ^. uuid, _metricSummaryMeasure = Just 1.0}
      , MetricSummary {_metricSummaryMetricUuid = metricA ^. uuid, _metricSummaryMeasure = Just 1.0}
      ]
  }

report1_ch2_full :: ChapterReport
report1_ch2_full =
  ChapterReport
  { _chapterReportChapterUuid = report1_ch2 ^. chapterUuid
  , _chapterReportIndications = report1_ch2 ^. indications
  , _chapterReportMetrics =
      (report1_ch2 ^. metrics) ++
      [ MetricSummary {_metricSummaryMetricUuid = metricI ^. uuid, _metricSummaryMeasure = Nothing}
      , MetricSummary {_metricSummaryMetricUuid = metricR ^. uuid, _metricSummaryMeasure = Nothing}
      , MetricSummary {_metricSummaryMetricUuid = metricG ^. uuid, _metricSummaryMeasure = Nothing}
      , MetricSummary {_metricSummaryMetricUuid = metricO ^. uuid, _metricSummaryMeasure = Nothing}
      ]
  }

report1_ch3 :: ChapterReport
report1_ch3 =
  ChapterReport
  { _chapterReportChapterUuid = chapter3 ^. uuid
  , _chapterReportIndications =
      [ AnsweredIndication' $
        AnsweredIndication {_answeredIndicationAnsweredQuestions = 2, _answeredIndicationUnansweredQuestions = 0}
      ]
  , _chapterReportMetrics = []
  }

report1_ch3_full :: ChapterReport
report1_ch3_full =
  ChapterReport
  { _chapterReportChapterUuid = report1_ch3 ^. chapterUuid
  , _chapterReportIndications = report1_ch3 ^. indications
  , _chapterReportMetrics =
      [ MetricSummary {_metricSummaryMetricUuid = metricF ^. uuid, _metricSummaryMeasure = Nothing}
      , MetricSummary {_metricSummaryMetricUuid = metricA ^. uuid, _metricSummaryMeasure = Nothing}
      , MetricSummary {_metricSummaryMetricUuid = metricI ^. uuid, _metricSummaryMeasure = Nothing}
      , MetricSummary {_metricSummaryMetricUuid = metricR ^. uuid, _metricSummaryMeasure = Nothing}
      , MetricSummary {_metricSummaryMetricUuid = metricG ^. uuid, _metricSummaryMeasure = Nothing}
      , MetricSummary {_metricSummaryMetricUuid = metricO ^. uuid, _metricSummaryMeasure = Nothing}
      ]
  }