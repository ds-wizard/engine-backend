module Wizard.Model.Report.Report where

import Data.Time
import qualified Data.UUID as U

data Report =
  Report
    { _reportUuid :: U.UUID
    , _reportTotalReport :: TotalReport
    , _reportChapterReports :: [ChapterReport]
    , _reportCreatedAt :: UTCTime
    , _reportUpdatedAt :: UTCTime
    }
  deriving (Show)

instance Eq Report where
  a == b =
    _reportUuid a == _reportUuid b &&
    _reportTotalReport a == _reportTotalReport b && _reportChapterReports a == _reportChapterReports b

data TotalReport =
  TotalReport
    { _totalReportIndications :: [Indication]
    , _totalReportMetrics :: [MetricSummary]
    }
  deriving (Show, Eq)

data ChapterReport =
  ChapterReport
    { _chapterReportChapterUuid :: U.UUID
    , _chapterReportIndications :: [Indication]
    , _chapterReportMetrics :: [MetricSummary]
    }
  deriving (Show, Eq)

data Indication
  = AnsweredIndication' AnsweredIndication
  | LevelsAnsweredIndication' LevelsAnsweredIndication
  deriving (Show, Eq)

data AnsweredIndication =
  AnsweredIndication
    { _answeredIndicationAnsweredQuestions :: Int
    , _answeredIndicationUnansweredQuestions :: Int
    }
  deriving (Show, Eq)

data LevelsAnsweredIndication =
  LevelsAnsweredIndication
    { _levelsAnsweredIndicationAnsweredQuestions :: Int
    , _levelsAnsweredIndicationUnansweredQuestions :: Int
    }
  deriving (Show, Eq)

data MetricSummary =
  MetricSummary
    { _metricSummaryMetricUuid :: U.UUID
    , _metricSummaryMeasure :: Maybe Double
    }
  deriving (Show, Eq)
