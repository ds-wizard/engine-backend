module Api.Resource.Report.ReportDTO where

import Data.Time
import qualified Data.UUID as U

data IndicationDTO =
  AnsweredIndicationDTO' AnsweredIndicationDTO
  deriving (Show, Eq)

data AnsweredIndicationDTO = AnsweredIndicationDTO
  { _answeredIndicationDTOAnsweredQuestions :: Int
  , _answeredIndicationDTOUnansweredQuestions :: Int
  } deriving (Show, Eq)

data MetricSummaryDTO = MetricSummaryDTO
  { _metricSummaryDTOMetricUuid :: U.UUID
  , _metricSummaryDTOMeasure :: Double
  } deriving (Show, Eq)

data ChapterReportDTO = ChapterReportDTO
  { _chapterReportDTOChapterUuid :: U.UUID
  , _chapterReportDTOIndications :: [IndicationDTO]
  , _chapterReportDTOMetrics :: [MetricSummaryDTO]
  } deriving (Show, Eq)

data ReportDTO = ReportDTO
  { _reportDTOUuid :: U.UUID
  , _reportDTOChapterReports :: [ChapterReportDTO]
  , _reportDTOCreatedAt :: UTCTime
  , _reportDTOUpdatedAt :: UTCTime
  } deriving (Show)

instance Eq ReportDTO where
  a == b = _reportDTOUuid a == _reportDTOUuid b && _reportDTOChapterReports a == _reportDTOChapterReports b
