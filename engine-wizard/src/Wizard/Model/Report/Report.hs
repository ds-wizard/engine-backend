module Wizard.Model.Report.Report where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Report = Report
  { uuid :: U.UUID
  , totalReport :: TotalReport
  , chapterReports :: [ChapterReport]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq Report where
  a == b =
    a.uuid == b.uuid
      && a.totalReport == b.totalReport
      && a.chapterReports == b.chapterReports

data TotalReport = TotalReport
  { indications :: [Indication]
  , metrics :: [MetricSummary]
  }
  deriving (Show, Eq, Generic)

data ChapterReport = ChapterReport
  { chapterUuid :: U.UUID
  , indications :: [Indication]
  , metrics :: [MetricSummary]
  }
  deriving (Show, Eq, Generic)

data Indication
  = AnsweredIndication' AnsweredIndication
  | PhasesAnsweredIndication' PhasesAnsweredIndication
  deriving (Show, Eq, Generic)

data AnsweredIndication = AnsweredIndication
  { answeredQuestions :: Int
  , unansweredQuestions :: Int
  }
  deriving (Show, Eq, Generic)

data PhasesAnsweredIndication = PhasesAnsweredIndication
  { answeredQuestions :: Int
  , unansweredQuestions :: Int
  }
  deriving (Show, Eq, Generic)

data MetricSummary = MetricSummary
  { metricUuid :: U.UUID
  , measure :: Maybe Double
  }
  deriving (Show, Eq, Generic)
