module Database.Migration.Development.Feedback.Data.Feedbacks where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Database.Migration.Development.KnowledgeModel.Data.Questions
import Database.Migration.Development.Package.Data.Packages
import LensesConfig
import Model.Feedback.Feedback

feedback1 =
  Feedback
  { _feedbackUuid = fromJust . U.fromString $ "c44c06d1-ad9f-4f73-9c05-2aa9eddacae1"
  , _feedbackIssueId = 1
  , _feedbackQuestionUuid = question1 ^. uuid
  , _feedbackPackageId = elixirCzPackage2Dto ^. pId
  , _feedbackTitle = "Provide more descriptive content"
  , _feedbackContent = "I'm not very satisfied with a description of this question"
  , _feedbackCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _feedbackUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  }

feedback2 =
  Feedback
  { _feedbackUuid = fromJust . U.fromString $ "22e24917-7443-40f7-a3f2-4ea9f69ceebb"
  , _feedbackIssueId = 99999
  , _feedbackQuestionUuid = question1 ^. uuid
  , _feedbackPackageId = elixirCzPackage2Dto ^. pId
  , _feedbackTitle = "Non-existing issue"
  , _feedbackContent = "There is no issue like that"
  , _feedbackCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _feedbackUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  }
