module Database.Migration.Development.PublicQuestionnaire.Data.PublicQuestionnaires where

import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Model.Questionnaire.Questionnaire

publicQuestionnaire =
  Questionnaire
  { _questionnaireUuid = fromJust (U.fromString "8a016763-ae43-4a85-afa6-fda5067c5357")
  , _questionnaireName = "Public Questionnaire"
  , _questionnaireLevel = 2
  , _questionnairePackageId = "elixir.nl:core-nl:2.0.0"
  , _questionnaireKnowledgeModel = km1WithQ4
  , _questionnaireReplies = []
  , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }
