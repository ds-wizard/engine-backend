module Wizard.Model.Questionnaire.QuestionnaireVersion where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data QuestionnaireVersion =
  QuestionnaireVersion
    { _questionnaireVersionUuid :: U.UUID
    , _questionnaireVersionName :: String
    , _questionnaireVersionDescription :: Maybe String
    , _questionnaireVersionEventUuid :: U.UUID
    , _questionnaireVersionCreatedBy :: U.UUID
    , _questionnaireVersionCreatedAt :: UTCTime
    , _questionnaireVersionUpdatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq QuestionnaireVersion where
  a == b =
    _questionnaireVersionUuid a == _questionnaireVersionUuid b &&
    _questionnaireVersionName a == _questionnaireVersionName b &&
    _questionnaireVersionDescription a == _questionnaireVersionDescription b &&
    _questionnaireVersionEventUuid a == _questionnaireVersionEventUuid b &&
    _questionnaireVersionCreatedBy a == _questionnaireVersionCreatedBy b
