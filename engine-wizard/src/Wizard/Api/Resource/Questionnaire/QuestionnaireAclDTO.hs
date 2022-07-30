module Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Acl.MemberDTO

data QuestionnairePermRecordDTO =
  QuestionnairePermRecordDTO
    { _questionnairePermRecordDTOUuid :: U.UUID
    , _questionnairePermRecordDTOQuestionnaireUuid :: U.UUID
    , _questionnairePermRecordDTOMember :: MemberDTO
    , _questionnairePermRecordDTOPerms :: [String]
    }
  deriving (Generic, Eq, Show)

instance Ord QuestionnairePermRecordDTO where
  compare a b = compare (_questionnairePermRecordDTOUuid a) (_questionnairePermRecordDTOUuid b)
