module Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO where

import GHC.Generics

import Wizard.Api.Resource.Acl.MemberDTO

data QuestionnairePermRecordDTO =
  QuestionnairePermRecordDTO
    { _questionnairePermRecordDTOMember :: MemberDTO
    , _questionnairePermRecordDTOPerms :: [String]
    }
  deriving (Generic, Eq, Show)
