module Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Acl.MemberDTO

data QuestionnairePermRecordDTO = QuestionnairePermRecordDTO
  { uuid :: U.UUID
  , questionnaireUuid :: U.UUID
  , member :: MemberDTO
  , perms :: [String]
  }
  deriving (Generic, Eq, Show)

instance Ord QuestionnairePermRecordDTO where
  compare a b = compare a.uuid b.uuid
