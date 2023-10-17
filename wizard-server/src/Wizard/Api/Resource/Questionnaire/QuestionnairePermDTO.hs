module Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Acl.MemberDTO

data QuestionnairePermDTO = QuestionnairePermDTO
  { questionnaireUuid :: U.UUID
  , member :: MemberDTO
  , perms :: [String]
  }
  deriving (Generic, Eq, Show)

instance Ord QuestionnairePermDTO where
  compare a b = compare a.member.uuid b.member.uuid
