module Wizard.Model.Common.Lens where

import qualified Data.UUID as U

class HasQuestionnaireUuid' entity where
  getQuestionnaireUuid :: entity -> U.UUID
  setQuestionnaireUuid :: entity -> U.UUID -> entity
