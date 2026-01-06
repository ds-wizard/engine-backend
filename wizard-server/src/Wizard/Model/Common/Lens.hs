module Wizard.Model.Common.Lens where

import qualified Data.UUID as U

class HasProjectUuid' entity where
  getProjectUuid :: entity -> U.UUID
  setProjectUuid :: entity -> U.UUID -> entity
