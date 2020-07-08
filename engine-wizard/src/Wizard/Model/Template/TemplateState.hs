module Wizard.Model.Template.TemplateState where

import GHC.Generics

data TemplateState
  = UnknownTemplateState
  | OutdatedTemplateState
  | UpToDateTemplateState
  | UnpublishedTemplateState
  deriving (Show, Eq, Generic)
