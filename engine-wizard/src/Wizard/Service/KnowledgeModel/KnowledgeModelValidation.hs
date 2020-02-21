module Wizard.Service.KnowledgeModel.KnowledgeModelValidation where

import Shared.Model.Event.Event
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.KnowledgeModelService

validateKmValidity :: [Event] -> Maybe String -> AppContextM ()
validateKmValidity events mPackageId = do
  _ <- compileKnowledgeModel events mPackageId []
  return ()
