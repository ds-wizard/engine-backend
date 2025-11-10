module Wizard.Service.KnowledgeModel.KnowledgeModelValidation where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.KnowledgeModelService

validateKmValidity :: [KnowledgeModelEvent] -> Maybe String -> AppContextM ()
validateKmValidity events mPackageId = do
  _ <- compileKnowledgeModel events mPackageId []
  return ()
