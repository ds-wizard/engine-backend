module Wizard.Service.KnowledgeModel.KnowledgeModelValidation where

import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import WizardLib.KnowledgeModel.Model.Event.Event

validateKmValidity :: [Event] -> Maybe String -> AppContextM ()
validateKmValidity events mPackageId = do
  _ <- compileKnowledgeModel events mPackageId []
  return ()
