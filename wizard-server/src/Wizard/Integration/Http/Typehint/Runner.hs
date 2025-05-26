module Wizard.Integration.Http.Typehint.Runner (
  retrieveTypehints,
) where

import Data.Map.Strict as M

import Shared.Common.Integration.Http.Common.HttpClient
import Wizard.Integration.Http.Typehint.RequestMapper
import Wizard.Integration.Http.Typehint.ResponseMapper
import Wizard.Integration.Resource.Typehint.TypehintIDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

retrieveTypehints :: ApiIntegration -> M.Map String String -> M.Map String String -> AppContextM (Either String [TypehintIDTO])
retrieveTypehints integrationConfig encodedVariables variables =
  runRequest' (toRetrieveTypehintsRequest integrationConfig encodedVariables variables) (toRetrieveTypehintsResponse integrationConfig)
