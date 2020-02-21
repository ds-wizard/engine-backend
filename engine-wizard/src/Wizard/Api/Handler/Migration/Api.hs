module Wizard.Api.Handler.Migration.Api where

import Servant

import qualified Wizard.Api.Handler.Migration.KnowledgeModel.Api as KM
import qualified Wizard.Api.Handler.Migration.Questionnaire.Api as QTN
import Wizard.Model.Context.BaseContext

type MigrationAPI
   = KM.MigrationAPI
     :<|> QTN.MigrationAPI

migrationApi :: Proxy MigrationAPI
migrationApi = Proxy

migrationServer :: ServerT MigrationAPI BaseContextM
migrationServer = KM.migrationServer :<|> QTN.migrationServer
