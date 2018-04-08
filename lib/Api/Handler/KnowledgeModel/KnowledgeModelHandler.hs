module Api.Handler.KnowledgeModel.KnowledgeModelHandler where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text.Lazy
import Data.UUID
import qualified Web.Scotty as Scotty

import Api.Handler.Common
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Common.Context
import Model.Config.DSWConfig
import Service.KnowledgeModel.KnowledgeModelService

getKnowledgeModelA :: Context -> DSWConfig -> Scotty.ActionM ()
getKnowledgeModelA context dswConfig =
  checkPermission context "KM_PERM" $ do
    branchUuid <- Scotty.param "branchUuid"
    eitherDto <- liftIO $ getKnowledgeModelByBranchId context branchUuid
    case eitherDto of
      Right dto -> sendJson dto
      Left error -> sendError error
