module Wizard.Service.KnowledgeModel.Secret.KnowledgeModelSecretService where

import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Uuid
import Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.KnowledgeModelSecret
import Wizard.Service.KnowledgeModel.Secret.KnowledgeModelSecretMapper

getKnowledgeModelSecrets :: AppContextM [KnowledgeModelSecret]
getKnowledgeModelSecrets = do
  checkPermission _KM_PERM
  findKnowledgeModelSecrets

createKnowledgeModelSecret :: KnowledgeModelSecretChangeDTO -> AppContextM KnowledgeModelSecret
createKnowledgeModelSecret reqDto =
  runInTransaction $ do
    checkPermission _KM_PERM
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    tenantUuid <- asks currentTenantUuid
    let kmSecret = fromCreateDTO reqDto uuid tenantUuid now
    insertKnowledgeModelSecret kmSecret
    return kmSecret

modifyKnowledgeModelSecret :: U.UUID -> KnowledgeModelSecretChangeDTO -> AppContextM KnowledgeModelSecret
modifyKnowledgeModelSecret uuid reqDto =
  runInTransaction $ do
    checkPermission _KM_PERM
    kmSecret <- findKnowledgeModelSecretByUuid uuid
    now <- liftIO getCurrentTime
    tenantUuid <- asks currentTenantUuid
    let kmSecretUpdated = fromChangeDTO kmSecret reqDto now
    updateKnowledgeModelSecretByUuid kmSecretUpdated
    return kmSecretUpdated

deleteKnowledgeModelSecret :: U.UUID -> AppContextM ()
deleteKnowledgeModelSecret uuid = do
  checkPermission _KM_PERM
  _ <- findKnowledgeModelSecretByUuid uuid
  void $ deleteKnowledgeModelSecretByUuid uuid
