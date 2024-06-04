module WizardLib.Public.Database.DAO.PersistentCommand.PersistentCommandDAO where

import Data.String (fromString)
import Database.PostgreSQL.Simple

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.Common.Util.String (f'')
import WizardLib.Public.Database.Mapping.PersistentCommand.PersistentCommandList ()
import WizardLib.Public.Model.PersistentCommand.PersistentCommandList

entityName = "persistent_command"

pageLabel = "persistentCommands"

findPersistentCommandsPage :: AppContextC s sc m => [String] -> Pageable -> [Sort] -> m (Page PersistentCommandList)
findPersistentCommandsPage states pageable sort = do
  -- 1. Prepare variables
  do
    let (statesCondition, statesParam) =
          case states of
            [] -> ("", [])
            _ -> (f' "WHERE state in (%s)" [generateQuestionMarks states], states)
    let conditionParams = statesParam
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    count <- createCountByFn entityName statesCondition statesParam
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT persistent_command.uuid, \
              \       persistent_command.state, \
              \       persistent_command.component, \
              \       persistent_command.function, \
              \       persistent_command.attempts, \
              \       persistent_command.max_attempts, \
              \       persistent_command.created_at, \
              \       persistent_command.updated_at, \
              \       concat(tenant.uuid, '::', \
              \              tenant.name, '::', \
              \              tenant_config.look_and_feel -> 'primaryColor', '::', \
              \              tenant_config.look_and_feel -> 'logoUrl', '::', \
              \              tenant.client_url) AS tenant, \
              \       concat(user_entity.uuid, '::', \
              \              user_entity.first_name, '::', \
              \              user_entity.last_name, '::', \
              \              user_entity.email, '::', \
              \              user_entity.image_url) AS created_by \
              \FROM persistent_command \
              \         LEFT JOIN tenant ON tenant.uuid = persistent_command.tenant_uuid \
              \         LEFT JOIN tenant_config ON tenant_config.uuid = persistent_command.tenant_uuid \
              \         LEFT JOIN user_entity ON user_entity.uuid = persistent_command.created_by AND user_entity.tenant_uuid = persistent_command.tenant_uuid \
              \${statesCondition} \
              \${sort} \
              \OFFSET ${offset} \
              \LIMIT ${limit}"
              [ ("statesCondition", statesCondition)
              , ("sort", mapSort sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
              ]
    logQuery sql conditionParams
    let action conn = query conn sql conditionParams
    entities <- runDB action
    -- 4. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page pageLabel metadata entities
