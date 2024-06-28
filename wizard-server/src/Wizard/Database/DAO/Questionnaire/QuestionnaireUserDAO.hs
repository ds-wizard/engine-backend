module Wizard.Database.DAO.Questionnaire.QuestionnaireUserDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.User.UserSuggestion ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.UserSuggestion

findQuestionnaireUserSuggestionsPage :: U.UUID -> String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page UserSuggestion)
findQuestionnaireUserSuggestionsPage qtnUuid perm mQuery pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks currentTenantUuid
    let (qCondition, qRegex) =
          ( "WHERE (concat(first_name, ' ', last_name) ~* ? OR email ~* ?)"
          , [regexM mQuery, regexM mQuery]
          )
    let params = [U.toString qtnUuid, U.toString tenantUuid, U.toString qtnUuid, U.toString tenantUuid] ++ qRegex
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    let countSql =
          fromString $
            f''
              "SELECT DISTINCT COUNT(uuid) \
              \FROM (SELECT u.uuid, \
              \             u.first_name, \
              \             u.last_name, \
              \             u.email \
              \      FROM questionnaire_perm_user qtn_perm_user \
              \      JOIN user_entity u ON qtn_perm_user.user_uuid = u.uuid AND qtn_perm_user.tenant_uuid = u.tenant_uuid \
              \      WHERE qtn_perm_user.questionnaire_uuid = ? \
              \        AND qtn_perm_user.tenant_uuid = ? \
              \        AND qtn_perm_user.perms @> ARRAY ['${perm}'] \
              \        AND u.active = true \
              \        AND u.machine = false \
              \      UNION ALL \
              \      SELECT u.uuid, \
              \             u.first_name, \
              \             u.last_name, \
              \             u.email \
              \      FROM questionnaire_perm_group qtn_perm_group \
              \      LEFT JOIN user_group_membership ug_membership ON ug_membership.user_group_uuid = qtn_perm_group.user_group_uuid AND ug_membership.tenant_uuid = qtn_perm_group.tenant_uuid \
              \      LEFT JOIN user_entity u ON u.uuid = ug_membership.user_uuid AND u.tenant_uuid = qtn_perm_group.tenant_uuid \
              \      WHERE qtn_perm_group.questionnaire_uuid = ? \
              \        AND qtn_perm_group.tenant_uuid = ? \
              \        AND qtn_perm_group.perms @> ARRAY ['${perm}'] \
              \        AND u.active = true \
              \        AND u.machine = false) u \
              \${qCondition}"
              [ ("qCondition", qCondition)
              , ("perm", perm)
              ]
    logQuery countSql params
    let action conn = query conn countSql params
    result <- runDB action
    let count =
          case result of
            [count] -> fromOnly count
            _ -> 0
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT DISTINCT * \
              \FROM (SELECT u.uuid, \
              \             u.first_name, \
              \             u.last_name, \
              \             u.email, \
              \             u.image_url \
              \      FROM questionnaire_perm_user qtn_perm_user \
              \      JOIN user_entity u ON qtn_perm_user.user_uuid = u.uuid AND qtn_perm_user.tenant_uuid = u.tenant_uuid \
              \      WHERE qtn_perm_user.questionnaire_uuid = ? \
              \        AND qtn_perm_user.tenant_uuid = ? \
              \        AND qtn_perm_user.perms @> ARRAY ['${perm}'] \
              \        AND u.active = true \
              \        AND u.machine = false \
              \      UNION ALL \
              \      SELECT u.uuid, \
              \             u.first_name, \
              \             u.last_name, \
              \             u.email, \
              \             u.image_url \
              \      FROM questionnaire_perm_group qtn_perm_group \
              \      LEFT JOIN user_group_membership ug_membership ON ug_membership.user_group_uuid = qtn_perm_group.user_group_uuid AND ug_membership.tenant_uuid = qtn_perm_group.tenant_uuid \
              \      LEFT JOIN user_entity u ON u.uuid = ug_membership.user_uuid AND u.tenant_uuid = qtn_perm_group.tenant_uuid \
              \      WHERE qtn_perm_group.questionnaire_uuid = ? \
              \        AND qtn_perm_group.tenant_uuid = ? \
              \        AND qtn_perm_group.perms @> ARRAY ['${perm}'] \
              \        AND u.active = true \
              \        AND u.machine = false) u \
              \${qCondition} \
              \${sort} \
              \OFFSET ${offset} \
              \LIMIT ${limit}"
              [ ("qCondition", qCondition)
              , ("perm", perm)
              , ("sort", mapSort sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
              ]
    logQuery sql params
    let action conn = query conn sql params
    entities <- runDB action
    -- 5. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page "users" metadata entities
