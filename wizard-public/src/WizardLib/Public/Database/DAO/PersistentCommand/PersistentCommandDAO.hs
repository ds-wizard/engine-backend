module WizardLib.Public.Database.DAO.PersistentCommand.PersistentCommandDAO where

import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.Common.Util.String (f'')
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.LambdaInvocationResult ()
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.PersistentCommand ()
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.PersistentCommandSimple ()
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple

findPersistentCommandsForLambdaByStates :: AppContextC s sc m => [String] -> m [PersistentCommandSimple U.UUID]
findPersistentCommandsForLambdaByStates components = do
  let componentCondition =
        case components of
          [] -> ""
          _ -> f' "AND component IN (%s) " [generateQuestionMarks components]
  let sql =
        fromString $
          f''
            "SELECT uuid, destination, component, tenant_uuid, created_by \
            \FROM persistent_command \
            \WHERE (state = 'NewPersistentCommandState' \
            \  OR (state = 'ErrorPersistentCommandState' AND attempts < max_attempts AND updated_at < (now() - (2 ^ attempts - 1) * INTERVAL '1 min'))) \
            \  AND internal = false ${componentCondition} \
            \ORDER BY created_at \
            \LIMIT 5 \
            \FOR UPDATE"
            [ ("componentCondition", componentCondition)
            ]
  let params = components
  logQuery sql params
  let action conn = query conn sql params
  runDB action
