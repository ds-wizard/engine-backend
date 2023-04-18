module Wizard.Database.DAO.Questionnaire.QuestionnaireProjectTagDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

entityName = "questionnaire"

pageLabel = "projectTags"

findQuestionnaireProjectTagsPage :: Maybe String -> [String] -> Pageable -> [Sort] -> AppContextM (Page String)
findQuestionnaireProjectTagsPage mQuery excludeTags pageable sort =
  -- 1. Prepare variables
  do
    appUuid <- asks currentAppUuid
    let params = [U.toString appUuid, regex mQuery] ++ excludeTags
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    count <- findCount excludeTags params
    -- 3. Prepare SQL
    let sql =
          fromString $
            f'
              "SELECT * \
              \FROM (%s) merged \
              \WHERE merged.project_tag ~* ? %s \
              \%s \
              \OFFSET %s LIMIT %s"
              [sqlBase, excludeTagsCondition excludeTags, mapSort sort, show skip, show sizeI]
    createFindColumnBySqlPageFn pageLabel pageable sql params count

findCount :: [String] -> [String] -> AppContextM Int
findCount excludeTags params = do
  let sql =
        fromString $
          f'
            "SELECT COUNT(*) \
            \FROM (%s) merged \
            \WHERE merged.project_tag::text ~* ? %s"
            [sqlBase, excludeTagsCondition excludeTags]
  createCountWithSqlFn sql params

sqlBase :: String
sqlBase =
  "SELECT jsonb_array_elements_text((questionnaire -> 'projectTagging' ->> 'tags') ::jsonb) as project_tag \
  \FROM app_config \
  \UNION \
  \SELECT nested.project_tag \
  \FROM (SELECT unnest(project_tags) as project_tag, app_uuid FROM questionnaire) nested \
  \WHERE nested.app_uuid = ? "

excludeTagsCondition :: [String] -> String
excludeTagsCondition excludeTags =
  if null excludeTags
    then ""
    else f' "AND NOT (project_tag IN (%s))" [generateQuestionMarks excludeTags]
