module Service.Statistics.StatisticsService
  ( getInstanceStatistics
  -- Helpers
  , heGetInstanceStatistics
  ) where

import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.DAO.User.UserDAO
import Model.Context.AppContext
import Model.Error.Error
import Model.Statistics.InstanceStatistics

getInstanceStatistics :: AppContextM (Either AppError InstanceStatistics)
getInstanceStatistics = do
  heCountUsers $ \uCount ->
    heCountPackages $ \pCount ->
      heCountQuestionnaires $ \qCount ->
        return . Right $
        InstanceStatistics
        { _instanceStatisticsUserCount = uCount
        , _instanceStatisticsPkgCount = pCount
        , _instanceStatisticsQtnCount = qCount
        }

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetInstanceStatistics callback = do
  eitherResult <- getInstanceStatistics
  case eitherResult of
    Right result -> callback result
    Left error -> return . Left $ error
