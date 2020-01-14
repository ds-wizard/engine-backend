module Wizard.Service.Statistics.StatisticsService
  ( getInstanceStatistics
  -- Helpers
  , heGetInstanceStatistics
  ) where

import Shared.Model.Error.Error
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Statistics.InstanceStatistics

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
