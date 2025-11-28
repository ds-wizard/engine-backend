module Wizard.Service.User.News.NewsService where

import Control.Monad (void)
import qualified Data.UUID as U

import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

updateNews :: U.UUID -> String -> AppContextM ()
updateNews userUuid lastSeenNewsId = void $ updateUserLastSeenNewsIdUuid userUuid lastSeenNewsId
