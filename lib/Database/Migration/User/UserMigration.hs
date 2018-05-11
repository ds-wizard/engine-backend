module Database.Migration.User.UserMigration where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Logger (logInfo)
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromJust)
import qualified Data.UUID as U

import Api.Resource.User.UserCreateDTO
import Database.DAO.User.UserDAO
import LensesConfig
import Model.User.User
import Service.User.UserService

runMigration appContext = do
  $(logInfo) "MIGRATION (User/User): started"
  let context = appContext ^. oldContext
  let dswConfig = appContext ^. config
  liftIO $ deleteUsers context
  liftIO $ insertAdmin context dswConfig
  liftIO $ insertDataSteward context dswConfig
  liftIO $ insertResearcher context dswConfig
  $(logInfo) "MIGRATION (User/User): ended"

insertAdmin context dswConfig = do
  liftIO $
    createUserWithGivenUuid
      context
      dswConfig
      (fromJust (U.fromString "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"))
      UserCreateDTO
      { _ucdtoName = "Albert"
      , _ucdtoSurname = "Einstein"
      , _ucdtoEmail = "albert.einstein@example.com"
      , _ucdtoRole = Just "ADMIN"
      , _ucdtoPassword = "password"
      }
      True
  eitherUser <- findUserByEmail context "albert.einstein@example.com"
  let (Right user) = eitherUser
  let updatedUser = user & uIsActive .~ True
  updateUserById context updatedUser

insertDataSteward context dswConfig = do
  liftIO $
    createUserWithGivenUuid
      context
      dswConfig
      (fromJust (U.fromString "30d48cf4-8c8a-496f-bafe-585bd238f798"))
      UserCreateDTO
      { _ucdtoName = "Nikola"
      , _ucdtoSurname = "Tesla"
      , _ucdtoEmail = "nikola.tesla@example.com"
      , _ucdtoRole = Just "DATASTEWARD"
      , _ucdtoPassword = "password"
      }
      True
  eitherUser <- findUserByEmail context "nikola.tesla@example.com"
  let (Right user) = eitherUser
  let updatedUser = user & uIsActive .~ True
  updateUserById context updatedUser

insertResearcher context dswConfig = do
  liftIO $
    createUserWithGivenUuid
      context
      dswConfig
      (fromJust (U.fromString "e1c58e52-0824-4526-8ebe-ec38eec67030"))
      UserCreateDTO
      { _ucdtoName = "Isaac"
      , _ucdtoSurname = "Newton"
      , _ucdtoEmail = "isaac.newton@example.com"
      , _ucdtoRole = Just "RESEARCHER"
      , _ucdtoPassword = "password"
      }
      True
  eitherUser <- findUserByEmail context "isaac.newton@example.com"
  let (Right user) = eitherUser
  let updatedUser = user & uIsActive .~ True
  updateUserById context updatedUser
