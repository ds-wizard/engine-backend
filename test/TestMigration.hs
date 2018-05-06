module TestMigration where

import Control.Lens ((&), (.~), (^.))
import Data.Maybe
import qualified Data.UUID as U

import Api.Resource.User.UserCreateDTO
import Api.Resource.User.UserDTO
import Common.Context
import Database.DAO.ActionKey.ActionKeyDAO
import Database.DAO.Branch.BranchDAO
import Database.DAO.Migrator.MigratorDAO
import Database.DAO.Organization.OrganizationDAO
import Database.DAO.User.UserDAO
import LensesConfig
import Model.Context.AppContext
import Model.Organization.Organization
import Model.User.User
import Service.Organization.OrganizationService
import Service.User.UserService

createUserAlbert appContext = do
  let context = appContext ^. oldContext
  let dswConfig = appContext ^. config
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

createUserNikola appContext = do
  let context = appContext ^. oldContext
  let dswConfig = appContext ^. config
  createUserWithGivenUuid
    context
    dswConfig
    (fromJust (U.fromString "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"))
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

resetDB appContext = do
  let context = appContext ^. oldContext
  let dswConfig = appContext ^. config
  deleteUsers context
  createUserAlbert appContext
  deleteOrganizations context
  insertOrganization
    context
    Organization
    { _orgUuid = (fromJust (U.fromString "d0619a24-db8a-48e1-a033-0d4ef8b8da78"))
    , _orgName = "Elixir Amsterdam"
    , _orgGroupId = "elixir.nl.amsterdam"
    }
  deleteBranches context
  deleteMigratorStates context
  deleteActionKeys context
  return ()
