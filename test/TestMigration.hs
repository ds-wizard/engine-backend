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
import Model.Organization.Organization
import Model.User.User
import Service.Organization.OrganizationService
import Service.User.UserService

resetDB context dspConfig = do
  deleteUsers context
  createUserWithGivenUuid
    context
    dspConfig
    (fromJust (U.fromString "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"))
    UserCreateDTO
    { _ucdtoName = "Darth"
    , _ucdtoSurname = "Vader"
    , _ucdtoEmail = "darth.vader@deathstar.com"
    , _ucdtoRole = Just "ADMIN"
    , _ucdtoPassword = "password"
    }
    True
  eitherUser <- findUserByEmail context "darth.vader@deathstar.com"
  let (Right user) = eitherUser
  let updatedUser = user & uIsActive .~ True
  updateUserById context updatedUser
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
