module TestMigration where

import Data.Maybe
import qualified Data.UUID as U

import Api.Resources.User.UserCreateDTO
import Context
import Database.DAO.KnowledgeModelContainer.KnowledgeModelContainerDAO
import Database.DAO.Organization.OrganizationDAO
import Database.DAO.User.UserDAO
import Model.Organization.Organization
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
    , _ucdtoRole = "ADMIN"
    , _ucdtoPassword = "password"
    }
  deleteOrganizations context
  insertOrganization
    context
    Organization
    { _orgUuid =
        (fromJust (U.fromString "d0619a24-db8a-48e1-a033-0d4ef8b8da78"))
    , _orgName = "Elixir Netherlands"
    , _orgNamespace = "elixir-nl"
    }
  deleteKnowledgeModelContainers context
  return ()
