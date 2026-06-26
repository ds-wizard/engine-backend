module Wizard.Specs.API.Role.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Model.Context.AppContext ()
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Public.Database.DAO.User.RoleDAO
import WizardLib.Public.Model.User.Role
import WizardLib.Public.Model.User.RoleList

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfRoleInDB appContext role = do
  eRole <- runInContextIO (findRoleByUuid role.uuid) appContext
  liftIO $ isRight eRole `shouldBe` True
  let (Right roleFromDB) = eRole
  compareRoles roleFromDB role

assertAbsenceOfRoleInDB appContext roleUuid = do
  roles <- runInContextIO findRoles appContext
  let exists = either (const False) (any (\r -> r.uuid == roleUuid)) roles
  liftIO $ exists `shouldBe` False

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareRoles :: Role -> Role -> WaiSession st ()
compareRoles resModel expModel = do
  liftIO $ resModel.uuid `shouldBe` expModel.uuid
  liftIO $ resModel.name `shouldBe` expModel.name
  liftIO $ resModel.permissions `shouldBe` expModel.permissions
  liftIO $ resModel.isAdmin `shouldBe` expModel.isAdmin
  liftIO $ resModel.tenantUuid `shouldBe` expModel.tenantUuid

compareRoleDtos :: RoleList -> RoleList -> WaiSession st ()
compareRoleDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.permissions `shouldBe` expDto.permissions
  liftIO $ resDto.usersCount `shouldBe` expDto.usersCount
  liftIO $ resDto.isAdmin `shouldBe` expDto.isAdmin
