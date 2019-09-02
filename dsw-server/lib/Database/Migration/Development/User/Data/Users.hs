module Database.Migration.Development.User.Data.Users where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.User.UserCreateDTO
import Model.User.User

userAlbert :: User
userAlbert =
  User
  { _userUuid = fromJust . U.fromString $ "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66"
  , _userName = "Albert"
  , _userSurname = "Einstein"
  , _userEmail = "albert.einstein@example.com"
  , _userRole = "ADMIN"
  , _userPermissions =
      [ "UM_PERM"
      , "ORG_PERM"
      , "KM_PERM"
      , "KM_UPGRADE_PERM"
      , "KM_PUBLISH_PERM"
      , "PM_READ_PERM"
      , "PM_WRITE_PERM"
      , "QTN_PERM"
      , "DMP_PERM"
      ]
  , _userActive = True
  , _userPasswordHash = "sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
  , _userCreatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _userUpdatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

userNikola :: User
userNikola =
  User
  { _userUuid = fromJust . U.fromString $ "30d48cf4-8c8a-496f-bafe-585bd238f798"
  , _userName = "Nikola"
  , _userSurname = "Tesla"
  , _userEmail = "nikola.tesla@example.com"
  , _userRole = "DATASTEWARD"
  , _userPermissions = ["KM_PERM", "KM_UPGRADE_PERM", "KM_PUBLISH_PERM", "PM_READ_PERM", "QTN_PERM", "DMP_PERM"]
  , _userActive = True
  , _userPasswordHash = "sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
  , _userCreatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _userUpdatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

userIsaac :: User
userIsaac =
  User
  { _userUuid = fromJust . U.fromString $ "e1c58e52-0824-4526-8ebe-ec38eec67030"
  , _userName = "Isaac"
  , _userSurname = "Newton"
  , _userEmail = "isaac.newton@example.com"
  , _userRole = "RESEARCHER"
  , _userPermissions = ["PM_READ_PERM", "QTN_PERM", "DMP_PERM"]
  , _userActive = True
  , _userPasswordHash = "sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
  , _userCreatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _userUpdatedAt = Just $ UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

userJohnCreate :: UserCreateDTO
userJohnCreate =
  UserCreateDTO
  { _userCreateDTOName = "John"
  , _userCreateDTOSurname = "Doe"
  , _userCreateDTOEmail = "john.doe@example.com"
  , _userCreateDTORole = Just "ADMIN"
  , _userCreateDTOPassword = "password"
  }
