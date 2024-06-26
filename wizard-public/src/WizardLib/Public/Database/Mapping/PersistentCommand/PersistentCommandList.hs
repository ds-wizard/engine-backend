module WizardLib.Public.Database.Mapping.PersistentCommand.PersistentCommandList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Common.Util.Gravatar
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.PersistentCommand ()
import WizardLib.Public.Api.Resource.Tenant.TenantSuggestionDTO
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO
import WizardLib.Public.Model.PersistentCommand.PersistentCommandList

instance FromRow PersistentCommandList where
  fromRow = do
    uuid <- field
    state <- field
    component <- field
    function <- field
    attempts <- field
    maxAttempts <- field
    createdAt <- field
    updatedAt <- field
    tenantS <- field
    let tenant = parseTenant tenantS
    createdByS <- fieldWith (optionalField fromField)
    let createdBy = fmap parseUser createdByS
    return $ PersistentCommandList {..}
    where
      parseTenant :: String -> TenantSuggestionDTO
      parseTenant tenant =
        let parts = splitOn "::" tenant
         in TenantSuggestionDTO
              { uuid = u' . head $ parts
              , name = parts !! 1
              , logoUrl =
                  case parts !! 2 of
                    "" -> Nothing
                    logoUrl -> Just logoUrl
              , primaryColor =
                  case parts !! 3 of
                    "" -> Nothing
                    primaryColor -> Just primaryColor
              , clientUrl = parts !! 4
              }
      parseUser :: String -> UserSuggestionDTO
      parseUser user =
        let parts = splitOn "::" user
         in UserSuggestionDTO
              { uuid = u' . head $ parts
              , firstName = parts !! 1
              , lastName = parts !! 2
              , gravatarHash = createGravatarHash $ parts !! 3
              , imageUrl =
                  case parts !! 4 of
                    "" -> Nothing
                    imageUrl -> Just imageUrl
              }
