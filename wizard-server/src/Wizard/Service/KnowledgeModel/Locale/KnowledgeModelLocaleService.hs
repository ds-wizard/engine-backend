module Wizard.Service.KnowledgeModel.Locale.KnowledgeModelLocaleService where

import Control.Monad (void, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (Value, decodeStrict)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Localization.Messages.Public
import Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleCreateDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Wizard.S3.KnowledgeModel.KnowledgeModelLocaleS3
import Wizard.Service.KnowledgeModel.Locale.KnowledgeModelLocaleMapper
import Wizard.Service.KnowledgeModel.Locale.KnowledgeModelLocaleUtil
import Wizard.Service.KnowledgeModel.Locale.KnowledgeModelLocaleValidation

getLocalesForPackage :: U.UUID -> AppContextM [KnowledgeModelLocaleList]
getLocalesForPackage pkgUuid = do
  _ <- findPackageByUuid pkgUuid
  locales <- findKnowledgeModelLocalesByPackageUuid pkgUuid
  return . fmap toList $ locales

createLocale :: U.UUID -> KnowledgeModelLocaleCreateDTO -> AppContextM KnowledgeModelLocaleList
createLocale pkgUuid reqDto =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
    _ <- findPackageByUuid pkgUuid
    code <- extractLanguageCode reqDto.poContent
    validateJsonContent reqDto.jsonContent
    validateCodeUniqueness pkgUuid code
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    tenantUuid <- asks currentTenantUuid
    let locale =
          KnowledgeModelLocale
            { uuid = uuid
            , name = reqDto.name
            , code = code
            , knowledgeModelPackageUuid = pkgUuid
            , tenantUuid = tenantUuid
            , createdAt = now
            , updatedAt = now
            }
    insertKnowledgeModelLocale locale
    putKnowledgeModelLocale uuid translationPoFileName reqDto.poContent
    putKnowledgeModelLocale uuid translationJsonFileName reqDto.jsonContent
    return . toList $ locale

getLocaleContent :: U.UUID -> U.UUID -> AppContextM BS.ByteString
getLocaleContent pkgUuid localeUuid = do
  locale <- findKnowledgeModelLocaleByPackageUuidAndUuid pkgUuid localeUuid
  retrieveKnowledgeModelLocale locale.uuid translationPoFileName

deleteLocale :: U.UUID -> U.UUID -> AppContextM ()
deleteLocale pkgUuid localeUuid =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
    locale <- findKnowledgeModelLocaleByPackageUuidAndUuid pkgUuid localeUuid
    void $ deleteKnowledgeModelLocaleByUuid locale.uuid

findLocaleJson :: U.UUID -> Maybe String -> AppContextM (Maybe Value)
findLocaleJson _ Nothing = return Nothing
findLocaleJson pkgUuid (Just code) =
  catchError
    ( do
        mLocale <- findKnowledgeModelLocaleByPackageUuidAndCode' pkgUuid code
        case mLocale of
          Just locale -> do
            content <- retrieveKnowledgeModelLocale locale.uuid translationJsonFileName
            return . decodeStrict $ content
          Nothing -> return Nothing
    )
    (\_ -> return Nothing)

getReusableLocalesForEditor :: U.UUID -> AppContextM [KnowledgeModelLocaleList]
getReusableLocalesForEditor editorUuid = do
  checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
  editor <- findKnowledgeModelEditorByUuid editorUuid
  case editor.previousPackageUuid of
    Just previousPackageUuid -> do
      locales <- findKnowledgeModelLocalesByPackageUuid previousPackageUuid
      return . fmap toList $ locales
    Nothing -> return []

copyLocalesForPublishedPackage :: Maybe [U.UUID] -> Maybe U.UUID -> U.UUID -> AppContextM ()
copyLocalesForPublishedPackage Nothing _ _ = return ()
copyLocalesForPublishedPackage (Just []) _ _ = return ()
copyLocalesForPublishedPackage (Just localeUuids) mPreviousPackageUuid targetPkgUuid =
  case mPreviousPackageUuid of
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__KM_LOCALE_NOT_REUSABLE
    Just previousPackageUuid -> do
      reusableLocales <- findKnowledgeModelLocalesByPackageUuid previousPackageUuid
      let selectedLocales = filter (\l -> l.uuid `elem` localeUuids) reusableLocales
      when
        (length selectedLocales /= length localeUuids)
        (throwError . UserError $ _ERROR_VALIDATION__KM_LOCALE_NOT_REUSABLE)
      copyLocales targetPkgUuid selectedLocales

copyLocales :: U.UUID -> [KnowledgeModelLocale] -> AppContextM ()
copyLocales targetPkgUuid locales = do
  now <- liftIO getCurrentTime
  mapM_ (copyLocale now) locales
  where
    copyLocale now locale = do
      newUuid <- liftIO generateUuid
      let newLocale =
            locale
              { uuid = newUuid
              , knowledgeModelPackageUuid = targetPkgUuid
              , createdAt = now
              , updatedAt = now
              }
      insertKnowledgeModelLocale newLocale
      poContent <- retrieveKnowledgeModelLocale locale.uuid translationPoFileName
      jsonContent <- retrieveKnowledgeModelLocale locale.uuid translationJsonFileName
      putKnowledgeModelLocale newUuid translationPoFileName poContent
      void $ putKnowledgeModelLocale newUuid translationJsonFileName jsonContent
