module Wizard.Service.Template.File.TemplateFileService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.List as L
import qualified Data.UUID as U

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Shared.Util.Uuid
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO
import Wizard.Database.DAO.Template.TemplateDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Common.ACL
import Wizard.Service.Template.File.TemplateFileMapper

getTemplateFiles :: String -> AppContextM [TemplateFile]
getTemplateFiles tmlId = do
  checkPermission _TML_PERM
  tml <- findTemplateById tmlId
  return $ tml ^. files

getTemplateFile :: String -> String -> AppContextM TemplateFile
getTemplateFile tmlId fileUuid = do
  checkPermission _TML_PERM
  tml <- findTemplateById tmlId
  case L.find (\f -> U.toString (f ^. uuid) == fileUuid) (tml ^. files) of
    Just file -> return file
    Nothing -> throwError $ NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "file" fileUuid

createTemplateFile :: String -> TemplateFileChangeDTO -> AppContextM TemplateFile
createTemplateFile tmlId reqDto
  -- 1. Check permission
 = do
  checkPermission _TML_PERM
  -- 2. Check existence of template
  tml <- findTemplateById tmlId
  -- 3. Create file
  aUuid <- liftIO generateUuid
  let newFile = fromChangeDTO reqDto aUuid
  -- 4. Add to template
  let updatedTml = tml & files .~ ((tml ^. files) ++ [newFile])
  updateTemplateById updatedTml
  -- 5. Return file
  return newFile

modifyTemplateFile :: String -> String -> TemplateFileChangeDTO -> AppContextM TemplateFile
modifyTemplateFile tmlId fileUuid reqDto
  -- 1. Check permission
 = do
  checkPermission _TML_PERM
  -- 2. Check existence of template and file
  tml <- findTemplateById tmlId
  file <- getTemplateFile tmlId fileUuid
  -- 3. Create file
  let updatedFile = fromChangeDTO reqDto (file ^. uuid)
  -- 4. Update to template
  let updatedTml = tml & files .~ updateFileInList updatedFile (tml ^. files)
  updateTemplateById updatedTml
  -- 5. Return file
  return updatedFile

deleteTemplateFile :: String -> String -> AppContextM ()
deleteTemplateFile tmlId fileUuid = do
  checkPermission _TML_PERM
  file <- getTemplateFile tmlId fileUuid
  tml <- findTemplateById tmlId
  let updatedTml = tml & files .~ filter (\a -> a ^. uuid /= file ^. uuid) (tml ^. files)
  updateTemplateById updatedTml

updateFileInList :: TemplateFile -> [TemplateFile] -> [TemplateFile]
updateFileInList file = foldl go []
  where
    go :: [TemplateFile] -> TemplateFile -> [TemplateFile]
    go acc f =
      if f ^. uuid == file ^. uuid
        then acc ++ [file]
        else acc ++ [f]
