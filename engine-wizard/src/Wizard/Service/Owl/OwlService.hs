module Wizard.Service.Owl.OwlService where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Servant.Multipart (Input(..))

import LensesConfig
import Shared.Database.DAO.Package.PackageDAO
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Event.Event
import Shared.Service.Package.PackageMapper
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.Compilator.Compilator
import Wizard.Service.Owl.Convertor.OwlConvertor
import Wizard.Service.Owl.Diff.Differ
import Wizard.Service.Owl.OwlMapper
import Wizard.Service.Package.PackageMapper

importOwl :: [Input] -> BSL.ByteString -> AppContextM [PackageSimpleDTO]
importOwl inputs content = do
  rootElement <- getTextFromInput "rootElement" inputs
  name <- getStringFromInput "name" inputs
  organizationId <- getStringFromInput "organizationId" inputs
  kmId <- getStringFromInput "kmId" inputs
  version <- getStringFromInput "version" inputs
  let mPreviousPackageId = getMaybeStringFromInput "previousPackageId" inputs
  appUuid <- asks _appContextAppUuid
  now <- liftIO getCurrentTime
  events <- importEvents mPreviousPackageId rootElement (TE.decodeUtf8 . BSL.toStrict $ content)
  let pkg = fromOwl name organizationId kmId version mPreviousPackageId events appUuid now
  insertPackage pkg
  return [toSimpleDTO . toPackage $ pkg]

importEvents :: Maybe String -> T.Text -> T.Text -> AppContextM [Event]
importEvents mPreviousPackageId rootElement content = do
  pkgEvents <- convertOwlToEvents rootElement content
  case mPreviousPackageId of
    Just previousPackageId -> do
      previousPackage <- findPackageWithEventsById previousPackageId
      let (Right km1) = compile Nothing pkgEvents
      let (Right km2) = compile Nothing (previousPackage ^. events)
      diffKnowledgeModel (km1, km2)
    Nothing -> return pkgEvents

-- --------------------------------
-- PRIVATE
-- --------------------------------
getTextFromInput :: T.Text -> [Input] -> AppContextM T.Text
getTextFromInput name inputs = do
  case getMaybeTextFromInput name inputs of
    Just text -> return text
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__FIELD_ABSENCE (T.unpack name)

getMaybeTextFromInput :: T.Text -> [Input] -> Maybe T.Text
getMaybeTextFromInput name = fmap iValue . L.find (\i -> iName i == name)

getStringFromInput :: T.Text -> [Input] -> AppContextM String
getStringFromInput name inputs = fmap T.unpack $ getTextFromInput name inputs

getMaybeStringFromInput :: T.Text -> [Input] -> Maybe String
getMaybeStringFromInput name inputs = fmap T.unpack $ getMaybeTextFromInput name inputs
