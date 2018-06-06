module Service.DataManagementPlan.Templates.HtmlMapper where

import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import LensesConfig
import Service.DataManagementPlan.Templates.Html

toHTML :: DataManagementPlanDTO -> BSL.ByteString
toHTML dmp = BSL.fromStrict . E.encodeUtf8 . T.pack . dmp2html $ dmp ^. filledKnowledgeModel
