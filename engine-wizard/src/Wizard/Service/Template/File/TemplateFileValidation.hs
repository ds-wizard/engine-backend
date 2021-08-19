module Wizard.Service.Template.File.TemplateFileValidation where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.DAO.Template.TemplateAssetDAO
import Shared.Database.DAO.Template.TemplateFileDAO
import Shared.Model.Error.Error
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
