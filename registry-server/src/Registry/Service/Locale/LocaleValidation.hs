module Registry.Service.Locale.LocaleValidation where

import Registry.Model.Config.ServerConfig
import Registry.Model.Context.AppContext
import Registry.Service.Common

checkIfLocaleEnabled :: AppContextM ()
checkIfLocaleEnabled = checkIfServerFeatureIsEnabled "Locale" (\s -> s.general.localeEnabled)
