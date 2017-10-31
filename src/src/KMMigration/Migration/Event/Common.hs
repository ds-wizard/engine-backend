module KMMigration.Migration.Event.Common where

import Control.Lens

import Model.KnowledgeModel.KnowledgeModel

applyValue (Just val) ch setter = ch & setter .~ val
applyValue Nothing ch setter = ch
