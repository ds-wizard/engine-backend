module Shared.Database.Mapping.DocumentTemplate.DocumentTemplateFileList where

import Database.PostgreSQL.Simple

import Shared.Model.DocumentTemplate.DocumentTemplateFileList

instance FromRow DocumentTemplateFileList
