module Wizard.Service.Questionnaire.File.QuestionnaireFileMapper where

import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.File.FileCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireFile
import Wizard.Model.Questionnaire.QuestionnaireFileList
import Wizard.Model.Questionnaire.QuestionnaireFileSimple
import qualified Wizard.Service.Questionnaire.QuestionnaireMapper as QuestionnaireMapper
import qualified Wizard.Service.User.UserMapper as UserMapper

toList :: QuestionnaireFile -> Questionnaire -> Maybe UserDTO -> QuestionnaireFileList
toList QuestionnaireFile {..} questionnaire mCreatedBy =
  QuestionnaireFileList
    { uuid = uuid
    , fileName = fileName
    , contentType = contentType
    , fileSize = fileSize
    , questionnaire = QuestionnaireMapper.toSimple questionnaire
    , createdBy = fmap UserMapper.toSuggestionDTO' mCreatedBy
    , createdAt = createdAt
    }

toSimple :: QuestionnaireFile -> QuestionnaireFileSimple
toSimple QuestionnaireFile {..} =
  QuestionnaireFileSimple
    { uuid = uuid
    , fileName = fileName
    , contentType = contentType
    , fileSize = fileSize
    }

fromFileCreateDTO :: FileCreateDTO -> U.UUID -> U.UUID -> Maybe UserDTO -> U.UUID -> UTCTime -> QuestionnaireFile
fromFileCreateDTO reqDto uuid qtnUuid mCreatedBy tenantUuid now =
  QuestionnaireFile
    { uuid = uuid
    , fileName = reqDto.fileName
    , contentType = reqDto.contentType
    , fileSize = fromIntegral . BS.length $ reqDto.content
    , questionnaireUuid = qtnUuid
    , createdBy = fmap (.uuid) mCreatedBy
    , tenantUuid = tenantUuid
    , createdAt = now
    }
