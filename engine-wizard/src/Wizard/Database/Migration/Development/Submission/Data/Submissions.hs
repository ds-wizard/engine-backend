module Wizard.Database.Migration.Development.Submission.Data.Submissions where

import Control.Lens ((^.))

import LensesConfig
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Database.Migration.Development.Document.Data.Documents

submissionCreate :: SubmissionCreateDTO
submissionCreate =
  SubmissionCreateDTO
    {_submissionCreateDTOServiceId = defaultSubmissionService ^. sId, _submissionCreateDTODocUuid = doc1 ^. uuid}

submission1 :: SubmissionDTO
submission1 = SubmissionDTO {_submissionDTOLocation = Nothing}
