module Wizard.Api.Handler.Submission.Api where

import Servant

import Wizard.Api.Handler.Submission.List_POST
import Wizard.Model.Context.BaseContext

type SubmissionAPI = List_POST

submissionApi :: Proxy SubmissionAPI
submissionApi = Proxy

submissionServer :: ServerT SubmissionAPI BaseContextM
submissionServer = list_POST
