module Wizard.Specs.Service.Project.Event.ProjectEventServiceSpec where

import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U
import Test.Hspec

import Shared.Common.Constant.Tenant
import Shared.Common.Model.Common.Lens
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.ProjectVersions
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.Project.Event.ProjectEvent
import Wizard.Model.Project.Event.ProjectEventLenses ()
import Wizard.Model.Project.ProjectReply
import Wizard.Model.Project.Version.ProjectVersion
import Wizard.Service.Project.Event.ProjectEventService

-- ---------------------------
-- TESTS
-- ---------------------------
projectEventServiceSpec =
  describe "ProjectEventService" $
    it "squash" $
      -- GIVEN: prepare data
      do
        let versions = [version1]
        let events =
              [ setCreatedAt q1_event1 (dt'' 2018 1 21 1)
              , setCreatedAt (cre_rQ1' project1Uuid) (dt'' 2018 1 21 2)
              , setCreatedAt q1_event2 (dt'' 2018 1 21 3)
              , setCreatedAt (sphse_1' project1Uuid) (dt'' 2018 1 21 4)
              , setCreatedAt q1_event3 (dt'' 2018 1 21 5)
              , setCreatedAt (slble_rQ1' project1Uuid) (dt'' 2018 1 21 6)
              , setCreatedAt q2_event1 (dt'' 2018 1 21 7)
              , setCreatedAt q1_event4 (dt'' 2018 1 21 8)
              , setCreatedAt q1_event5_nikola (dt'' 2018 1 21 9)
              , setCreatedAt q1_event6_anonymous1 (dt'' 2018 1 21 10)
              , setCreatedAt q1_event7_nikola (dt'' 2018 1 21 11)
              , setCreatedAt q1_event8_nikola (dt'' 2018 1 21 12)
              , setCreatedAt q2_event2 (dt'' 2018 1 22 0)
              ]
        -- AND: prepare expectation
        let expEvents =
              [ setCreatedAt (cre_rQ1' project1Uuid) (dt'' 2018 1 21 2)
              , setCreatedAt (sphse_1' project1Uuid) (dt'' 2018 1 21 4)
              , setCreatedAt (slble_rQ1' project1Uuid) (dt'' 2018 1 21 6)
              , setCreatedAt q2_event1 (dt'' 2018 1 21 7)
              , setCreatedAt q1_event4 (dt'' 2018 1 21 8)
              , setCreatedAt q1_event5_nikola (dt'' 2018 1 21 9)
              , setCreatedAt q1_event6_anonymous1 (dt'' 2018 1 21 10)
              , setCreatedAt q1_event7_nikola (dt'' 2018 1 21 11)
              , setCreatedAt q1_event8_nikola (dt'' 2018 1 21 12)
              , setCreatedAt q2_event2 (dt'' 2018 1 22 0)
              ]
        -- WHEN:
        let resultEvents = squash versions events
        -- THEN:
        resultEvents `shouldBe` expEvents

-- ---------------------------
-- EVENTS
-- ---------------------------
q1_event1 :: ProjectEvent
q1_event1 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "4b2a1d62f725"
      , path = "question1"
      , value = StringReply "question1_value_1"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 0
      }

q1_event2 :: ProjectEvent
q1_event2 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "0d2b486b3231"
      , path = "question1"
      , value = StringReply "question1_value_2"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 1
      }

q1_event3 :: ProjectEvent
q1_event3 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "04702766ab48"
      , path = "question1"
      , value = StringReply "question1_value_3"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 2
      }

q2_event1 :: ProjectEvent
q2_event1 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "b2eb9e2aacc7"
      , path = "question2"
      , value = StringReply "question2_value_1"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 3
      }

q1_event4 :: ProjectEvent
q1_event4 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "4db8b2bd8345"
      , path = "question1"
      , value = StringReply "question1_value_4"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = dt'' 2018 1 21 4
      }

q1_event5_nikola :: ProjectEvent
q1_event5_nikola =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "3fa10bf8bc47"
      , path = "question1"
      , value = StringReply "question1_value_5"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = nikola
      , createdAt = dt'' 2018 1 21 5
      }

q1_event6_anonymous1 :: ProjectEvent
q1_event6_anonymous1 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "fbbb6cc6d91c"
      , path = "question1"
      , value = StringReply "question1_value_6"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = Nothing
      , createdAt = dt'' 2018 1 21 6
      }

q1_event7_nikola :: ProjectEvent
q1_event7_nikola =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "fe7cfccc9c50"
      , path = "question1"
      , value = StringReply "question1_value_7"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = nikola
      , createdAt = dt'' 2018 1 21 7
      }

q1_event8_nikola :: ProjectEvent
q1_event8_nikola =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "b9c6b1dd31f8"
      , path = "question1"
      , value = StringReply "question1_value_8"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = nikola
      , createdAt = dt'' 2018 1 21 8
      }

q2_event2 :: ProjectEvent
q2_event2 =
  SetReplyEvent' $
    SetReplyEvent
      { uuid = createEventUuid project1Uuid "a023f5ef76f7"
      , path = "question2"
      , value = StringReply "question2_value_2"
      , projectUuid = project1Uuid
      , tenantUuid = defaultTenantUuid
      , createdBy = albert
      , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
      }

-- ---------------------------
-- VERSIONS
-- ---------------------------
version1 = (projectVersion1 project1Uuid) {eventUuid = getUuid q1_event7_nikola}

-- ---------------------------
-- USERS
-- ---------------------------
albert :: Maybe U.UUID
albert = Just $ u' "3e9da440-0a4f-43dc-86b0-0fe9009ae6f3"

nikola :: Maybe U.UUID
nikola = Just $ u' "dbcc9ac4-7e63-4d12-9a14-f2f918fd0a78"
