module Result where

result =
  [ AddQuestionEvent'
      (AddOptionsQuestionEvent'
         (AddOptionsQuestionEvent
            { _addOptionsQuestionEventUuid = abfcd9db - 64 df - 44 c7 - 96 d4 - 135 d543a0001
            , _addOptionsQuestionEventParentUuid = 70 c0e4f3 - 7 a67 - 49 dd - 8043 - e504392d7903
            , _addOptionsQuestionEventEntityUuid = 1890 b807 - 83e8 - 4 a20 - 8515 - 83930 cab0001
            , _addOptionsQuestionEventTitle = ""
            , _addOptionsQuestionEventText = Nothing
            , _addOptionsQuestionEventRequiredPhaseUuid = Nothing
            , _addOptionsQuestionEventAnnotations = []
            , _addOptionsQuestionEventTagUuids = []
            , _addOptionsQuestionEventCreatedAt = 2018 - 01 - 01 00 : 00 : 01 UTC
            }))
  , AddQuestionEvent'
      (AddOptionsQuestionEvent'
         (AddOptionsQuestionEvent
            { _addOptionsQuestionEventUuid = abfcd9db - 64 df - 44 c7 - 96 d4 - 135 d543a0009
            , _addOptionsQuestionEventParentUuid = 70 c0e4f3 - 7 a67 - 49 dd - 8043 - e504392d7903
            , _addOptionsQuestionEventEntityUuid = 1890 b807 - 83e8 - 4 a20 - 8515 - 83930 cab0005
            , _addOptionsQuestionEventTitle = ""
            , _addOptionsQuestionEventText = Nothing
            , _addOptionsQuestionEventRequiredPhaseUuid = Nothing
            , _addOptionsQuestionEventAnnotations = []
            , _addOptionsQuestionEventTagUuids = []
            , _addOptionsQuestionEventCreatedAt = 2018 - 01 - 01 00 : 00 : 01 UTC
            }))
  , AddChoiceEvent'
      (AddChoiceEvent
         { _addChoiceEventUuid = abfcd9db - 64 df - 44 c7 - 96 d4 - 135 d543a0002
         , _addChoiceEventParentUuid = 70 c0e4f3 - 7 a67 - 49 dd - 8043 - e504392d7903
         , _addChoiceEventEntityUuid = 1890 b807 - 83e8 - 4 a20 - 8515 - 83930 cab0002
         , _addChoiceEventLabel = ""
         , _addChoiceEventAnnotations = []
         , _addChoiceEventCreatedAt = 2018 - 01 - 01 00 : 00 : 02 UTC
         })
  , AddReferenceEvent'
      (AddResourcePageReferenceEvent'
         (AddResourcePageReferenceEvent
            { _addResourcePageReferenceEventUuid = abfcd9db - 64 df - 44 c7 - 96 d4 - 135 d543a0005
            , _addResourcePageReferenceEventParentUuid = 70 c0e4f3 - 7 a67 - 49 dd - 8043 - e504392d7903
            , _addResourcePageReferenceEventEntityUuid = 1890 b807 - 83e8 - 4 a20 - 8515 - 83930 cab0003
            , _addResourcePageReferenceEventShortUuid = ""
            , _addResourcePageReferenceEventAnnotations = []
            , _addResourcePageReferenceEventCreatedAt = 2018 - 01 - 01 00 : 00 : 05 UTC
            }))
  , AddQuestionEvent'
      (AddOptionsQuestionEvent'
         (AddOptionsQuestionEvent
            { _addOptionsQuestionEventUuid = abfcd9db - 64 df - 44 c7 - 96 d4 - 135 d543a0006
            , _addOptionsQuestionEventParentUuid = 70 c0e4f3 - 7 a67 - 49 dd - 8043 - e504392d7903
            , _addOptionsQuestionEventEntityUuid = 1890 b807 - 83e8 - 4 a20 - 8515 - 83930 cab0004
            , _addOptionsQuestionEventTitle = ""
            , _addOptionsQuestionEventText = Nothing
            , _addOptionsQuestionEventRequiredPhaseUuid = Nothing
            , _addOptionsQuestionEventAnnotations = []
            , _addOptionsQuestionEventTagUuids = []
            , _addOptionsQuestionEventCreatedAt = 2018 - 01 - 01 00 : 00 : 06 UTC
            }))
  , EditQuestionEvent'
      (EditOptionsQuestionEvent'
         (EditOptionsQuestionEvent
            { _editOptionsQuestionEventUuid = abfcd9db - 64 df - 44 c7 - 96 d4 - 135 d543a0007
            , _editOptionsQuestionEventParentUuid = 70 c0e4f3 - 7 a67 - 49 dd - 8043 - e504392d7903
            , _editOptionsQuestionEventEntityUuid = 1890 b807 - 83e8 - 4 a20 - 8515 - 83930 cab0004
            , _editOptionsQuestionEventTitle = ChangedValue "Question 2 Title"
            , _editOptionsQuestionEventText = NothingChanged
            , _editOptionsQuestionEventRequiredPhaseUuid = NothingChanged
            , _editOptionsQuestionEventAnnotations = NothingChanged
            , _editOptionsQuestionEventTagUuids = NothingChanged
            , _editOptionsQuestionEventExpertUuids = NothingChanged
            , _editOptionsQuestionEventReferenceUuids = NothingChanged
            , _editOptionsQuestionEventAnswerUuids = NothingChanged
            , _editOptionsQuestionEventCreatedAt = 2018 - 01 - 01 00 : 00 : 07 UTC
            }))
  , EditQuestionEvent'
      (EditOptionsQuestionEvent'
         (EditOptionsQuestionEvent
            { _editOptionsQuestionEventUuid = abfcd9db - 64 df - 44 c7 - 96 d4 - 135 d543a0012
            , _editOptionsQuestionEventParentUuid = 70 c0e4f3 - 7 a67 - 49 dd - 8043 - e504392d7903
            , _editOptionsQuestionEventEntityUuid = 1890 b807 - 83e8 - 4 a20 - 8515 - 83930 cab0001
            , _editOptionsQuestionEventTitle = ChangedValue "Question 1 Title - 2"
            , _editOptionsQuestionEventText = NothingChanged
            , _editOptionsQuestionEventRequiredPhaseUuid = NothingChanged
            , _editOptionsQuestionEventAnnotations = NothingChanged
            , _editOptionsQuestionEventTagUuids = NothingChanged
            , _editOptionsQuestionEventExpertUuids = NothingChanged
            , _editOptionsQuestionEventReferenceUuids = NothingChanged
            , _editOptionsQuestionEventAnswerUuids = ChangedValue []
            , _editOptionsQuestionEventCreatedAt = 2018 - 01 - 01 00 : 00 : 11 UTC
            }))
  , EditQuestionEvent'
      (EditValueQuestionEvent'
         (EditValueQuestionEvent
            { _editValueQuestionEventUuid = abfcd9db - 64 df - 44 c7 - 96 d4 - 135 d543a0013
            , _editValueQuestionEventParentUuid = 70 c0e4f3 - 7 a67 - 49 dd - 8043 - e504392d7903
            , _editValueQuestionEventEntityUuid = 1890 b807 - 83e8 - 4 a20 - 8515 - 83930 cab0001
            , _editValueQuestionEventTitle = NothingChanged
            , _editValueQuestionEventText = NothingChanged
            , _editValueQuestionEventRequiredPhaseUuid = NothingChanged
            , _editValueQuestionEventAnnotations = NothingChanged
            , _editValueQuestionEventTagUuids = NothingChanged
            , _editValueQuestionEventExpertUuids = NothingChanged
            , _editValueQuestionEventReferenceUuids = NothingChanged
            , _editValueQuestionEventValueType = NothingChanged
            , _editValueQuestionEventCreatedAt = 2018 - 01 - 01 00 : 00 : 12 UTC
            }))
  , EditReferenceEvent'
      (EditURLReferenceEvent'
         (EditURLReferenceEvent
            { _editURLReferenceEventUuid = abfcd9db - 64 df - 44 c7 - 96 d4 - 135 d543a0015
            , _editURLReferenceEventParentUuid = 70 c0e4f3 - 7 a67 - 49 dd - 8043 - e504392d7903
            , _editURLReferenceEventEntityUuid = 1890 b807 - 83e8 - 4 a20 - 8515 - 83930 cab0003
            , _editURLReferenceEventUrl = ChangedValue "Url"
            , _editURLReferenceEventLabel = NothingChanged
            , _editURLReferenceEventAnnotations = NothingChanged
            , _editURLReferenceEventCreatedAt = 2018 - 01 - 01 00 : 00 : 14 UTC
            }))
  , EditChoiceEvent'
      (EditChoiceEvent
         { _editChoiceEventUuid = abfcd9db - 64 df - 44 c7 - 96 d4 - 135 d543a0016
         , _editChoiceEventParentUuid = 70 c0e4f3 - 7 a67 - 49 dd - 8043 - e504392d7903
         , _editChoiceEventEntityUuid = 1890 b807 - 83e8 - 4 a20 - 8515 - 83930 cab0002
         , _editChoiceEventLabel = ChangedValue "Label 1 - 2"
         , _editChoiceEventAnnotations = NothingChanged
         , _editChoiceEventCreatedAt = 2018 - 01 - 01 00 : 00 : 15 UTC
         })
  , EditChoiceEvent'
      (EditChoiceEvent
         { _editChoiceEventUuid = 1e1 a97e6 - cea3 - 47 a8 - 9 de0 - 135 d543a0017
         , _editChoiceEventParentUuid = 70 c0e4f3 - 7 a67 - 49 dd - 8043 - e504392d7903
         , _editChoiceEventEntityUuid = 1890 b807 - 83e8 - 4 a20 - 8515 - 83930 cab0002
         , _editChoiceEventLabel = ChangedValue "Label 1 - 3"
         , _editChoiceEventAnnotations = NothingChanged
         , _editChoiceEventCreatedAt = 2018 - 01 - 02 00 : 00 : 01 UTC
         })
  ]
