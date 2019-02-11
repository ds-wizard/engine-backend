module Database.Migration.Development.Package.Data.Packages where

import Control.Lens ((^.))

import Database.Migration.Development.Event.Data.Events
import LensesConfig
import Model.Event.Event
import Service.Package.PackageMapper

baseElixir0PackageDto = buildPackage "Elixir Base Package" "elixir.base" "core" "0.0.1" "Beta version" Nothing []

baseElixirPackageDto =
  buildPackage
    "Elixir Base Package"
    "elixir.base"
    "core"
    "1.0.0"
    "First Release"
    Nothing
    [AddKnowledgeModelEvent' a_km1, AddTagEvent' a_km1_tds, AddTagEvent' a_km1_tbi]

elixirNlPackageDto =
  buildPackage
    "Elixir Netherlands"
    "elixir.nl"
    "core-nl"
    "1.0.0"
    "First Release"
    (Just $ baseElixirPackageDto ^. pId)
    [AddChapterEvent' a_km1_ch1]

elixirNlPackage2Dto =
  buildPackage
    "Elixir Netherlands"
    "elixir.nl"
    "core-nl"
    "2.0.0"
    "Second Release"
    (Just $ elixirNlPackageDto ^. pId)
    [AddChapterEvent' a_km1_ch3]

amsterdamPackage1Dto =
  buildPackage
    "Amsterdam KM"
    "elixir.nl.amsterdam"
    "amsterdam-km"
    "1.0.0"
    "First Release"
    (Just $ elixirNlPackageDto ^. pId)
    []

elixirCzPackage2Dto =
  buildPackage
    "Elixir Czech Republic"
    "elixir.cz"
    "core-cz"
    "1.0.0"
    "First Release"
    (Just $ baseElixir0PackageDto ^. pId)
    [ AddKnowledgeModelEvent' a_km1
    , AddTagEvent' a_km1_tds
    , AddTagEvent' a_km1_tbi
    , AddChapterEvent' a_km1_ch1
    , AddQuestionEvent' a_km1_ch1_q1
    , AddQuestionEvent' a_km1_ch1_q2
    , AddAnswerEvent' a_km1_ch1_q2_aNo1
    , AddAnswerEvent' a_km1_ch1_q2_aYes1
    , AddQuestionEvent' a_km1_ch1_ansYes1_fuq1
    , AddAnswerEvent' a_km1_ch1_q2_aYes1_fuq1_aNo
    , AddAnswerEvent' a_km1_ch1_q2_aYesFu1
    , AddQuestionEvent' a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2
    , AddAnswerEvent' a_km1_ch1_q2_aNoFu2
    , AddAnswerEvent' a_km1_ch1_q2_aYesFu2
    , AddExpertEvent' a_km1_ch1_q2_eAlbert
    , AddExpertEvent' a_km1_ch1_q2_eNikola
    , AddReferenceEvent' a_km1_ch1_q2_rCh1
    , AddReferenceEvent' a_km1_ch1_q2_rCh2
    , AddChapterEvent' a_km1_ch2
    , AddQuestionEvent' a_km1_ch2_q3
    , AddAnswerEvent' a_km1_ch2_q3_aNo2
    , AddAnswerEvent' a_km1_ch2_q3_aYes2
    , AddQuestionEvent' a_km1_ch2_q4
    , AddQuestionEvent' a_km1_ch2_q4_ait1_q5
    , AddQuestionEvent' a_km1_ch2_q4_ait1_q7
    , AddQuestionEvent' a_km1_ch2_q4_ait1_q8
    , AddQuestionEvent' a_km1_ch2_q4_ait1_q6
    , AddAnswerEvent' a_km1_ch2_q4_ait_q6_aNo
    , AddAnswerEvent' a_km1_ch2_q4_ait_q6_aYes
    , AddQuestionEvent' a_km1_ch2_ansYes6_fuq4
    , AddQuestionEvent' a_km1_ch2_q4_ait1_q6_fuq4_q1
    , AddQuestionEvent' a_km1_ch2_q4_ait1_q6_fuq4_q2
    , AddExpertEvent' a_km1_ch2_q6_eAlbert
    , AddExpertEvent' a_km1_ch2_q6_eNikola
    , AddReferenceEvent' a_km1_ch2_q6_rCh1
    , AddReferenceEvent' a_km1_ch2_q6_rCh2
    ]
