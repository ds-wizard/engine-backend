var MIGRATION_NAME = "0007_publicQuestionnaires"
logStartMigrationProcess()

// ------------------------------------------------------------------------
// CHANGES
// ------------------------------------------------------------------------

// Insert default organization
startChange("Insert publicQuestionnaire")
logInsert(PUBLIC_QUESTIONNAIRE_COL, "Insert public Questionnaire")
db.getCollection("publicQuestionnaires").insert({
    "uuid" : "e1de9b3a-4a40-4c1c-8606-6a6eb947c5d9",
    "name" : "Core Questionnaire",
    "packageId" : "elixir:root:1.0.0",
    "knowledgeModel" : {
        "uuid" : "00000000-eeee-eeee-eeee-111111111111",
        "name" : "DS-KM",
        "chapters" : [
            {
                "uuid" : "82fd0cce-2b41-423f-92ad-636d0872045c",
                "title" : "Design of experiment",
                "text" : "Before you decide to embark on any new study, it is nowadays good practice to consider all options to keep the data generation part of your study as limited as possible. It is not because we can generate massive amounts of data that we always need to do so. Creating data with public money is bringing with it the responsibility to treat those data well and (if potentially useful) make them available for re-use by others.",
                "questions" : [
                    {
                        "uuid" : "efc80cc8-8318-4f8c-acb7-dc1c60e491c1",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Is there any pre-existing data?",
                        "text" : "Are there any data sets available in the world that are relevant to your planned research?",
                        "answers" : [
                            {
                                "uuid" : "72cdbc99-2707-4817-ac40-435a03e5e837",
                                "label" : "No",
                                "advice" : "You know that this is very unlikely? This question is not only about data sets that are similar to what you want to determine yourself, but also reference data or data that should be mined from the existing literature. Further, it is very likely that you will refer to related data, e.g. other databases where you usually \"quickly look something up\", but that could maybe be properly integrated, especially if you need to do such lookups multiple times.",
                                "followUps" : []
                            },
                            {
                                "uuid" : "2663b978-5125-4224-9930-0a50dbe895c9",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "59efe0c4-fc18-4082-a656-6d0c1db45899",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you be using any pre-existing data (including other people's data)?",
                                        "text" : "Will you be referring to any earlier measured data, reference data, or data that should be mined from existing literature? Your own data as well as data from others?",
                                        "answers" : [
                                            {
                                                "uuid" : "18a860e6-4f77-459e-bfeb-24949924ac44",
                                                "label" : "No",
                                                "advice" : "Did you research all the data that exists? You may not be aware of all existing data that could be available. Although using and/or integrating existing data sets may pose a challenge, it will normally be cheaper than collecting everything yourself. Even if you decide not to use an existing data set, it is better to do this as a conscious decision.",
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "dac677d4-99f9-4af8-935a-4a1c9c7cdbdf",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "fcc51962-08df-4f4c-85ad-6bb932107010",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeList",
                                                        "title" : "What reference data will you use?",
                                                        "text" : "Much of todays data is used in comparison with reference data. A genome for instance is compared with a reference genome to identify genomic variants. If you use reference data, there are several other issues that you should consider. What are the reference data sets that you will use?",
                                                        "answers" : null,
                                                        "answerItemTemplate" : {
                                                            "title" : "Item",
                                                            "questions" : [
                                                                {
                                                                    "uuid" : "9ac2713c-6aa1-4884-a93f-2ac727d00567",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Do you know where and how is it available?",
                                                                    "text" : "Do you know where the reference data is available, what the conditions for use are, and how to reference it?",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "08bed2b3-fe76-48e5-a884-7f2658f90845",
                                                                            "label" : "No",
                                                                            "advice" : "Figure this out quickly!",
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "14a7b8cd-7edb-4dab-937b-36b42726fe6e",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [
                                                                        {
                                                                            "uuid" : "d22c0b62-90b0-437b-84e9-4b3604e2cd0a",
                                                                            "chapter" : "1.4"
                                                                        }
                                                                    ],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "7b8c75c0-8d5c-4fde-9898-ded8b91f7f1f",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Do you know in what format the reference data is available?",
                                                                    "text" : "Do you know the data format of the reference data? Is this suitable for your work? Does it need to be converted?",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "2c2a9b84-b140-4e17-8026-56d5372064d0",
                                                                            "label" : "I can directly use it",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "9de86d7b-9c60-47d6-8cb1-bfca13d6f619",
                                                                            "label" : "I need to convert it before using",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [
                                                                        {
                                                                            "uuid" : "49f413e1-9b44-4baf-91a0-37bfe1d3b708",
                                                                            "chapter" : "1.5"
                                                                        }
                                                                    ],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "10b6548c-21cd-44c8-a8de-47415e7b012e",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Is the reference data resource versioned?",
                                                                    "text" : "Many reference data sets evolve over time. If the reference data set changes, this may affect your results. If different versions of a reference data set exist, you need to establish your \"version policy\".",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "32c9e457-8289-4825-9334-83ff87adeb3f",
                                                                            "label" : "No",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "842007ce-0951-4655-8acc-b0f582e82ea1",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : [
                                                                                {
                                                                                    "uuid" : "f00a28fb-ac77-4110-b285-c73c8bc62630",
                                                                                    "shortUuid" : null,
                                                                                    "type" : "QuestionTypeText",
                                                                                    "title" : "Which version will you use?",
                                                                                    "text" : "If there are different versions available, you have to decide with all project partners together which version you will be using. Probably you will go for the latest release as of the date of the start of your research project. However, if you have other data from older projects that need to be merged, you may need to consider using the same release you used for a previous project.",
                                                                                    "answers" : null,
                                                                                    "answerItemTemplate" : null,
                                                                                    "references" : [],
                                                                                    "experts" : []
                                                                                },
                                                                                {
                                                                                    "uuid" : "481d0157-6d89-4f68-9f3b-cf1b05f8f15d",
                                                                                    "shortUuid" : null,
                                                                                    "type" : "QuestionTypeOptions",
                                                                                    "title" : "Will you change version if it updates?",
                                                                                    "text" : "If the reference changes while you are working on your research project, you need to decide whether you will follow these changes. Most likely that will mean that you have to do some analyses again, so you will need to make sure enough resources are available to do so. You can decide to stay with the version that you started with; this can have the disadvantage that you will not benefit from added information or added consistency.",
                                                                                    "answers" : [
                                                                                        {
                                                                                            "uuid" : "a4f6a1da-536b-43fa-9637-71e33327fa08",
                                                                                            "label" : "Will stay with the old version",
                                                                                            "advice" : null,
                                                                                            "followUps" : [
                                                                                                {
                                                                                                    "uuid" : "deb9d6b3-4f11-4aa8-b016-52d1612c002a",
                                                                                                    "shortUuid" : null,
                                                                                                    "type" : "QuestionTypeOptions",
                                                                                                    "title" : "How will the old version be available?",
                                                                                                    "text" : "Since you want to keep using the old version of the reference data, how will it be available to you?",
                                                                                                    "answers" : [
                                                                                                        {
                                                                                                            "uuid" : "bbef3d6c-7a90-4d5e-8d72-1fea083f6410",
                                                                                                            "label" : "I will need it only at the beginning",
                                                                                                            "advice" : null,
                                                                                                            "followUps" : []
                                                                                                        },
                                                                                                        {
                                                                                                            "uuid" : "e6ddd126-0878-4831-9450-8c5f3e9126dd",
                                                                                                            "label" : "I will use a downloaded version",
                                                                                                            "advice" : null,
                                                                                                            "followUps" : []
                                                                                                        },
                                                                                                        {
                                                                                                            "uuid" : "07f06e60-9d5b-46b9-832a-75b881c4a9cb",
                                                                                                            "label" : "The provider keeps old versions around",
                                                                                                            "advice" : null,
                                                                                                            "followUps" : []
                                                                                                        }
                                                                                                    ],
                                                                                                    "answerItemTemplate" : null,
                                                                                                    "references" : [],
                                                                                                    "experts" : []
                                                                                                }
                                                                                            ]
                                                                                        },
                                                                                        {
                                                                                            "uuid" : "069a0976-498c-4a47-b42f-33c6a1e40126",
                                                                                            "label" : "New analyses will be done with the new version",
                                                                                            "advice" : null,
                                                                                            "followUps" : []
                                                                                        },
                                                                                        {
                                                                                            "uuid" : "dc8dbe60-43e8-4ed4-ab11-13bb74aea820",
                                                                                            "label" : "All analyses will be redone with the new version",
                                                                                            "advice" : null,
                                                                                            "followUps" : []
                                                                                        }
                                                                                    ],
                                                                                    "answerItemTemplate" : null,
                                                                                    "references" : [],
                                                                                    "experts" : []
                                                                                }
                                                                            ]
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [
                                                                        {
                                                                            "uuid" : "d8ac2156-60d9-4420-8d0e-103762774656",
                                                                            "chapter" : "1.6"
                                                                        }
                                                                    ],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "12762d00-f02a-40f7-b175-c7e4c92f5f54",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "How will you make sure the same reference data will be available to reproduce your results?",
                                                                    "text" : "Will the reference data in the version you use be available to others?",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "9832a3a5-e24d-4db1-b48f-4d171c1402ae",
                                                                            "label" : "I will keep a copy and make it available with my results",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "b4d8d8b8-3c8e-4b41-87a4-dbd01e7b7ad9",
                                                                            "label" : "The provider keeps old versions around",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        },
                                                        "references" : [
                                                            {
                                                                "uuid" : "b04d8e87-9374-4581-b244-464c6721c38d",
                                                                "chapter" : "1.3"
                                                            }
                                                        ],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "be872000-cb98-442f-999c-ca3ef58dcfe8",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeList",
                                                        "title" : "What existing non-reference data sets will you use?",
                                                        "text" : "Even if you will be producing your own data, you often will also be relying on existing data sets (e.g. from earlier . You may need to integrate your new data with an existing data set or retrieve additional information from related data bases. Will you be doing such things?",
                                                        "answers" : null,
                                                        "answerItemTemplate" : {
                                                            "title" : "Item",
                                                            "questions" : [
                                                                {
                                                                    "uuid" : "39e0cc7c-17fe-4991-88d8-1280c3fe7923",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Will the owners of this data set work with you on this study",
                                                                    "text" : "",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "08fab0f9-20a7-4c22-9e7b-3a9fa81e49e9",
                                                                            "label" : "No",
                                                                            "advice" : null,
                                                                            "followUps" : [
                                                                                {
                                                                                    "uuid" : "f59d2d89-fe27-4a8a-b3e6-2782bcc2fb97",
                                                                                    "shortUuid" : null,
                                                                                    "type" : "QuestionTypeOptions",
                                                                                    "title" : "Do you need to request access to the data",
                                                                                    "text" : "",
                                                                                    "answers" : [
                                                                                        {
                                                                                            "uuid" : "d719a2b4-08e7-427e-9a8e-e4c6d827c0d4",
                                                                                            "label" : "No",
                                                                                            "advice" : null,
                                                                                            "followUps" : []
                                                                                        },
                                                                                        {
                                                                                            "uuid" : "ef89ca6e-3059-4821-995a-034cdb3cf48d",
                                                                                            "label" : "Yes",
                                                                                            "advice" : null,
                                                                                            "followUps" : []
                                                                                        }
                                                                                    ],
                                                                                    "answerItemTemplate" : null,
                                                                                    "references" : [],
                                                                                    "experts" : []
                                                                                }
                                                                            ]
                                                                        },
                                                                        {
                                                                            "uuid" : "b0384f48-668f-4be5-86cc-e3fec81bcdb7",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "08b6593f-60a2-498a-9c20-eeb55568f8c1",
                                                                            "label" : "We are the owners",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [
                                                                        {
                                                                            "uuid" : "c8d9b722-3dcb-488e-8443-9880881faf82",
                                                                            "chapter" : "1.8"
                                                                        }
                                                                    ],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "27c83b38-1df2-4f91-89ac-2f6bf4e72190",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Will any usage restrictions affect your reuse?",
                                                                    "text" : "",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "7dc57afd-66cc-4760-8f37-eb0db7bb1a96",
                                                                            "label" : "No",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "525459fb-182b-4ebe-9558-7b3b3679dcd5",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "467ff3ad-156c-4285-9efb-c91072b3544e",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Is extension of any consent for privacy sensitive data needed?",
                                                                    "text" : "If the data that you will re-use is coupled to people, is the informed consent that was originally obtained from those people covering your current research?",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "6a3e4ed2-60cf-47b5-b6e5-1cf26655e251",
                                                                            "label" : "Not applicable",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "1d732f5b-e532-4672-9f67-4aa4e4eef449",
                                                                            "label" : "Existing consent applies",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "e29f35b6-c138-401a-b3d8-9cdcff0b8223",
                                                                            "label" : "New consent needed",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [
                                                                        {
                                                                            "uuid" : "b2274290-d7cd-4114-8eb7-78c14677e96e",
                                                                            "chapter" : "1.9"
                                                                        }
                                                                    ],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "62ddfc73-1680-4bbf-b198-6a7231875fb1",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "How will you be accessing the data?",
                                                                    "text" : "",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "423f7c77-c3bd-4ece-a9f6-ad9e3d0428a1",
                                                                            "label" : "Already have a copy",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "4e183e8d-d4bd-442a-a663-d3accf4f9217",
                                                                            "label" : "Will download or get a copy",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "f5e1c4d3-79f7-4950-9b10-6a7738f21978",
                                                                            "label" : "Will use it online",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "588e3c1f-e9fd-411f-954e-f7a9b1cff1de",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Do you know in what format the data is available?",
                                                                    "text" : "Do you know the data format of the data? Is this suitable for your work? Does it need to be converted?",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "4831f187-8660-4641-9a32-a6f4f569614f",
                                                                            "label" : "I can directly use it",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "3da33836-eff3-4166-898b-3c2c3d3989a8",
                                                                            "label" : "I need to convert it before using",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "3b088f38-81bd-46f9-8066-90513740b662",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Is the data set fixed, or will it be updated in the future?",
                                                                    "text" : "Is the data set you will reuse a fixed data set (with a persistent identifier), or is it a data set that is being worked on (by others) and may be updated during your project or after?",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "aff2edcd-4d18-4311-9a03-3b6b676c9277",
                                                                            "label" : "It is a fixed data set, this will not influence reproducibility of my results",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "d700715e-4ae0-45ed-a1d3-4d79bb32155f",
                                                                            "label" : "It may change",
                                                                            "advice" : "Make sure a copy of the version you use will be available with my results",
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "2e7af465-4852-4411-8d8e-7f0ff64d8b25",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Can you and will you use the complete existing data set?",
                                                                    "text" : "If you use any filtering, how will you make sure that your results will be reproducible by yourself and others at a later time?",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "2076afb5-3c5c-4f42-8f77-9b78fec76a62",
                                                                            "label" : "I will use the complete data set",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "f748624d-8d28-4d07-91c7-28b33db1fabd",
                                                                            "label" : "Any filtering or selection will be well documented",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "cba7c977-986d-4673-9bbc-fb3f7fed95c6",
                                                                            "label" : "I will make sure the selected subset will be available together with my results",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        },
                                                        "references" : [
                                                            {
                                                                "uuid" : "ad40d357-7fba-4f50-8286-6565b180098f",
                                                                "chapter" : "1.7"
                                                            }
                                                        ],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "5b283886-089b-44cf-a01b-e68800fd92ae",
                                                "chapter" : "1.2"
                                            }
                                        ],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [
                            {
                                "uuid" : "e7b9bf05-a77d-46f1-a5ce-bfae587c1c38",
                                "chapter" : "1.1"
                            }
                        ],
                        "experts" : []
                    },
                    {
                        "uuid" : "588ad032-56ba-4d52-b29c-6a5b56aa6569",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will reference data be created?",
                        "text" : "Will any of the data that you will be creating form a reference data set for future research (by others)?",
                        "answers" : [
                            {
                                "uuid" : "1e28b25b-bfe8-4d67-9643-6e57587a9495",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "bdf55011-db06-4f4a-b26f-ebbbe029da04",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "9f081091-0810-4f12-be7f-0a22ee928a14",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeText",
                                        "title" : "What will the Intellectual Property be like?",
                                        "text" : "Who will own the rights to the reference data set? Who will be able to use it?",
                                        "answers" : null,
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "e7f94c40-7295-40df-8bbd-f115b052b6c0",
                                                "chapter" : "1.12.1"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "27769a31-717b-4204-8468-175ac93b195a",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeText",
                                        "title" : "How will you maintain it?",
                                        "text" : "How will maintenance be paid for in the long run? Will you host it yourself or deposit it with a repository? How will you deal with requests for help? And with requests for adding data?",
                                        "answers" : null,
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "14efd439-8beb-4480-b3b1-fdf18bd5a154",
                                                "chapter" : "1.12.2"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "2679736a-800e-4eaa-a70f-818070540c48",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeText",
                                        "title" : "How will the release schedule be?",
                                        "text" : "Will you be updating the reference data at regular intervals?",
                                        "answers" : null,
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [
                            {
                                "uuid" : "b4c4132e-23e2-4302-aa6d-bf29804899e7",
                                "chapter" : "1.12"
                            }
                        ],
                        "experts" : []
                    },
                    {
                        "uuid" : "0e810343-6026-4992-8ea0-5539cbc77093",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you be storing samples?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "bd40f22d-6bce-464c-b525-e7813762a006",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "05873934-f03d-4d90-8bfd-291350c14673",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [
                            {
                                "uuid" : "21fc0d1e-bde3-4855-8f3f-0fa9454e2fa0",
                                "chapter" : "1.13"
                            }
                        ],
                        "experts" : []
                    },
                    {
                        "uuid" : "8d082e2f-7792-4ef7-a851-6e5041cba8b6",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you be collecting experimental data?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "16003c9d-4639-46ab-a305-df528f58d1c5",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "640a6f6f-6bce-4847-892c-57ae459bff1d",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [
                            {
                                "uuid" : "7eea813e-195f-4843-9738-c7fd087bfcc8",
                                "chapter" : "1.14"
                            }
                        ],
                        "experts" : []
                    }
                ]
            },
            {
                "uuid" : "c4eda690-066f-495a-8c29-8e8a258ac487",
                "title" : "Data Capture/Measurement",
                "text" : "",
                "questions" : [
                    {
                        "uuid" : "f5fef09d-ade5-4019-b089-f05bd89c34bc",
                        "shortUuid" : null,
                        "type" : "QuestionTypeList",
                        "title" : "Please specify what data sets you will acquire using measurement equipment",
                        "text" : "",
                        "answers" : null,
                        "answerItemTemplate" : {
                            "title" : "Item",
                            "questions" : [
                                {
                                    "uuid" : "ee59664e-4026-4796-a42a-e8003df6dadf",
                                    "shortUuid" : null,
                                    "type" : "QuestionTypeOptions",
                                    "title" : "Who will do the measurements? And where?",
                                    "text" : "Are there easily accessible specialized service providers for data capture?",
                                    "answers" : [
                                        {
                                            "uuid" : "44ef09a4-5a78-439d-9fb4-940d3d84bfab",
                                            "label" : "Experts in the project, with our own equipment",
                                            "advice" : null,
                                            "followUps" : []
                                        },
                                        {
                                            "uuid" : "eb0b9a82-0379-47c3-967f-b35b40129af2",
                                            "label" : "Experts in the project, at a specialized infrastructure",
                                            "advice" : null,
                                            "followUps" : []
                                        },
                                        {
                                            "uuid" : "f2784b7e-b0ed-4973-9d68-954c5c24e8bb",
                                            "label" : "External party",
                                            "advice" : "consider making them partner in the project",
                                            "followUps" : [
                                                {
                                                    "uuid" : "aa27852a-00f4-44a4-a85c-0e0fd3ac20d1",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "Has formal ownership of the data been established?",
                                                    "text" : "",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "786edd0d-ae07-489c-b078-423c18e02043",
                                                            "label" : "The party measuring the data owns it",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "306f7ef4-97fd-47b0-84de-f192cb94ea3a",
                                                            "label" : "The project partners acquire full ownership",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "399245d4-7988-461c-b940-ee813a1c99b1",
                                                            "label" : "We have made other arrangements",
                                                            "advice" : null,
                                                            "followUps" : [
                                                                {
                                                                    "uuid" : "227f056a-ada1-473b-af71-bb63d48b4940",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeText",
                                                                    "title" : "What other arrangements?",
                                                                    "text" : "",
                                                                    "answers" : null,
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        },
                                                        {
                                                            "uuid" : "840b561f-3e8f-435e-a1f2-68dc75dc4f22",
                                                            "label" : "This still needs to be decided",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                },
                                                {
                                                    "uuid" : "f0a474ac-21b5-4b6a-9fd5-b9c3f8882055",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "Has responsibility for long term safe keeping of the raw data been established? Who will deal with data publication?",
                                                    "text" : "",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "b10cd3bf-ecd6-4fa1-a9c4-d8d974de714c",
                                                            "label" : "The measuring party will publish it after a fixed embargo period",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "a3393cc1-3393-45ba-ba91-e447bf755eb7",
                                                            "label" : "The measuring party will publish it when the project tells them to",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "350fa8cc-ecc8-40cc-a50e-1d87405970b1",
                                                            "label" : "The measuring party will delete the data, the project partners will deal with data publication",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "8197cf1c-999a-43ae-9e29-bb5d0452052b",
                                                            "label" : "We have made other arrangements",
                                                            "advice" : null,
                                                            "followUps" : [
                                                                {
                                                                    "uuid" : "e576471a-40dd-453d-ba59-d3f4e1c30cc4",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeText",
                                                                    "title" : "What other arrangements?",
                                                                    "text" : "",
                                                                    "answers" : null,
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        },
                                                        {
                                                            "uuid" : "2fa2df9a-5dee-496c-9aa3-24dba3bf2b80",
                                                            "label" : "This still needs to be decided",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                }
                                            ]
                                        }
                                    ],
                                    "answerItemTemplate" : null,
                                    "references" : [],
                                    "experts" : []
                                },
                                {
                                    "uuid" : "bce52f7f-2256-4afc-9801-2e4da8111470",
                                    "shortUuid" : null,
                                    "type" : "QuestionTypeOptions",
                                    "title" : "Is the equipment completely standard and well described?",
                                    "text" : "If the technology is very much under development, you may want to come back later to understand exactly how the measurements have been made. Is the measurement equipment and protocol sufficiently standard that you will be able to explain how it is done or refer to a standard explanation?",
                                    "answers" : [
                                        {
                                            "uuid" : "cba473be-2489-4387-8877-ce49cf95a304",
                                            "label" : "Very well described and known",
                                            "advice" : null,
                                            "followUps" : []
                                        },
                                        {
                                            "uuid" : "526db9ec-a5a1-4431-9cc3-751fc6625217",
                                            "label" : "Less well described or not completely standard",
                                            "advice" : "Take pictures of the instruments for documentation, and keep copies of any documentation.",
                                            "followUps" : []
                                        }
                                    ],
                                    "answerItemTemplate" : null,
                                    "references" : [],
                                    "experts" : []
                                },
                                {
                                    "uuid" : "28b99b8e-3a0c-45c5-8d2b-b778a7d43323",
                                    "shortUuid" : null,
                                    "type" : "QuestionTypeOptions",
                                    "title" : "Is special care needed to get the raw data ready for processing?",
                                    "text" : "Where does the data come from? And who will need it? Sometimes the raw data is measured somewhere else than where the primary processing is taking place. In such cases the ingestion or transport of the primary data may take special planning. You also need to make sure that data is secure and that data integrity is guaranteed.",
                                    "answers" : [
                                        {
                                            "uuid" : "b541e78e-3300-4e7d-ac36-93d7b5ebf031",
                                            "label" : "No, this is all fine",
                                            "advice" : null,
                                            "followUps" : []
                                        },
                                        {
                                            "uuid" : "065a500b-53c0-487f-9096-00fd6f7234a6",
                                            "label" : "Yes, lets explore this",
                                            "advice" : null,
                                            "followUps" : [
                                                {
                                                    "uuid" : "2c5fd209-1ff5-4c34-b6e4-2bf1228e97de",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "Is the data format established?",
                                                    "text" : "Has the storage and transport format of the primary data been established between the people responsible for the measurement and the people responsible for the processing?",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "856438ef-024b-4e82-b8ba-0010beb39f61",
                                                            "label" : "No",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "abc6b715-2d2b-4ee8-81c5-ac2c55d1cac2",
                                                            "label" : "Yes",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                },
                                                {
                                                    "uuid" : "2e8d6e55-36ea-46eb-a921-65e550bce5dc",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "How will the raw data be transported?",
                                                    "text" : "",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "489c3813-c010-4197-b54a-41e742aa8f35",
                                                            "label" : "No transport is needed, analysis is done where it is obtained",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "8c9c3d59-cd51-4303-9202-519e30998e1a",
                                                            "label" : "On physical media",
                                                            "advice" : null,
                                                            "followUps" : [
                                                                {
                                                                    "uuid" : "31da89a7-4d04-4345-a414-ea1226377df7",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Is sufficient writing and reading capacity available for the physical media?",
                                                                    "text" : "Has time been reserved on any tape/disk reading and writing stations?",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "5c3c64b1-fdbd-4267-9b24-dc59456a3828",
                                                                            "label" : "Not yet",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "be674d81-cadb-470a-acf9-7365175b533d",
                                                                            "label" : "Yes, has been taken care of",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        },
                                                        {
                                                            "uuid" : "ca02196a-aae3-42aa-b6cb-aeb1b86f752a",
                                                            "label" : "Via the network",
                                                            "advice" : null,
                                                            "followUps" : [
                                                                {
                                                                    "uuid" : "3d942d91-39a2-4d2f-a27a-6fe57deb87ff",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Is sufficient network capacity available?",
                                                                    "text" : "Can the volume of data be accommodated by the standard network connection? Has a special network connection (e.g. light path) that is needed been reserved?",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "b7a228a3-7555-4572-b753-6383ddb5fd1a",
                                                                            "label" : "Not yet",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "44dfd6cb-b3ce-41a4-bca8-9689be8df67c",
                                                                            "label" : "Yes, has been taken care of",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                },
                                                {
                                                    "uuid" : "129f74a4-dc91-4cc7-aee3-b384c8ea30dd",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "Is data integrity guaranteed during this stage?",
                                                    "text" : "Do you have any means of identifying whether the raw data has been transferred error free and has not been tampered with?",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "faafb22b-bebe-4dc1-adf7-2d398d5d52de",
                                                            "label" : "No",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "5935eebc-017c-4b4c-b44f-02ba0e614eca",
                                                            "label" : "Yes",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                },
                                                {
                                                    "uuid" : "0ebd660b-364a-4357-9044-c4dc0293a9ba",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "Is data security guaranteed during this stage?",
                                                    "text" : "Are the raw data encrypted or otherwise protected from theft or leaks at either site or during transport? You could e.g. use a light path or a virtual private network if you transport the data over the net.",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "c30318bd-f2ca-4328-ae27-a14870916543",
                                                            "label" : "No, not needed",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "a87f2a11-94c5-43cf-acc4-30dc54a1a41b",
                                                            "label" : "Yes",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                }
                                            ]
                                        }
                                    ],
                                    "answerItemTemplate" : null,
                                    "references" : [],
                                    "experts" : []
                                }
                            ]
                        },
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "f038bd46-ee4e-4f53-b7ea-482381c2c855",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Do you have any non-equipment data capture?",
                        "text" : "Does the data you collect contain non-equipment captured data such as questionnaires, case report forms, electronic patient records?",
                        "answers" : [
                            {
                                "uuid" : "514befe5-76b0-4e4e-bdf5-2bc164e38d7f",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "4fd89b13-f33c-4858-8b25-ab6da271efc6",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "85079340-7b80-4dc7-86ae-cc5f599ec737",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you be collecting questionnaires?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "000f21de-fdf6-4c9d-9bfb-847db544613b",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "81baba8e-d43c-48f9-b39a-c0adfad08f64",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "36a5ed1e-ecd5-4b86-a719-f1196e376a52",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you be collecting case report forms?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "3e3d559c-5fbf-4c43-b1f5-763e84fc9c78",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "7fcce8f6-e52b-444e-88f6-74ba3e4ee37b",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "a1f33a26-8caa-4b68-b3f7-0b5d5a659e68",
                                                "chapter" : "3.2.1.1"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "7e456b72-72a1-427d-8e75-9da096bc9806",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you be collecting data from electronic patient records?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "1e097cb9-7d94-4bc6-a261-f78f8208f9f3",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "2d3d08b8-7d63-4645-9499-f465caa06204",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "4eb1bd6f-5d10-4b0a-aab1-715facc29cbb",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Has access to the electronic patient records been arranged?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "03f16c20-4299-4511-a893-79da17a16b50",
                                                                "label" : "Not yet",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "5af68c53-0156-4290-972f-a86a09d36232",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "b642c31d-a512-4ca7-8743-a2e0254006fa",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeList",
                                        "title" : "Please list all non-equipment data sets you will collect",
                                        "text" : "",
                                        "answers" : null,
                                        "answerItemTemplate" : {
                                            "title" : "Item",
                                            "questions" : [
                                                {
                                                    "uuid" : "4e84a44f-e66b-42f5-bb42-7e6c4bd3b79b",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "How will the data be captured?",
                                                    "text" : "",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "407560bc-705a-4885-85d6-4f61fe58eeed",
                                                            "label" : "All immediately in digital form",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "374974a6-2c51-4508-aa93-9ef8f1ef65fd",
                                                            "label" : "Data will first be on paper",
                                                            "advice" : null,
                                                            "followUps" : [
                                                                {
                                                                    "uuid" : "32f5bc5c-6bc6-4df0-849f-1619951c5b98",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Who will do the data entry?",
                                                                    "text" : "",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "9d51aae1-ca1b-4267-b0df-d43dc85f5bc6",
                                                                            "label" : "A specialized team",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "9660950a-41d7-4c3b-a6fd-97e58c8fa412",
                                                                            "label" : "One of the members of the project team",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "82ef3ee8-461e-4f5b-a66a-2d4aaccbe68b",
                                                                            "label" : "We have made other arrangements",
                                                                            "advice" : null,
                                                                            "followUps" : [
                                                                                {
                                                                                    "uuid" : "f0e57014-79ad-4f2d-becd-e00f50ed1770",
                                                                                    "shortUuid" : null,
                                                                                    "type" : "QuestionTypeText",
                                                                                    "title" : "What other arrangements?",
                                                                                    "text" : "",
                                                                                    "answers" : null,
                                                                                    "answerItemTemplate" : null,
                                                                                    "references" : [],
                                                                                    "experts" : []
                                                                                }
                                                                            ]
                                                                        },
                                                                        {
                                                                            "uuid" : "aebe403b-e350-474e-94fb-d210d15e8b4d",
                                                                            "label" : "This is not yet clear",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "a205f751-cbb6-4f58-a692-2ba450e1e133",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Did you arrange who will make the data digitally available to you?",
                                                                    "text" : "",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "1c6da256-72d5-45b1-842e-c1458e6d2e21",
                                                                            "label" : "Not yet",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "83d23b9f-4b91-4444-8e38-f1c7982e41aa",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                },
                                                {
                                                    "uuid" : "36d40d39-894b-4c1a-9e9c-feaddef09df3",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "Will all data be collected by a single person?",
                                                    "text" : "",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "4ea74c15-c1d2-46c1-9bd6-8fe251648a4e",
                                                            "label" : "This data will be collected by one person",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "d988687b-41e9-4be4-8472-031938d13c37",
                                                            "label" : "More people will capture data",
                                                            "advice" : null,
                                                            "followUps" : [
                                                                {
                                                                    "uuid" : "68956a97-6138-4538-9968-e7a925f299cc",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Is there a risk of different interpretations? Subjectivity?",
                                                                    "text" : "",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "2a1261d5-6678-4181-8ab9-b9671546a02a",
                                                                            "label" : "The data capture is objective",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "c4443193-11f5-4a9e-9444-fabf575d9246",
                                                                            "label" : "There are internal controls to deal with interperation differences",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "2dc09f04-0613-45e5-bba9-18ffe4db8a82",
                                                                            "label" : "We accept the risk of different interpretations",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "8bf15202-b0dc-4be7-8093-72349c7b65c9",
                                                                            "label" : "We do validation afterwards",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "17443e43-bc5a-49ce-8197-6997e9cf3d41",
                                                                            "label" : "We have made other arrangements",
                                                                            "advice" : null,
                                                                            "followUps" : [
                                                                                {
                                                                                    "uuid" : "ef139df4-fb76-442e-a617-58a638c428f9",
                                                                                    "shortUuid" : null,
                                                                                    "type" : "QuestionTypeText",
                                                                                    "title" : "What other arrangements?",
                                                                                    "text" : "",
                                                                                    "answers" : null,
                                                                                    "answerItemTemplate" : null,
                                                                                    "references" : [],
                                                                                    "experts" : []
                                                                                }
                                                                            ]
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                }
                                            ]
                                        },
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [
                            {
                                "uuid" : "a18889af-00d0-40ff-8f4a-a2ae19664687",
                                "chapter" : "3.2.1"
                            }
                        ],
                        "experts" : []
                    },
                    {
                        "uuid" : "ab4b3f39-dfab-45a5-9489-2d46ceacbb73",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Is there a data integration tool that can handle and combine all the data types you are dealing with in your project?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "9e5abf02-c19d-4826-a1f2-6db5acf471a6",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "20a9685a-5560-43dc-bb13-73c434f8d666",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Can all data be brought into the same format, e.g. RDF?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "6c382760-fc18-47e5-9bd3-91747a24bd83",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "2d5c8925-3b78-4cdf-be9a-e66351b123fc",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            },
                            {
                                "uuid" : "b24b7cbe-2209-47e9-87d6-401fbfb3cdec",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "2e394765-d87d-4c1e-9f90-d8d2a06826e3",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "What software will you be using to collect all data?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "ffcb476b-8321-42d6-84fb-8347fcff1011",
                                                "label" : "We will not use a data warehouse",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "cce63dc5-5ac7-4706-ab5a-4a204ef380e3",
                                                "label" : "TranSMART",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "aa23e0c5-45ca-4061-9e81-04fc9d56c97c",
                                                "label" : "Software not listed here",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "2be7a50f-f044-4649-a163-2197a13e5edc",
                                                "label" : "We will develop our own",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "677c07f0-a709-4d77-946f-78735a88c858",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Why will you develop your own software?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "2bfd3b78-6013-42b3-ba13-51041aac5618",
                                                                "label" : "Nothing like this has ever been done before",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "62028d39-f35d-4b09-ad7a-a0772d45573e",
                                                                "label" : "Existing solutions are not suitable",
                                                                "advice" : "Did you consider collaborating to improve an existing solution?",
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    }
                ]
            },
            {
                "uuid" : "10a10ffd-bfe1-4c6b-bbb6-3dfb1e63a5d5",
                "title" : "Data processing and curation",
                "text" : "",
                "questions" : [
                    {
                        "uuid" : "4ba3304e-225c-4916-bbf7-754de381253c",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Workflow development",
                        "text" : "It is likely that you will be developing or modifying the workflow for data processing. There are a lot of aspects of this workflow that can play a role in your data management, such as the use of an existing work flow engine, the use of existing software vs development of new components, and whether every run needs human intervention or whether all data processing can be run in bulk once the work flow has been defined.",
                        "answers" : [
                            {
                                "uuid" : "4d74014e-588d-4a1d-a343-9bc3a4d72e25",
                                "label" : "This has been arranged",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "bb00eb32-e1c2-4288-a874-02264f757cb3",
                                "label" : "More guidance is desired",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "13c2a033-423c-4bb3-b596-840de2f6c9c9",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you be exploring options, or run in bulk?",
                                        "text" : "What will be the operational mode for your workflows? Will you be exploring options by changing tools and tweaking parameters? Of will you be running the same exact workflow on a large number of data files?",
                                        "answers" : [
                                            {
                                                "uuid" : "ec5b49f0-2a91-47fa-aee2-9636b8f9f2d0",
                                                "label" : "We will be exploring options",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "a91af528-0ab7-4b23-8b27-b131a7731dce",
                                                "label" : "We will be running in bulk",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "f357ae1a-6563-48dd-add2-580e1f1a565d",
                                                "label" : "A bit of both",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "962ce8ee-86b2-40c3-8844-cf20a50f295e",
                                                "chapter" : "4.1.1"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "14ed71a8-80c6-465a-94db-1115502297bd",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "What data will the workflow developers or implementers use?",
                                        "text" : "The people implementing the data analysis work flow for your project probably need test data that they can use to see whether what they build works. How will this be arranged?",
                                        "answers" : [
                                            {
                                                "uuid" : "e8779d3b-8292-429e-a9b1-3df49c495674",
                                                "label" : "They do not need data",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "9d7f9c48-0907-4798-b7bd-454505a54b1a",
                                                "label" : "They can use existing data from another project",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "3996826a-b67a-42e7-9223-f2a438fddbe6",
                                                "label" : "They can use data from our project",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "c56f2e8f-bb78-412e-b238-974a14482f28",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "When will they have access?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "94f0f330-54fd-425f-a644-b8a7c674026f",
                                                                "label" : "Data will be available at the start of the project, no waiting needed",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "4844d32a-be6a-490a-b12e-3a31503d7bd1",
                                                                "label" : "They can start with a subset as soon as our first data comes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "e5bc08e1-815f-44f6-94ea-3a32e1808d51",
                                                                "label" : "Workflow development will commence as soon as our data is available",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "d206fcaf-a238-48ef-925e-6f006d39f2af",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "How will data serurity be dealt with?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "01a9f738-7950-4ee0-b9d0-ecd0df8fa7f2",
                                                                "label" : "Data security is not an issue",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "2359684e-be7f-4990-8f2e-84079ceb48c3",
                                                                "label" : "Data will not be on systems owned by workflow developers",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "340ae978-4435-4b20-87bf-0d65d82ac7b2",
                                                                "label" : "Data will always be encrypted",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "a3286c6a-ce1b-4ff0-8cee-9686d0bd1e72",
                                                                "label" : "We have made other arrangements",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "4bf93fbc-1116-4d40-b2fb-625a0e2e20f9",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeText",
                                                                        "title" : "What other arrangements?",
                                                                        "text" : "",
                                                                        "answers" : null,
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "9a1c0153-3eae-40d5-8e91-66e108ef1fe8",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeList",
                                        "title" : "List existing software components you will use in the analysis workflow",
                                        "text" : "Your workflow may be available in components from different sources. Specify the different parts that you recognize and that you will each acquire in a different way",
                                        "answers" : null,
                                        "answerItemTemplate" : {
                                            "title" : "Item",
                                            "questions" : [
                                                {
                                                    "uuid" : "996df4e8-90ea-4aaf-8c3d-d92b3d145dd3",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeText",
                                                    "title" : "Where are you getting this software from? Please specify a web address if available.",
                                                    "text" : "",
                                                    "answers" : null,
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                },
                                                {
                                                    "uuid" : "cfb67fc9-4a65-4221-87ca-7ca7c99f13a4",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "What version of this software will you use?",
                                                    "text" : "",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "d707aef1-856d-4ab1-903a-2bccdd0c9a65",
                                                            "label" : "The exact version that we point to",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "6730b8b5-7abe-41e5-b97c-08e1f32fb523",
                                                            "label" : "The current version, and we will stick with that",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "e3e4240a-0785-4a76-9dd6-ce073abf643b",
                                                            "label" : "Whatever is the latest version at the time the analysis is run",
                                                            "advice" : null,
                                                            "followUps" : [
                                                                {
                                                                    "uuid" : "b482e4e9-9435-4e2c-9ce5-e35845ba53b0",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Will you re-run any analysis when a new version comes out?",
                                                                    "text" : "",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "8a3adc2b-cef2-43a1-8552-644acc3939a7",
                                                                            "label" : "No",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "de969453-db1e-48b3-9024-6ed9fc23152f",
                                                                            "label" : "If there is potentially a better result",
                                                                            "advice" : "Make sure to take that into account when calculating the need for compute resources",
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "7e0cd029-7588-4a94-9bfc-506dda26066d",
                                                                            "label" : "Yes",
                                                                            "advice" : "Make sure to take that into account when calculating the need for compute resources",
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                },
                                                {
                                                    "uuid" : "b66496d0-6fd4-4854-8f07-961045de4b7c",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "How is your experience with this software?",
                                                    "text" : "",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "30aedfab-3199-43b8-bcc6-caffad6148f0",
                                                            "label" : "We know the software well",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "4140dd2d-aeb0-4cb8-960b-b46e74305dd5",
                                                            "label" : "We know the authors well",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "e97a9384-85b6-464a-9d06-3a22f29a4465",
                                                            "label" : "We will gather experience by visiting others who have used it",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "d1a1cac8-a82c-4ef4-8f25-7dc196bdcf70",
                                                            "label" : "We will use the manual",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                }
                                            ]
                                        },
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "2109e98d-fe60-412d-8e72-48b47a181627",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeList",
                                        "title" : "List new software components you will develop for the analysis workflow",
                                        "text" : "Not all components you need may be available already. Please list here what you will be developing yourself. Do not underestimate the time needed to integrate components into a work flow!",
                                        "answers" : null,
                                        "answerItemTemplate" : {
                                            "title" : "Item",
                                            "questions" : [
                                                {
                                                    "uuid" : "dbf05c2a-0108-4cae-947d-c0383140f006",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeText",
                                                    "title" : "Please specify the software repository you use for development",
                                                    "text" : "Preferably use a direct URL other users could use",
                                                    "answers" : null,
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                },
                                                {
                                                    "uuid" : "99f4a43c-9848-488a-9b99-ff8dd6a4269c",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "Did you consider existing options?",
                                                    "text" : "",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "1f53a431-ec8d-4a16-875a-c2f7843a2294",
                                                            "label" : "Nothing like this exists",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "37f92ce2-48e9-4437-bb34-b034b69ad4a2",
                                                            "label" : "Authors of similar alternatives are not willing to work with us",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "45aa7af4-137a-4db6-abd7-2260e32f40a5",
                                                            "label" : "Alternatives exist, but we prefer to develop our own",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "f43fbe42-2e6c-4c96-a66d-a717c68e63ec",
                                                            "label" : "This will be one of the prime distinctive outcomes of our project",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                },
                                                {
                                                    "uuid" : "3b0f752e-e46b-4c8b-835f-14c426036ece",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "What license will you use for your tool?",
                                                    "text" : "Make sure the license is compatible with all components you use, and also make sure the license is made explicit in the repository.",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "8d3c3ffa-f194-4930-8cc5-ffa1f8f77fd7",
                                                            "label" : "Apache 2.0",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "7e87b33a-4ba9-494a-b2f0-62ac6ad62ae8",
                                                            "label" : "GPL 3.0 or later",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "5e5babe4-a37d-449e-9929-887614d55a3a",
                                                            "label" : "AGPL 3.0 or later",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "7785de46-4011-43b0-8b90-3a088c829d2c",
                                                            "label" : "LGPL 3.0 or later",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "584339ee-b313-44ef-8aba-c0f252eb2ade",
                                                            "label" : "We are using a different license",
                                                            "advice" : null,
                                                            "followUps" : [
                                                                {
                                                                    "uuid" : "6b5d7d2e-c117-4e61-a597-5207bc7fc057",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeText",
                                                                    "title" : "What other license?",
                                                                    "text" : "Preferably give a web pointer to the license.",
                                                                    "answers" : null,
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "c83097bb-e057-4cde-848d-a2e57d6ee3c9",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Is this a recognised reusable open source license?",
                                                                    "text" : "",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "aefb5f4a-dd6e-44a1-8834-a064a4534196",
                                                                            "label" : "No",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "d67fb3b1-3f11-481b-bf2c-ff4302bc5326",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                }
                                            ]
                                        },
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "a1c37c05-57ff-499c-b58c-e90f511241fa",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Did you choose the workflow engine you will be using?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "e856c626-bf9e-4550-997c-c6d0f66205e4",
                                                "label" : "Yes, we will be using what we always use",
                                                "advice" : "Make sure that you are not missing out on alternatives that would have better properties for the project",
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "7626c84b-1b2a-4809-99d1-da4ad42f3c9b",
                                                "label" : "Yes, we have selected one based on what features we need",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "e9e91c82-a5b5-4766-a643-b00a68c37d10",
                                                "label" : "Not yet, we will",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "00842b33-a854-4e25-8bed-05e2462fecf1",
                                                "label" : "Not yet, please guide the choice with more questions on this topic",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "28b10fd8-28a6-49b8-822d-f3d47780ab20",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Do you need the workflow engine to produce provenance information automatically?",
                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "25b6c0b3-b957-45d9-b657-7b1ccbbfbbb0",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "1693bb64-b047-4464-ae45-c5e8aca12263",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "15e6148f-f4e0-4bbc-93c5-6fdf4fe47608",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Does the provenance need to be stored or converted to some standard format?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "275de315-eda7-408d-b69c-47a15c678404",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "e1482f60-44dd-4179-b464-d0f66a3dbb1a",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "ba0000c2-65e0-4af3-95c3-bdcc1d3de291",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Can the work flow be annotated to make it understandable?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "e3241ef2-9716-46f7-a1f4-4ae3c3446c6f",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "fadd9cac-c327-447f-b490-c9c295dd18b5",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "ff6f53f1-1adc-4401-bbda-a159d283a8eb",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Do you need the workflow engine to be run high-throughput?",
                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "a4f6d057-f24c-4e84-bb82-0a4dd159af5c",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "f0c9bb01-a3f4-4ea9-82b6-ccc7a1feb0a9",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "fdeb19c8-393c-4c4e-8c88-58a9c4d19e2b",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Is ease of development of the workflow engine itself an issue for you?",
                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "95c45541-8d6d-4c40-93d2-005baa32b729",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "40eb8be9-1726-4cf5-b9c6-1adbb5cbccd1",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "2122d8b6-ab79-4a20-9016-d2ab8d154a58",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Can you reach out to the developers? Is there a contact?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "617518d4-66d6-434a-b2ea-9298d9a7a06b",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "cc057984-4a03-4942-8726-d1cfd9e97713",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "c1cbd221-6881-42be-b6d6-8b842f3f05db",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Does the workflow engine need to support a particular compute back end you will use?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "9540973a-c3b9-45f8-a127-e76b053874a8",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "6c8cf810-c95d-4241-af5d-af7fa8130de8",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "20fa4bb1-ff56-4050-b20e-3544beb02c17",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Does the workflow engine need standard tools for administrators?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "e32737aa-338e-41d2-af0d-2764c7dfcde6",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "93cd18d8-d818-4aaf-b1c4-91ea99007da1",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "f5b2b4d2-2031-47da-95ff-6d10792c841f",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Is ease of development of the workflows an issue for you?",
                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "ed8000c8-f690-4e6f-a600-ad956490471c",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "d88f0055-2ca3-4070-89cd-b1a5422ccaba",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "f3c858ab-a89f-4f08-9c17-ca45e38f9340",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Does the workflow engine need a developer GUI?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "d92e6bb3-81cb-4218-a76c-94871dc8ff60",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "fbe3bbd8-77e4-4cab-a4b0-63aee4afb0b6",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "f1609178-599a-4da6-b93b-776e4f9b626c",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Does it need to be easy to support new tools in a workflow?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "c3c74172-5030-44e0-9c04-6c29479ccfdc",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "3d891703-defa-4c47-aeb6-e52b6afbdcb6",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "0567eb3f-3a55-4b8d-b61b-c530f8a52223",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Does it need support for specific kinds of data processing or data integration pipelines?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "9681e27d-d756-4708-8d6f-8c26ce57e4a5",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "893753a6-1b98-4126-8811-e42792d87df7",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "74d6de0c-8c7d-4001-8019-2a29391afed1",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Does it need support for complex control structures like conditionals and/or loops?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "694c4a4c-5272-444c-80bb-0c87ece9d874",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "5628065a-204f-4357-9607-95d74e3363fa",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "d8833614-8fb5-4535-b448-8e657d17ad44",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Does it need native support for specific data types?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "866bc1b1-d6c4-49cb-9f3b-75afbe35f5d3",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "290f7b14-8c88-41e5-b791-ca373174f45e",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "5179b194-aef6-47c5-bfb0-a21a0ba950ef",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Does it need  support for nested workflows?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "453c3e72-ee44-4c6b-824d-50d10e144c59",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "fd6d72e5-b927-4576-a903-75fb3072082b",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "5a83b169-3e41-42a5-8a4e-5c8c78111d0e",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Does it need  support collaborative editing of workflows?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "53cef353-cbb5-4985-9e92-b931cd3d676d",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "fbda04fb-0585-4cb0-bb38-37e75248eb37",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "add9596e-eaad-4720-9a11-f779fe1e6538",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Is ease of use an issue for you?",
                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "254bd2bb-1002-4173-a666-ac9849ab69cf",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "4908bd0c-2d3d-486c-998e-7d622bd7e371",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "6d31cd2e-ca00-4cf8-961e-f84d4dabdda4",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Who are the people that will run the workflows?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "272e49e3-688f-4a52-90fb-399be6ce7282",
                                                                                "label" : "Computer savvy operators",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "555dbdf9-0ddf-4294-8979-aea69eb1572d",
                                                                                "label" : "Subject matter experts with a strong computer knowledge",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "7cfd3e4a-67e4-4988-804d-81c6589a7046",
                                                                                "label" : "Subject matter experts with no affinity for computer solutions",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [
                                                                            {
                                                                                "uuid" : "18599585-b0f8-49e1-b775-78ba327fa531",
                                                                                "chapter" : "4.2.1"
                                                                            }
                                                                        ],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "6ed1691c-9298-48d7-aaf8-224bca512419",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Does the running of work flows need to be controlled via a GUI?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "f9f33b39-ad28-4872-86f1-0952465fb434",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "9e1303bf-3ee4-4ac5-bbb4-d14b44a42783",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "8330c11f-4a4f-4ed6-a088-58b50acfea62",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Do work flows need to be run on remote computers?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "cb4444e3-ca51-48ee-9a68-0c1e983c1396",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "adc8fe50-eeea-4acc-ad23-344501b01d5c",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [
                                                                            {
                                                                                "uuid" : "fb3f2cdc-bf6b-48f3-b5f3-c17fa2c3a28a",
                                                                                "chapter" : "4.2.2"
                                                                            }
                                                                        ],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "38b2a0d8-ce59-46cd-afa0-258c1b2f469f",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Is durability of work flows important for you?",
                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "b863299e-94c3-4a17-ad66-748f7cba80d6",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "5a54c4c2-b1c4-4e51-87e8-102a6af3d961",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "0d13a230-a3eb-4f5e-b9c2-2f64a5d9c033",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Do you need the same workflow to run next year? Durability against 'workflow decay'?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "9fa5ba3b-4c92-4a6b-9a5b-99fb4fb4c7b2",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "382d0640-0c54-4b98-800d-a69f5633d25e",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "655180dc-1cb9-41f6-8a36-7c6ecf02009a",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Are all versions of all tools under total control of the project?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "d48d7df6-78f2-4d0a-a129-fb59aa4f9cee",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "29cf1c35-6c17-4d45-890d-dc79f371dac8",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "92cab7ad-92ff-44ec-9678-8e5c95d66816",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Do you need to be able to export/import workflows (e.g. in the Common Workflow Language, CWL)?",
                                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "b3403202-6a35-488d-89e7-4c31273ac326",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "cc6e3b14-fb35-4acd-8984-c34144f060a7",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [
                                                            {
                                                                "uuid" : "2ab58d0b-a7e5-46a3-a1c8-f6f4378de5dd",
                                                                "chapter" : "4.2.3"
                                                            }
                                                        ],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "75af1eb3-1e0e-4be8-b0b6-cdb0883c791b",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Do you want your workflow engine to be professionally hosted by a specialized party?",
                                                        "text" : "There is no concrete map of these features to suitable workflow engines at this moment, but filling in these questions can lead you to think about the most important features for the project",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "7c8ab0c9-2c3d-49b3-bd3e-c731d46fe519",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "592d8fb9-1efd-491a-8ef4-e03b8d54225a",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "bdce5cee-2930-4b30-b3d5-d503ab0b5d1a",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Can you make a decision for the workflow engine based on the criteria deemed important?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "80164f45-1925-4b3f-b977-e6a1c59f8760",
                                                                "label" : "No selection can be made at this point",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "eb286cc2-f632-4c77-9a4d-8f3e8d734516",
                                                                "label" : "We will use 'make'",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "d827db55-b0c7-4ba0-a692-5a334e29f0a0",
                                                                "label" : "We will use 'snakemake'",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "d508ff6c-fbce-45b5-918c-0322a4687f2e",
                                                                "label" : "We will use 'taverna'",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "ce39135b-f399-4d76-a643-e170d7bcedcc",
                                                                "label" : "We will use 'galaxy'",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "560c5520-8dfd-42f1-ac36-7616edb8a4f9",
                                                                "label" : "We will use 'molgenis'",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "c1b026a4-38cd-41ad-acf9-541784c07567",
                                                                "label" : "We will use 'moteur'",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "5f111644-6f0e-4930-a551-1ab9c00a43d0",
                                                                "label" : "We will use 'knime'",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "c6068285-a656-4a41-97b0-61a9667c7c70",
                                                                "label" : "We will use 'clcbio'",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "eb5addd3-9ae2-47fb-bf5a-2d7adf9a9865",
                                                                "label" : "We will use 'chipster'",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "dd72ae3c-6945-463c-8f50-6045561b267c",
                                                                "label" : "We will need more than one engine",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "a13394f1-c99a-44b2-8464-cfc59ab3e57e",
                                                "chapter" : "4.2"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "558f01a3-2563-4114-9244-812b212c9791",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Do you plan taking special measures to guaranty the integrity of tools in the workflow?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "fe13a1d8-06be-430c-a486-7346b769b48f",
                                                "label" : "No",
                                                "advice" : "Consider changing this!",
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "ebc3a870-5f89-43ad-9d90-de5c91528ea2",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "4b4d5878-970b-41aa-bbf5-15072e704417",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will you only use well-scrutinized open source tools?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "1c921d11-0353-4cdb-8e64-265b2854269c",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "5bae42be-f2f9-4548-b7e8-41f38476a015",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "5ea8a4fd-4bed-48a9-aacf-71e2d4953eb4",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will you have all changes checked by a second developer before deploying them?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "7dbd8884-1e02-4c45-a31d-51d4a0922601",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "f3ecf7bd-13f9-451e-b91a-851e5a26396d",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "cd44886c-877f-4387-b299-f8d81b559f73",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "How will you handle upgrades to tools?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "805ed3e6-a9c0-4f57-b62b-c82c42b4761b",
                                                                "label" : "We will trust a new release and deploy",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "272626a8-90f7-4339-9f9b-a3fc7f1c8968",
                                                                "label" : "We will offer multiple versions of tools for the end user to choose",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "9437d4c6-46e3-4f59-88e0-a1eea71d912f",
                                                                "label" : "We will only deploy selected stable upgrades to tools",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "b585a5f5-accf-43c1-9ccf-4e88e08ca132",
                                                                "label" : "We will test all tool changes on a subset of our own data before deployment",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "67bf1b45-722b-44ed-877f-37eaf66dc41b",
                                                                "label" : "We will not change tool versions after the initial selection",
                                                                "advice" : "Please consider how you will be solving issues/problems you find yourself and that are fixed in a new release",
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "099902bd-1637-4e32-a744-b95031030c6c",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will you test whether known correct results are obtained over time?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "a547baf0-4638-4b38-97b9-9a4eafce20fa",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "4cc765a2-39b3-4790-bd7d-979361677a20",
                                                                "label" : "Yes, we will regularly make test runs on the same data",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "c9c66274-13b1-4ab8-8245-56ab7415d858",
                                                                "label" : "Yes, we will automatically make test runs on the same data",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "df3aff84-acf9-4614-996b-e69f7ddb96de",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Are all tools you use under your complete control?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "af507b8a-824b-4198-9adf-c54f984f4fb5",
                                                                "label" : "No, we use tools maintained by others",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "94898b30-877d-4776-a03a-c57c93a725ac",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "How will you deal with the situation where support for a tool is dropped?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "d3fe6de8-b411-41c7-a336-c49e37d4b9d1",
                                                                                "label" : "We will keep running it unchanged",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "8e9bcaec-cc3a-45e3-a430-3cbf1488a2a3",
                                                                                "label" : "We will start to do necessary maintenance ourselves",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "523531e2-b2ae-4fbd-90a3-c2c007a9f04e",
                                                                                "label" : "We will look for alternatives",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            },
                                                            {
                                                                "uuid" : "614310a5-9bfd-49ae-9bec-5ec45cacad22",
                                                                "label" : "Yes, we only use tools that we can maintain ourselves",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "1991077f-04ae-4808-90a5-e4b2f82e30bf",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "How will you make sure to know what exactly has been run?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "1f88f7ba-2912-489c-910d-df16540d797a",
                                "label" : "This has been arranged",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "12a85bf3-1407-4783-9e73-564d75e7c98a",
                                "label" : "More guidance is desired",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "5e111fc1-9d17-4ec8-aafb-1694a538a3aa",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you keep results together with all processing scripts or workflows including documentation of the versions of the tools that have been run?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "f5518b1e-77c8-47f7-98e4-4be5ea58f9ab",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "0d9d77b1-bbcd-480e-b165-8bcb29bd4a67",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "a972fba5-7586-4156-bdf8-8251288dc341",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you make use of the metadata fields in your output data files to register how the data was obtained?",
                                        "text" : "File formats like VCF (for genetics) and TIFF (for images) have possibilities to document metadata in the file header. It is a good idea to use work flow tools that use these fields to document what was done to obtain the data.",
                                        "answers" : [
                                            {
                                                "uuid" : "a3031101-4077-4c5b-877f-bff865c1920c",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "2f4e30d6-6a43-4f52-b146-26f1f690549f",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "decb7c9c-c6dc-4027-8c0e-18934c852ca6",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you use a central repository for all tools and their versions as used in your project?",
                                        "text" : "Especially if analysis and processing of data in the project is done on multiple different computers by different people, it is a good idea to have your own repository of tools and their blessed versions.",
                                        "answers" : [
                                            {
                                                "uuid" : "45d40564-f470-4e56-9262-cfcb94b1bbef",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "a7b481ba-13cb-407d-896f-cc662160f4ee",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "a32cfa0c-e51a-40c2-a82c-835f0ee3ef6f",
                                                "chapter" : "4.4"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "4c7cd15f-90f9-4ab3-8082-38da4f642de1",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you use a central repository for reference data used in your project?",
                                        "text" : "Especially if analysis and processing of data in the project is done on multiple different computers by different people, it is a good idea to have your own repository of reference data versions.",
                                        "answers" : [
                                            {
                                                "uuid" : "67ba0ead-d482-48c8-965a-2472a99648f3",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "e8b48f96-d3d5-4e54-a215-4454d08e6b59",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "1419671e-5bb4-4a19-a801-2bc3305311cb",
                                                "chapter" : "4.4"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "49199b05-73d7-4912-a5ca-bc1e14a09468",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you make use of standard workflow engines and automatic work flows for all data analysis in the project?",
                                        "text" : "It is much easier to guarantee consistency and reproducibility if all data processing is done using automated work flows, especially if the workflow engine automatically keeps adequate provenance data.",
                                        "answers" : [
                                            {
                                                "uuid" : "a9db9446-5bbc-431e-981d-d2f487ab28ae",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "ea0ab328-184c-40f2-a391-c89617b37fe9",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "56bc8ebb-aba5-4745-8034-17ed3fe4fe07",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Are all software tools in the work flow professionally maintained, with version control?",
                                        "text" : "Will you be able to find and reproduce exactly which version was used for any analysis? Not only for the major tools in the workflows, but also for all 'glue' code and small tools you created especially for the project?",
                                        "answers" : [
                                            {
                                                "uuid" : "76d2327b-8ac0-44eb-9d42-2434182824a0",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "8afcd524-0916-4980-8bee-15b00bf5c335",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "918d5fd1-ea37-468f-8acd-ca3e80203900",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "How will you validate the integrity of the results?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "58851f0f-b3da-4694-9350-0055c072b9f6",
                                "label" : "This has been arranged",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "30d1e85c-0e3e-482f-97f9-22a7658b329e",
                                "label" : "More guidance is desired",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "e8a08f27-9f82-4147-b446-822d34a5d468",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you run a subset of your jobs several times across the different compute infrastructures you are using?",
                                        "text" : "There are surprisingly many complications that can cause (slight) inconsistencies between results when workflows are run on different compute infrastructures. A good way to make sure this does not bite you is to run a subset of all jobs on all different infrastructure to check the consistency.",
                                        "answers" : [
                                            {
                                                "uuid" : "8d038086-a207-46f4-9e7c-180780d9c3a6",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "be0761b6-421a-4683-8ff9-de496868feea",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "a3a4ce37-4ced-41df-8ec6-e42d87a6a3f1",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you be instrumenting the tools into pipelines and workflows using automated tools?",
                                        "text" : "Surrounding all tools in your data processing and analysis workflows with the 'boilerplate' code necessary on the computer system you are using is tedious and error prone. Especially if you are using the same tools in multiple different work flows and/or on multiple different computer architectures. Automated instrumentation, e.g. by using a workflow management system, can prevent many mistakes.",
                                        "answers" : [
                                            {
                                                "uuid" : "9ffbb7f2-5eb8-455a-bd98-6547887e0eb6",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "4715c145-8e34-45b9-b7c1-77d835d3ade8",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "34a3ef8b-4a17-4030-9157-ed1c1bf60b80",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you use independently developed duplicate tools or workflows for critical steps to reduce or eliminate human errors?",
                                        "text" : "Validation of results without a golden standard is very hard. One way of doing it is to develop two solutions for a problem (two independent workflows or two independently developed tools) to check whether the results are identical or comparable.",
                                        "answers" : [
                                            {
                                                "uuid" : "f14a3135-32d9-4520-bc09-45f50f0c6031",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "56c899c9-fb21-449c-a905-4b9f4fbf6458",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "faf72b3d-ac29-41d1-97f2-5223b199a086",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you run part of the data set repeatedly to catch unexpected changes in results?",
                                        "text" : "Running a small subset of the data repeatedly can be useful to catch unexpected problems that would otherwise be very hard to detect.",
                                        "answers" : [
                                            {
                                                "uuid" : "168ba2ce-e5aa-4dc5-a840-845a5c20058f",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "2c9f5097-34a8-4852-8e10-b26bd71ee1ba",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "199d32a0-b997-4570-ac83-8d6ded27dd3a",
                                                "chapter" : "4.2.4"
                                            }
                                        ],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "acc82522-327a-4c76-8c24-f8034f7dc1d4",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Do you have a contingency plan?",
                        "text" : "What will you do if the compute facility is down?",
                        "answers" : [
                            {
                                "uuid" : "05a642e0-4d1c-4c1e-ac9e-06463c2e97f4",
                                "label" : "We will wait until the problem is fixed",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "b430a132-0c46-4e16-9508-190801976633",
                                "label" : "We have an alternative",
                                "advice" : null,
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    }
                ]
            },
            {
                "uuid" : "d5b27482-b598-4b8c-b534-417d4ad27394",
                "title" : "Data interpretation",
                "text" : "",
                "questions" : [
                    {
                        "uuid" : "c8e35b21-9911-43f4-bb5e-0df3de1cd727",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will data interpretation and modeling require significant compute infrastructure capacity?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "5905c733-5113-401f-9343-24911af54e05",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "b5c79d03-4f0f-4714-91a8-9b04a8f3980f",
                                "label" : "Yes",
                                "advice" : "Make sure this has been taken into account in the capacity planning under 'Data design and planning'",
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "948b5fd8-c1bd-457d-8f81-ea2fd093f541",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "How will you be making sure there is good provenance of the data analysis?",
                        "text" : "Data analysis is normally done manually on a step-by-step basis. It is essential to make sure all steps are properly documented, otherwise results will not be reproducible.",
                        "answers" : [
                            {
                                "uuid" : "7b450ee7-8783-4adc-9955-fd667fac5a56",
                                "label" : "We use lab notebooks",
                                "advice" : "Make sure to make the notes available in electronic form along with your data",
                                "followUps" : []
                            },
                            {
                                "uuid" : "a1e67aee-77dd-486f-ab51-e5a6f39a32cc",
                                "label" : "We use an electronic lab notebook",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "ccdb5296-919c-44ac-89df-351fd2e564ae",
                                "label" : "We use other arrangements",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "b73b8310-3b4f-49c8-a5c0-4208883754ac",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeText",
                                        "title" : "What other arrangements?",
                                        "text" : "",
                                        "answers" : null,
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "460803f2-cc23-4c57-b50e-c5177a3c11d5",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you be doing (automated) knowledge discovery?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "8592fc25-fd0f-438c-b0e9-2b0ad81475c7",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "8d4640c2-25c2-4c1d-a1f2-6fb47a560b96",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [
                            {
                                "uuid" : "10f24580-e6a1-45a1-b1b1-c8664057f4ec",
                                "chapter" : "6.6"
                            }
                        ],
                        "experts" : []
                    }
                ]
            },
            {
                "uuid" : "6be88f7c-f868-460f-bba7-91e1c659adfd",
                "title" : "Information and insight",
                "text" : "",
                "questions" : [
                    {
                        "uuid" : "6516eae1-98b2-48f2-9862-b5fb140cfad7",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you be working with the philosophy 'as open as possible' for your data?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "ca5c002d-fb3f-4873-bede-a8f304a3934f",
                                "label" : "No",
                                "advice" : "You will need to explain!",
                                "followUps" : []
                            },
                            {
                                "uuid" : "67a8f6e1-50db-4b04-b1fa-d52941f4e535",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [
                            {
                                "uuid" : "4bf03b71-2739-492b-b568-057206d14197",
                                "chapter" : "7.1"
                            }
                        ],
                        "experts" : []
                    },
                    {
                        "uuid" : "a549d10b-aa46-4c0c-863f-30219ac5ecce",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Can all of your data become completely open immediately?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "b3739ebd-2d8e-42d3-9425-a7d6d1b26c79",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "c010e830-bd89-460d-9498-cb41e7ffeb87",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : " Are there legal reasons why (some of your) data can not be completely open?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "31f2fcda-dbb2-40f6-871b-c3cc59797a6b",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "aac95530-2978-4759-803b-64721533faf0",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "019db0b3-9067-4134-8bfd-76db3cfc572a",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Are there privacy reasons why your data can not be open?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "b6dfc087-93d6-4dcf-b45c-3c6600395ec6",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "8a56768a-5c5a-44c0-b21c-46a231fbf6be",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "754148c2-6019-4318-8d44-d73becc989f4",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Are there restrictions on where the data need to be stored?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "6fd34203-6217-4c1b-a706-c5fa155ea706",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "2f0e4c16-be62-4836-aa0e-b52fd9132ac7",
                                                                                "label" : "Yes, they must stay in the EU",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "00bddac1-2375-4554-bb3c-27b261cc22e7",
                                                                                "label" : "Yes, they must stay in the same country",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "f6adfe7d-45f5-41a4-ba48-e43cc131c824",
                                                                                "label" : "Yes, they must stay in the same institute",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "a25b30f4-2d0f-4132-9b8e-0950f0b0ed66",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Could pseudonymization be used to make the data more openly available?",
                                                                        "text" : "Legally, pseudonymous data (which means that someone has the key to reverse the process) is still considered privacy sensitive information. However, the EU is working on special cases where the data can still be opened as long as the key availability is sufficiently limited.",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "5edeab6e-81a9-4209-b063-8d6fca55a388",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "49f268a6-9566-4aa4-bec3-44fec2e64548",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : [
                                                                                    {
                                                                                        "uuid" : "5db6bd58-5d1d-4d02-9497-972cbef64c16",
                                                                                        "shortUuid" : null,
                                                                                        "type" : "QuestionTypeOptions",
                                                                                        "title" : "Can you make use of an existing 'trusted third party' for pseudonymization?",
                                                                                        "text" : "Making use of the same pseudonymization for different studies makes it possible to integrate information later. Obviously it also raises the risk of re-identification",
                                                                                        "answers" : [
                                                                                            {
                                                                                                "uuid" : "40be8b9d-0e4d-400f-a982-c0b53bc64b61",
                                                                                                "label" : "No",
                                                                                                "advice" : null,
                                                                                                "followUps" : []
                                                                                            },
                                                                                            {
                                                                                                "uuid" : "041db1a8-e263-4df0-aea6-337063008b37",
                                                                                                "label" : "Yes",
                                                                                                "advice" : null,
                                                                                                "followUps" : []
                                                                                            }
                                                                                        ],
                                                                                        "answerItemTemplate" : null,
                                                                                        "references" : [],
                                                                                        "experts" : []
                                                                                    }
                                                                                ]
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "15ee1921-1fea-4f22-b462-b3cf7cdd4646",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Could anonymization be used to make the data more openly available?",
                                                                        "text" : "Different anonymization techniques exist. Disadvantage of anonymization is that data integration becomes virtually impossible, but it may be the only way to open up your data for other research",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "324dc2d9-df7f-4849-a5c0-91ecf2ef2dbd",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "c0d7df59-0cf2-4ff1-9dd0-a2f2dd5bed91",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "69be6695-152b-48ba-a1fd-6662476e39b7",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Could you use data aggregation to make the data openly available?",
                                                                        "text" : "Aggregated data, where typically at least 15 individuals are in any data point, are considered sufficiently anonymous. This is an alternative way of making data openly available for future research",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "94811bac-3a00-40cd-acdf-638cd79845a8",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "1c1c557c-ef6c-44ff-b618-c1cfe3543057",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "a18d79d2-74b1-4524-a3c7-2c1a87b66a75",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Are there IP reasons why your data can not be open?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "cad24d70-f9b5-4159-b352-2f91dbc92ac5",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "2ffa365d-83f6-4adb-bcc6-377fec82a297",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "b4f9916a-c846-49ff-bfd2-f5d1346de553",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Is it clear who owns data and documents?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "3de2edc2-c05f-49a7-8d1b-699cdaa98350",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "fc008273-7382-422b-8735-1b75dfb5ba4c",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "c9ed4379-19e2-4ce3-8962-5c2fca69b6cd",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Will someone be given decision power to move documents or data to a new place after the project has finished?",
                                                                        "text" : "In one case in the past, all documents that had been assembled by a project in a documentation system had to be deleted because not a single person could decide to move them to a new platform when the documentation system was going off-line.",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "46f96ea2-3a3c-43d1-b6a2-394a49034ae2",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "8eff4cfd-65c1-435f-b27c-e805b3d72c5c",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "55f03a4a-034b-422a-adf6-757416b7650a",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will you be allowing authenticated access to the data?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "da8b25a9-8865-4ce8-a2ba-d592c42daa4c",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "18a46146-25f6-4e5c-90ae-c6b9be9cc3f0",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "baed9cce-91ab-45d0-a272-92246b2e6c72",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Where will the data be stored?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "09f8f08d-04ae-463a-bbe0-f5ce05595777",
                                                                                "label" : "In a domain repository (like EBI's EGA) that arranges restricted access",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "46a96e04-4d2a-41e8-90e7-b6afa3f318fd",
                                                                                "label" : "In a national or institutional repository that arranges restricted access",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "00e67874-dbe8-41a4-b9fc-0a4429765fce",
                                                                                "label" : "In a dedicated repository",
                                                                                "advice" : "make sure you can guarantee longevity of this repository!",
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "278a8218-eb9f-43b4-8dac-cd6a644537fa",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Who will take care of authentication of potential users?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "dc4e23fd-5477-41c8-919d-74df342f2d33",
                                                                                "label" : "We will use username/password authentication, possibly augmented with two-factor security",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "0c05f134-b694-4697-9d72-750374da423c",
                                                                                "label" : "We will use a single sign-on system such as edugain",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "109e8083-1eb6-4b69-b7ae-32dab8b6a5f6",
                                                                                "label" : "We will make other arrangements",
                                                                                "advice" : null,
                                                                                "followUps" : [
                                                                                    {
                                                                                        "uuid" : "7b187628-eb1e-40f7-9e75-5787a1ff5a3e",
                                                                                        "shortUuid" : null,
                                                                                        "type" : "QuestionTypeText",
                                                                                        "title" : "What other arrangements?",
                                                                                        "text" : "",
                                                                                        "answers" : null,
                                                                                        "answerItemTemplate" : null,
                                                                                        "references" : [],
                                                                                        "experts" : []
                                                                                    }
                                                                                ]
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "23909308-55cf-4e94-a616-2a74c809aa48",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Who will take care of authorization of potential users?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "ed47c9b7-9c2b-48ab-bdf4-efd8913f673e",
                                                                                "label" : "One of the project members will authorize",
                                                                                "advice" : "It is not a good idea to tie this to a person",
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "ca3b6db3-cd6c-4139-8d24-e5ded15c2796",
                                                                                "label" : "We will set up a data access committee",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "d010e09e-8ce8-47c6-a1f9-f281519ba936",
                                                                                "label" : "We will make use of an existing data access committee",
                                                                                "advice" : null,
                                                                                "followUps" : [
                                                                                    {
                                                                                        "uuid" : "0de7232f-802d-4e1f-a84c-2895d08901ce",
                                                                                        "shortUuid" : null,
                                                                                        "type" : "QuestionTypeText",
                                                                                        "title" : "Which existing Data Access Committee?",
                                                                                        "text" : "",
                                                                                        "answers" : null,
                                                                                        "answerItemTemplate" : null,
                                                                                        "references" : [],
                                                                                        "experts" : []
                                                                                    }
                                                                                ]
                                                                            },
                                                                            {
                                                                                "uuid" : "f57960de-3cfb-47da-a30b-da7c5d27a38e",
                                                                                "label" : "We will make other arrangements",
                                                                                "advice" : null,
                                                                                "followUps" : [
                                                                                    {
                                                                                        "uuid" : "346003fa-5ea6-41ed-a3b7-68d7e26a0b3e",
                                                                                        "shortUuid" : null,
                                                                                        "type" : "QuestionTypeText",
                                                                                        "title" : "What other arrangements?",
                                                                                        "text" : "",
                                                                                        "answers" : null,
                                                                                        "answerItemTemplate" : null,
                                                                                        "references" : [],
                                                                                        "experts" : []
                                                                                    }
                                                                                ]
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "c534c029-cb42-46a9-a118-87abfe3b54da",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Are the criteria for application to access the data openly available?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "b3d0fe1f-6ffd-4c29-9e67-22fcad6fb00b",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "c9509a79-9c56-44eb-8508-dc1c18dfec8d",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "b7cb192b-349a-4966-b37d-b63903bf2204",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Has auditing for the re-use been arranged?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "651f3154-1f37-4367-9651-ba5dfdf09783",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "0f2324d2-73c7-46e3-83b4-f38e78b3f0ac",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "85a9d872-3d41-4560-82c4-b850a6e2d5ac",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Are there business reasons why (some of your) data can not be completely open?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "c87ef228-d9e5-4387-8ff6-5b6a05402a95",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "1c377b4c-bd6f-457e-8e7e-06efcd1b6444",
                                                "label" : "Yes, patents still need to be applied for",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "0cfc22ec-5551-4a37-84ec-308c85440af5",
                                                "label" : "Yes, other business reasons",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "43370b85-0ee0-4f0f-b7de-9b04a54a2480",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeText",
                                                        "title" : "What other business reasons are there not to open all data immediately?",
                                                        "text" : "",
                                                        "answers" : null,
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "c10f9098-5b1c-4abc-adaa-bdef2fb537ca",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Are there other reasons why (some of your) data can not be completely open?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "0eeabfdc-d62a-4868-bece-b696bccecc3a",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "cb052963-c573-4567-a462-4a5444a87808",
                                                "label" : "Yes, papers need to be submitted first",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "2b755a90-8a43-41c0-9020-5e2b9ca8f88e",
                                                "label" : "Yes, other reasons",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "b289fdcc-aedc-496e-8169-e0bbe2346d1f",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeText",
                                                        "title" : "What other reasons are there not to open all data immediately?",
                                                        "text" : "",
                                                        "answers" : null,
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "f5c12fcf-15d0-44ba-a53c-49ea9d7f620f",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you use a limited embargo?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "3da39a55-18e5-4b0a-8da1-5b7fc5260ad1",
                                                "label" : "No, some restricted data will be embargoed indefinitely",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "b51a276b-3c57-40ef-bacc-0130d56db965",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeText",
                                                        "title" : "What is the maximum embargo period?",
                                                        "text" : "",
                                                        "answers" : null,
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            },
                                            {
                                                "uuid" : "82f84d7e-338c-46bc-9785-42df560c01ed",
                                                "label" : "No, data will be released only as soon as restrictions are falling away",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "1b120967-803b-458b-b565-dbee6a785509",
                                                "label" : "Yes, data that is not legally restrained will be released after a fixed time period, unconditionally",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            },
                            {
                                "uuid" : "8c33553c-9603-4156-82d7-85ab3d7de090",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "4e0c1edf-660c-4ebf-81f5-9fa959dead30",
                        "shortUuid" : null,
                        "type" : "QuestionTypeList",
                        "title" : "Specify a list of data sets you will be publishing",
                        "text" : "Specify a short name for each data set, sufficient for yourself to know what data it is about. It is useful to think about a data set as some collection of data that will be ending up in the same place.",
                        "answers" : null,
                        "answerItemTemplate" : {
                            "title" : "Item",
                            "questions" : [
                                {
                                    "uuid" : "80a682bd-8a5c-4a52-935d-680509838a4e",
                                    "shortUuid" : null,
                                    "type" : "QuestionTypeOptions",
                                    "title" : "What kind of repository will this data be stored in?",
                                    "text" : "Domain repositories often have the best functionality to make the data findable and reusable. Many of them are listed in https://fairsharing,org/",
                                    "answers" : [
                                        {
                                            "uuid" : "1dc412c6-da92-4cc2-8639-316c0c6ec5ff",
                                            "label" : "A domain-specific repository",
                                            "advice" : null,
                                            "followUps" : [
                                                {
                                                    "uuid" : "221c322e-dff5-438f-8a2e-90e762681156",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeText",
                                                    "title" : "What repository?",
                                                    "text" : "",
                                                    "answers" : null,
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                },
                                                {
                                                    "uuid" : "b1492627-9cb6-465a-945a-a2a7ff9a8189",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "Will you contact the repository beforehand?",
                                                    "text" : "Contacting the repository early may be useful to establish conditions, formats, and metadata requirements for submission. It may also be necessary to establish whether the repository can accommodate your data",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "c86c80b4-72b3-4194-956a-27e40d3c23eb",
                                                            "label" : "No, this submission is routine both for us and the repository",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "e09562c0-346c-4c95-b470-c5810aab18fa",
                                                            "label" : "Yes, we have contacted the repository",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "93c590a5-60a6-481d-9482-9145435143a2",
                                                            "label" : "Yes, we will contact the repository",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "620b1090-2a57-4f3c-b336-97c9e95da637",
                                                            "label" : "We have made other arrangements",
                                                            "advice" : null,
                                                            "followUps" : [
                                                                {
                                                                    "uuid" : "fc0857c1-e4d2-47f5-86b1-0292bfc7eecf",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeText",
                                                                    "title" : "What other arrangements?",
                                                                    "text" : "",
                                                                    "answers" : null,
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                }
                                            ]
                                        },
                                        {
                                            "uuid" : "fb96d008-f31a-4a1a-b203-b3f9bb32fcc8",
                                            "label" : "Our national repository",
                                            "advice" : "Disadvantage of a general purpose repository is the lack of data-specific features (e.g. 'play' instead of 'download' for an audio file) and limited findability",
                                            "followUps" : []
                                        },
                                        {
                                            "uuid" : "df0b2ee3-8e4d-42a1-9bb2-c69228074406",
                                            "label" : "Our institutional repository",
                                            "advice" : "Disadvantage of a general purpose repository is the lack of data-specific features (e.g. 'play' instead of 'download' for an audio file) and limited findability",
                                            "followUps" : []
                                        },
                                        {
                                            "uuid" : "afd9f4d5-20e0-4fa0-a42a-376c132ff5b0",
                                            "label" : "A special-purpose repository for the project",
                                            "advice" : null,
                                            "followUps" : [
                                                {
                                                    "uuid" : "f83a9afd-c6de-452b-be9f-bd76e5eb6b54",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "Will you be able to support this repository for a sufficiently long time?",
                                                    "text" : "Maintaining a repository is a long term commitment that can most of the time not be funded from project money. Think about who will be doing the system maintenance, hardware maintenance, and who will make necessary code changes to accommodate future (safe) versions of the system",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "dc921fac-cf77-4d3d-bb9c-907d37f838ef",
                                                            "label" : "No",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "714e0a22-c059-4f80-9780-6f7f2e2ce53b",
                                                            "label" : "Yes",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                },
                                                {
                                                    "uuid" : "cbbfb0da-caf1-4c04-b72f-de39c4a8fb33",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "What is the service level you will be offering to users of the data?",
                                                    "text" : "",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "dc1082fd-1c90-492c-833c-3e0909c643bb",
                                                            "label" : "Download only",
                                                            "advice" : "That functionality level can surely be found in another place too!",
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "da6725a2-62bb-4fe7-a76f-a4e396646b19",
                                                            "label" : "A search and access interface",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "d65ddfda-9b08-4ab6-90e5-07df748d7929",
                                                            "label" : "A processing service",
                                                            "advice" : "Make sure to budget sufficient compute resources for the users",
                                                            "followUps" : []
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [
                                                        {
                                                            "uuid" : "7651094b-48bb-47df-81ab-8539d4799fe9",
                                                            "chapter" : "7.4.1"
                                                        }
                                                    ],
                                                    "experts" : []
                                                },
                                                {
                                                    "uuid" : "f691ad73-1284-4c7d-999d-34bb0612dd42",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "Did you arrange for system administration (at least security updates) and hardware maintenance for the server(s)?",
                                                    "text" : "",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "4dd69e44-097b-42fc-98a4-4ce379453d46",
                                                            "label" : "No",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "d2c01a84-fc1d-4108-95c4-9e9e9198f93e",
                                                            "label" : "Yes",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                },
                                                {
                                                    "uuid" : "f606bc7e-8505-4a5d-90aa-684f2fb666cf",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "What kind of service will your repository offer to users?",
                                                    "text" : "The amount of funding needed to maintain a repository is strongly dependent on the services offered",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "069a6978-7771-418e-8f1a-1ce5f62347ca",
                                                            "label" : "Find and download only",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "f4fa7d05-ba8e-4095-918c-50c34ba54284",
                                                            "label" : "Search and advanced access",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "6de13f14-8f2a-422c-8cb8-1e68aaeeb7c4",
                                                            "label" : "Anything including some data processing",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                }
                                            ]
                                        }
                                    ],
                                    "answerItemTemplate" : null,
                                    "references" : [],
                                    "experts" : []
                                },
                                {
                                    "uuid" : "46039bea-87e6-4c61-8567-4e819404b4d0",
                                    "shortUuid" : null,
                                    "type" : "QuestionTypeOptions",
                                    "title" : "Will you be adding a reference to the published data to at least one data catalogue?",
                                    "text" : "Data is sometimes difficult to locate, especially if it is not in a domain-specific repository. Data catalogues may increase findability.",
                                    "answers" : [
                                        {
                                            "uuid" : "0224af28-8272-4360-b8b2-0fcc835832e9",
                                            "label" : "No",
                                            "advice" : null,
                                            "followUps" : []
                                        },
                                        {
                                            "uuid" : "9e39ea26-cdaf-483b-9502-d47fc4f69bab",
                                            "label" : "Yes",
                                            "advice" : null,
                                            "followUps" : []
                                        }
                                    ],
                                    "answerItemTemplate" : null,
                                    "references" : [],
                                    "experts" : []
                                }
                            ]
                        },
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "92a10652-3675-48f2-8d50-180a29cec62e",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you be making sure that blocks of data deposited in different repositories can be recognized as belonging to the same study?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "2f7e45cd-0f00-4228-865e-62f64ccde1b1",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "0e748d3d-fd83-470f-918a-0321b1fc9f11",
                                "label" : "Yes, all data sets will have links to the related data",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "18ca5e47-464d-40c5-881b-c15e54bd2bff",
                                "label" : "Yes, all data sets will be linked from a single catalog entry",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "6445c352-5685-4e23-ac52-ebcabe353bd0",
                                "label" : "Yes, we have made other arrangements",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "1accb605-505a-488b-be74-26faa3e34e2b",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeText",
                                        "title" : "What other arrangements?",
                                        "text" : "",
                                        "answers" : null,
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "3d20fbb7-c401-4fa2-aeaa-ec5a2ef35322",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Did you work out the financial aspects of making the data available?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "902cf40c-cb46-4f3f-811a-b36faa612fa6",
                                "label" : "This has been taken care of",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "e1f2f5cb-2e70-4d0d-b00c-c1f4792b6f49",
                                "label" : "Explore this with additional questions",
                                "advice" : null,
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "25e9fe5b-d8f7-4b43-a903-7809a571a2bb",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will your publications be open access?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "9e292e79-343e-4c87-8121-2ff19fc2e8d8",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "5b5d1c59-2e3a-4e1a-aafa-c2451c8e11da",
                                "label" : "Yes, this is budgeted in the project",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "ef5e44f4-4902-4117-b783-59270cb327c9",
                                "label" : "Yes, our department/institute will pay for open access publishing",
                                "advice" : null,
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [
                            {
                                "uuid" : "fa2d9ff4-bbc9-41d7-83f9-36a2b3c5d138",
                                "chapter" : "7.2"
                            }
                        ],
                        "experts" : []
                    },
                    {
                        "uuid" : "4baf405e-262e-4f1a-bfdc-fe72fc628650",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Are there any recurring fees to keep data or documents available?",
                        "text" : "Are you using any commercially licensed products to keep data, software or documents available, for which a regular fee must be paid?",
                        "answers" : [
                            {
                                "uuid" : "ccbe8121-ac77-4619-a49d-5a755c01d5a2",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "5951120f-af93-4f15-b15a-b2be9a89efff",
                                "label" : "Yes",
                                "advice" : "Make sure this will be kept running by the department or institute. It is best to also have a backup plan, being able to move data and documents to a different place if a service is discontinued. For this, you may need to arrange permission from all project partners beforehand.",
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "f9d84278-b61d-4314-94e9-12644bfa1d00",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you be archiving your data after the project in 'cold storage'?",
                        "text" : "Will you be storing (in cold storage) copies of your own data for a longer period after the project has ended? Possibly as a continuation of archival as part of data storage strategy during the project?",
                        "answers" : [
                            {
                                "uuid" : "b6890d88-88e4-4667-8b28-82bc73c728be",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "562a4823-3e58-44da-ba65-db522c4bbbd9",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "553834d6-ff71-4b76-b4b4-b90d19a3f0a4",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will data formats of data in cold storage be upgraded if they become obsolete?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "24782dc1-c7f9-4b9f-868e-09f6f6e5a0ed",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "522019ce-14d8-4f74-b4a5-79b05a2abac1",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "5a192c70-d824-49d2-965c-dca90deb04ac",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will data be migrated regularly to more modern storage media (e.g. newer tapes)?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "2bb32b88-b374-4090-82e4-64f5eab44850",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "6a1acee5-200a-449f-8200-4b294c8bc5f0",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [
                            {
                                "uuid" : "f9e87496-28a0-41ee-a95f-f23c01baecf4",
                                "chapter" : "7.4.4"
                            }
                        ],
                        "experts" : []
                    },
                    {
                        "uuid" : "a84cdefc-8c41-4949-9353-5916532ad50c",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you also publish data if the results of your study are negative/inconclusive or unpublishable?",
                        "text" : "Even if you do not obtain the results you had foreseen from your own study, the data can still be valuable for reuse in another context. Also, publishing the data can avoid that someone else collects a similar data set with a similar negative result.",
                        "answers" : [
                            {
                                "uuid" : "2eec960b-5033-4fcd-9d7e-219873762acc",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "a19404c9-94ae-4b3a-a728-2457084a5b31",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "1ae8d7b0-4bcd-4914-aaea-fa389fc056f4",
                        "shortUuid" : null,
                        "type" : "QuestionTypeList",
                        "title" : "Specify a list of software packages you will be publishing",
                        "text" : "Specify a short name for each software package.",
                        "answers" : null,
                        "answerItemTemplate" : {
                            "title" : "Item",
                            "questions" : [
                                {
                                    "uuid" : "36938ca8-e22d-47a6-8859-a52f536101c5",
                                    "shortUuid" : null,
                                    "type" : "QuestionTypeOptions",
                                    "title" : "Will you be adding a proper open-source license?",
                                    "text" : "",
                                    "answers" : [
                                        {
                                            "uuid" : "d675443d-3581-4b7e-bfeb-9b06349fec1a",
                                            "label" : "No",
                                            "advice" : null,
                                            "followUps" : []
                                        },
                                        {
                                            "uuid" : "0a779591-6b8e-4e65-96b6-e3782f71532c",
                                            "label" : "Yes, we have decided on an open source license",
                                            "advice" : null,
                                            "followUps" : [
                                                {
                                                    "uuid" : "0184bfcc-69ab-48c8-9d62-e23ae73a60be",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeString",
                                                    "title" : "What is the license?",
                                                    "text" : "",
                                                    "answers" : null,
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                }
                                            ]
                                        },
                                        {
                                            "uuid" : "6d887b51-696e-41ee-947f-d0022d7c6aee",
                                            "label" : "Yes, we will decide on an open source license",
                                            "advice" : null,
                                            "followUps" : []
                                        }
                                    ],
                                    "answerItemTemplate" : null,
                                    "references" : [],
                                    "experts" : []
                                },
                                {
                                    "uuid" : "87b45400-169d-424d-8e71-d2034618dab5",
                                    "shortUuid" : null,
                                    "type" : "QuestionTypeString",
                                    "title" : "Where will the software package be available?",
                                    "text" : "",
                                    "answers" : null,
                                    "answerItemTemplate" : null,
                                    "references" : [],
                                    "experts" : []
                                },
                                {
                                    "uuid" : "3effd452-f0ae-4c25-b971-2b5762889ca4",
                                    "shortUuid" : null,
                                    "type" : "QuestionTypeOptions",
                                    "title" : "Will this software be listed in a catalogue?",
                                    "text" : "",
                                    "answers" : [
                                        {
                                            "uuid" : "89e5e0fe-2066-4fc1-b872-be6b317a213b",
                                            "label" : "No",
                                            "advice" : null,
                                            "followUps" : []
                                        },
                                        {
                                            "uuid" : "3a417eea-0b1d-4dc8-b5f5-f109d7513eb4",
                                            "label" : "Yes",
                                            "advice" : null,
                                            "followUps" : []
                                        }
                                    ],
                                    "answerItemTemplate" : null,
                                    "references" : [],
                                    "experts" : []
                                }
                            ]
                        },
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "9b3e6391-d5c3-4d82-bf60-342ed2ac1f43",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will there be planning of valorization or translational returns?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "382dc918-7ff3-4cd0-a821-e0040945c7d2",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "ad2060bb-ea0e-4fb5-b8eb-a4fd6425b1ac",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    }
                ]
            },
            {
                "uuid" : "83438863-0aa0-4458-b14b-2b2c0d4f811d",
                "title" : "Data integration",
                "text" : "",
                "questions" : [
                    {
                        "uuid" : "109ed7ea-ef1d-4719-b2b4-eb4f7202c65c",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "How will you be doing the integration of different data sources?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "3b573e6c-67a5-442e-937e-33074a14fb96",
                                "label" : "This as been taken care of",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "238ace4b-22bb-4a9d-a312-e30d82d4382d",
                                "label" : "Explore this with additional questions",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "a797cab9-0829-4787-a096-1b5cedc9147f",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeList",
                                        "title" : "List the data formats you will be using for data integration",
                                        "text" : "Answer some questions for each",
                                        "answers" : null,
                                        "answerItemTemplate" : {
                                            "title" : "Item",
                                            "questions" : [
                                                {
                                                    "uuid" : "87fe7235-0a0f-4788-8f44-04523461655f",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "How is the data structured?",
                                                    "text" : "",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "b308b8bb-83ed-4599-ae28-39c5c1218cbf",
                                                            "label" : "(meta)data fields in a domain specific file",
                                                            "advice" : null,
                                                            "followUps" : [
                                                                {
                                                                    "uuid" : "37531bec-1488-4204-807d-762794b538e9",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Can all of the data you want to couple be captured in that format?",
                                                                    "text" : "",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "5313e765-56a9-4c22-8ecf-b55253f17366",
                                                                            "label" : "No",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "f2c24899-477f-492f-a979-c7586b9593b0",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : [
                                                                                {
                                                                                    "uuid" : "45efdccf-761e-4414-b6a8-1d28ddaeafec",
                                                                                    "shortUuid" : null,
                                                                                    "type" : "QuestionTypeOptions",
                                                                                    "title" : "Will you be doing it that way?",
                                                                                    "text" : "",
                                                                                    "answers" : [
                                                                                        {
                                                                                            "uuid" : "2d37695e-a212-4a92-9b19-70693cbfd4d6",
                                                                                            "label" : "No",
                                                                                            "advice" : null,
                                                                                            "followUps" : []
                                                                                        },
                                                                                        {
                                                                                            "uuid" : "9dd082ed-e29f-43c6-bb8c-d29daebfa629",
                                                                                            "label" : "Yes",
                                                                                            "advice" : null,
                                                                                            "followUps" : []
                                                                                        }
                                                                                    ],
                                                                                    "answerItemTemplate" : null,
                                                                                    "references" : [],
                                                                                    "experts" : []
                                                                                }
                                                                            ]
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "8018e15f-6c7e-433a-8686-ae0d695c7fc7",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Does the domain specific format come with its own suite of integration tools that you will use??",
                                                                    "text" : "",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "fa12de49-cf29-4bcd-acf9-85eb39957d95",
                                                                            "label" : "No",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "231524b8-9133-4319-b633-f0c96871211d",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        },
                                                        {
                                                            "uuid" : "e8334918-27f8-4e1c-aa9f-5e8191b7fabb",
                                                            "label" : "A table or set of tables (consisting of 'data records')",
                                                            "advice" : null,
                                                            "followUps" : [
                                                                {
                                                                    "uuid" : "6bfde30e-90e7-4422-b984-d571196194e0",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Does each column have a header?",
                                                                    "text" : "In a table, the data items are arranged in columns. Is there a header for each of these describing what is in there?",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "595acdbc-f0d7-4550-9c56-9177f3c5695b",
                                                                            "label" : "No",
                                                                            "advice" : "You will need to have a header in order to make the data interoperable and reusable",
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "53d43c6a-d12e-4bfd-b70d-233bf62c7f5b",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : [
                                                                                {
                                                                                    "uuid" : "cacee0fd-7de5-4e07-aa94-1661cb853558",
                                                                                    "shortUuid" : null,
                                                                                    "type" : "QuestionTypeOptions",
                                                                                    "title" : "Are all column headers unambiguous?",
                                                                                    "text" : "A human being quickly 'understands' data items and their relations. For good data reusability, it is necessary that computers can understand your data too.",
                                                                                    "answers" : [
                                                                                        {
                                                                                            "uuid" : "1485ec8b-094c-49bb-8de1-ce214c83790f",
                                                                                            "label" : "No",
                                                                                            "advice" : "Check whether you can find an ontology for each of your data items",
                                                                                            "followUps" : []
                                                                                        },
                                                                                        {
                                                                                            "uuid" : "02b2c93a-5956-4c75-8797-d3803d4344e8",
                                                                                            "label" : "Yes",
                                                                                            "advice" : null,
                                                                                            "followUps" : []
                                                                                        }
                                                                                    ],
                                                                                    "answerItemTemplate" : null,
                                                                                    "references" : [],
                                                                                    "experts" : []
                                                                                },
                                                                                {
                                                                                    "uuid" : "6ab36a2a-23ca-484d-b3d7-0c1a4eea821a",
                                                                                    "shortUuid" : null,
                                                                                    "type" : "QuestionTypeOptions",
                                                                                    "title" : "Do all columns/headers have a data type?",
                                                                                    "text" : "A label like 'temperature' only makes sense to a computer if it is also clear what the units are and what temperature has been measured. In many cases, it is also important how it was measured.",
                                                                                    "answers" : [
                                                                                        {
                                                                                            "uuid" : "7108a5e7-f804-4410-b40d-cabed6b3da15",
                                                                                            "label" : "No",
                                                                                            "advice" : "Check whether you can find a data type (potentially in a data type registry) for each of your data items",
                                                                                            "followUps" : []
                                                                                        },
                                                                                        {
                                                                                            "uuid" : "d4a545a3-0981-461b-9762-7073e5750ea9",
                                                                                            "label" : "Yes",
                                                                                            "advice" : null,
                                                                                            "followUps" : []
                                                                                        }
                                                                                    ],
                                                                                    "answerItemTemplate" : null,
                                                                                    "references" : [],
                                                                                    "experts" : []
                                                                                },
                                                                                {
                                                                                    "uuid" : "c47d330a-2eb4-472e-be48-d06b3ec9b8e8",
                                                                                    "shortUuid" : null,
                                                                                    "type" : "QuestionTypeOptions",
                                                                                    "title" : "Are the limitations to allowed data values in each column explicit?",
                                                                                    "text" : "If there are reasonable limitations to the values in a column, or even a limited set of allowed values, it is very good for data vvalidation and reusability if these limitations are explicit, and e.g. software used for data entry and editing will not allow anything else.",
                                                                                    "answers" : [
                                                                                        {
                                                                                            "uuid" : "6bda04d8-1f32-4ace-ab8b-1121ea7c526e",
                                                                                            "label" : "No",
                                                                                            "advice" : "Check whether there is a way to explicitly limit data values.",
                                                                                            "followUps" : []
                                                                                        },
                                                                                        {
                                                                                            "uuid" : "2376500a-34cb-4535-868e-b7b126321f34",
                                                                                            "label" : "Yes",
                                                                                            "advice" : null,
                                                                                            "followUps" : []
                                                                                        }
                                                                                    ],
                                                                                    "answerItemTemplate" : null,
                                                                                    "references" : [],
                                                                                    "experts" : []
                                                                                }
                                                                            ]
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "0259866a-50b2-4f77-858f-982c9e16091d",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Is it clear what a row in the table represents?",
                                                                    "text" : "",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "b75a4215-357e-48d2-bb66-436da069bb00",
                                                                            "label" : "No",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "7341ed8c-c4d6-4562-808f-068bc47ec418",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "ccaaf562-ebed-4d94-b168-b8398ed25546",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Does each row have an identifier?",
                                                                    "text" : "",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "435f76fe-feff-436f-b987-61d65a73f22b",
                                                                            "label" : "No",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "b904da82-6aae-4f2e-8b5d-5359cf48939e",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "f90914d8-2731-4cd1-bbf8-2710a5e08c43",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Is there a distinguishing way a missing value in the table can be recognized?",
                                                                    "text" : "Sometimes, and empty field or a zero is indicating a missing value. But is that really unique? Could there be valid empty or zero fields? Has the convention for missing values been made explicit somewhere?",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "3cb15eb1-dfdc-4a1d-b208-42fe7264d5bd",
                                                                            "label" : "No",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "6daf9964-6a2a-46aa-8a87-c918391f8625",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "0451d421-4c97-452e-b399-4fe278b97147",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Is the relation between each of the columns and the record identifier clear?",
                                                                    "text" : "It may appear that in a table with 'patients' as rows, a column labeled 'disease' coupled to an ontology has a clear meaning. But that is not always explicit enough! A 'disease' could e.g. be the disease that the patient is suffering from, but it could also be an earlier diagnose, a suspected diagnose, or the disease a family member recently died of.",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "3ab6d840-fd16-4ff4-bd2b-77139384b54e",
                                                                            "label" : "No",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "a2aa695e-4ea9-4dc6-81f3-cb8c9605bf98",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                },
                                                                {
                                                                    "uuid" : "bdaba73c-02e0-4ff1-acb3-a68df891c36a",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Are all the relations between the column headers explicit?",
                                                                    "text" : "For a good understanding of tabular data, you need to make the relationship between each pair of columns explicit. E.g. if one column is 'disease' and another is 'treatment', you want to make sure that this is the chosen treatment that this person is undergoing for the given disease.",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "b7118fba-770a-42cb-abe1-d243d0b624b4",
                                                                            "label" : "No",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "7dcc6fb0-e591-4f87-87fa-166ae839193a",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        },
                                                        {
                                                            "uuid" : "d7911ab4-d3cc-4dc2-95cc-4aba7b0d010a",
                                                            "label" : "Complex data, like a graph",
                                                            "advice" : null,
                                                            "followUps" : [
                                                                {
                                                                    "uuid" : "46e07a92-2070-4212-96ea-8b98da488292",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeOptions",
                                                                    "title" : "Are you representing the graph in RDF?",
                                                                    "text" : "RDF is the graph representation used in the 'semantic web'",
                                                                    "answers" : [
                                                                        {
                                                                            "uuid" : "2662b7d3-b2ff-4944-b2ad-574cd92a97b5",
                                                                            "label" : "No",
                                                                            "advice" : null,
                                                                            "followUps" : []
                                                                        },
                                                                        {
                                                                            "uuid" : "3525f6f9-4c0b-4560-a572-c9fe2e0a0d94",
                                                                            "label" : "Yes",
                                                                            "advice" : null,
                                                                            "followUps" : [
                                                                                {
                                                                                    "uuid" : "3196c225-2842-4766-9768-381fd6ee1e05",
                                                                                    "shortUuid" : null,
                                                                                    "type" : "QuestionTypeOptions",
                                                                                    "title" : "Are all the URLs resolving to common ontologies?",
                                                                                    "text" : "",
                                                                                    "answers" : [
                                                                                        {
                                                                                            "uuid" : "945aae79-4eed-4115-ac19-d295158a19f6",
                                                                                            "label" : "No",
                                                                                            "advice" : null,
                                                                                            "followUps" : []
                                                                                        },
                                                                                        {
                                                                                            "uuid" : "88f9fb7b-12a9-481a-9239-cbf43206056e",
                                                                                            "label" : "Yes",
                                                                                            "advice" : null,
                                                                                            "followUps" : []
                                                                                        }
                                                                                    ],
                                                                                    "answerItemTemplate" : null,
                                                                                    "references" : [],
                                                                                    "experts" : []
                                                                                }
                                                                            ]
                                                                        }
                                                                    ],
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        },
                                                        {
                                                            "uuid" : "4e6d2cb0-1901-41dd-9902-6066739c9ab5",
                                                            "label" : "We have made other arrangements",
                                                            "advice" : null,
                                                            "followUps" : [
                                                                {
                                                                    "uuid" : "024bb661-70d4-4da6-a938-934a3935b16a",
                                                                    "shortUuid" : null,
                                                                    "type" : "QuestionTypeText",
                                                                    "title" : "Describe how?",
                                                                    "text" : "",
                                                                    "answers" : null,
                                                                    "answerItemTemplate" : null,
                                                                    "references" : [],
                                                                    "experts" : []
                                                                }
                                                            ]
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                }
                                            ]
                                        },
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "b98c7dbd-de67-4ee4-8d86-45989511b1d6",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you be using a workflow for data integration, e.g. with tools for database access or conversion?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "7623d103-5bd6-4407-81b0-aa6c7f0cbc48",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "eade1627-7100-413e-8330-32b186d70fd7",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "d3d63615-17d5-4eb4-9587-9e47d18fe2fd",
                                                "chapter" : "5.3"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "ff85db18-565c-4177-a397-9a8cf4c7b374",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you use a 'linked data' approach?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "01aaaf8a-ee94-4cd0-8e36-adcb7ac9d18d",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "9cea7792-3e4c-42a6-b7e0-1abbdc9e2a5b",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "811965f0-b1b7-4b4b-92a6-01cd83b08fa2",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Are your data sources using linked data?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "d2ae9b5f-db3f-4b9e-8e06-723f82ca4c2e",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "395c9651-9d92-48f7-b1fc-cdccbd681f57",
                                                                "label" : "Partly",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "054125ff-7968-4c48-9359-0192042037a5",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "fad678c7-99e4-48e2-a708-fe0dfa1381b8",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will you provide your results as semantically interoperable linked data?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "fb400dcd-28f2-4577-ba3e-e588d8307e07",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "d17db5ee-275f-4783-8340-c89bc130c86f",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [
                                                            {
                                                                "uuid" : "8946bf81-5eec-44fe-9e36-f2163ae43c2f",
                                                                "chapter" : "5.2"
                                                            }
                                                        ],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "7733494c-fd84-4cd0-8463-5806f321f2c9",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you be using common or exchangeable units?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "83883ae4-6c04-44af-a1a0-db5ee5a1b412",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "e2759b73-a0c6-4b49-b628-52a3e87605f0",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "f67e1539-5c7d-4df7-8259-e4f2edfa2685",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you be using common ontologies?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "b497f330-91a0-44c9-990d-3d3e02d380b4",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "3e5ef321-76f3-4c65-a49f-4e03f6a23def",
                                "label" : "Yes",
                                "advice" : "Choose the ontologies before you start",
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "d0306914-c253-472c-869f-19a60578c087",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will there be potential issues with statistical normalization?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "6332fd3a-6b3b-4db6-a5bf-f108f5c19801",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "49ad2eca-b88d-4fd8-b210-5bec6fffb283",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "dbb6dcdf-dd2e-4829-8c03-16b2339ebd5a",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you be integrating different data sources to get more samples or more data points?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "d59b9031-78e7-4d37-a667-1aeb94522213",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "493b4264-185d-413e-83f8-d424c50768b9",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "18f9b8ec-bf1b-4035-8eac-3fd329613a7c",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Have these been collected with sufficiently identical protocols?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "e902668c-d033-4720-a3a7-a7694731bff0",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "cce50ae0-08ff-43f3-835c-a494d5a56222",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "d5d21ad3-720c-4a6a-bd5a-09f14b15666f",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you be integrating different data sources in order to get more information for each sample or data point?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "4ba2c122-b948-4e41-9e48-6e052e2dc77e",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "61d05101-2eaa-4ebb-8be7-2fcac50f2fb2",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "63b020de-664a-4271-807c-a6f4361d6439",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Did you already select the variables on which you will join the data sets?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "176cf769-81db-4298-bfae-c1e96e8bbecf",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "f8f49bdc-670b-495f-9a4b-5300db71cfe2",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "c6fdeacb-4a1f-4948-988a-0c6458cdf7b9",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you make sure that you do not inadvertently create a biased subset?",
                                        "text" : "Some parameters you select on may have been collected only for a subset of the subjects or data points. An obvious example is if you match on secondary education type, you will bias to people over 18 years old because younger people do not have this field. In many cases the selection bias may be a lot less obvious and special measures exist to verify that the diversity of the sample is not reduced by the integration step.",
                                        "answers" : [
                                            {
                                                "uuid" : "0cf3e92f-4cdf-4446-b2c6-1801338ec432",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "26f64599-467a-4339-a971-74ac638f4c37",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "6b3d62a5-1d4d-49e1-aaf1-0a8b398a7ac3",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Could the coupling of data create a danger of re-identification of anonymized privacy sensitive data?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "04fe8749-82a5-4b06-a7f8-58e634e5cd86",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "aad55658-91c1-4fb8-8869-c0dc1b02b96d",
                                                "label" : "Yes",
                                                "advice" : "Plan either preventative or corrective measures",
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "15eb6fe6-7b21-493b-a5a0-09ac63212ab6",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Did you make a conscious decision to be either accurate or complete?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "46ec2d2b-d1c2-44cd-8a99-2866f65349d2",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "ff4b9f47-39f4-436f-89bf-910dda950380",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "dc4f1dc7-4608-45f9-8503-aa05536f4d97",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Do you have all tools to couple the necessary data types?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "45169011-2336-4eb4-8026-fa7fa9ca9649",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "fa0f8ecf-9cd5-4395-9228-0aeca4339ea4",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : []
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    }
                ]
            },
            {
                "uuid" : "b1df3c74-0b1f-4574-81c4-4cc2d780c1af",
                "title" : "Data design and planning",
                "text" : "In the data design and planning phase, we will make sure that we know what data comes when, that we have enough storage space and compute power to deal with it, and that all the responsibilities have been taken care of.",
                "questions" : [
                    {
                        "uuid" : "b08fe063-33f8-4380-b3a9-ba1e586dedf2",
                        "shortUuid" : null,
                        "type" : "QuestionTypeList",
                        "title" : "What data formats/types will you be using?",
                        "text" : "Have you identified types of data that you will use that are used by others too? Some types of data (e.g. genetic variants in the life sciences) are used by many different projects. For such data, often common standards exist that help to make these data reusable. Are you using such common data formats?",
                        "answers" : null,
                        "answerItemTemplate" : {
                            "title" : "Item",
                            "questions" : [
                                {
                                    "uuid" : "ecff019a-d4e6-44c6-a8fe-c84eb15ed8b7",
                                    "shortUuid" : null,
                                    "type" : "QuestionTypeOptions",
                                    "title" : "Is this a standard data format used by others too?",
                                    "text" : "",
                                    "answers" : [
                                        {
                                            "uuid" : "52b7aba3-d809-4a30-b3f8-90caa7a32a10",
                                            "label" : "No",
                                            "advice" : null,
                                            "followUps" : []
                                        },
                                        {
                                            "uuid" : "d4ce2c56-b3df-4013-9220-289b28f922de",
                                            "label" : "Yes",
                                            "advice" : null,
                                            "followUps" : []
                                        }
                                    ],
                                    "answerItemTemplate" : null,
                                    "references" : [],
                                    "experts" : []
                                },
                                {
                                    "uuid" : "ced5a7c2-4034-4763-a1d5-3cb815cdfddb",
                                    "shortUuid" : null,
                                    "type" : "QuestionTypeOptions",
                                    "title" : "Does this data format enable sharing and long term archiving?",
                                    "text" : "Complicated (binary) file formats tend to change over time, and software may not stay compatible with older versions. Also, some formats hamper long term usability by making use of patents or being hampered by restrictive licensing",
                                    "answers" : [
                                        {
                                            "uuid" : "86e547b8-e3ae-44b2-89ad-3a0d67decc56",
                                            "label" : "No",
                                            "advice" : null,
                                            "followUps" : [
                                                {
                                                    "uuid" : "bf32374c-b24e-4b03-9898-a753cff56fcf",
                                                    "shortUuid" : null,
                                                    "type" : "QuestionTypeOptions",
                                                    "title" : "Will you convert to a file format more suitable for archiving later?",
                                                    "text" : "",
                                                    "answers" : [
                                                        {
                                                            "uuid" : "3159d107-d322-4126-958f-900fc011eeae",
                                                            "label" : "No",
                                                            "advice" : null,
                                                            "followUps" : []
                                                        },
                                                        {
                                                            "uuid" : "7d8dc2f5-2699-4fb3-8bf6-e620868a0583",
                                                            "label" : "Yes",
                                                            "advice" : "You may need to reserve time and budget for this",
                                                            "followUps" : []
                                                        }
                                                    ],
                                                    "answerItemTemplate" : null,
                                                    "references" : [],
                                                    "experts" : []
                                                }
                                            ]
                                        },
                                        {
                                            "uuid" : "985e488d-257f-479d-8113-5dcbe105a08e",
                                            "label" : "Yes",
                                            "advice" : null,
                                            "followUps" : []
                                        }
                                    ],
                                    "answerItemTemplate" : null,
                                    "references" : [],
                                    "experts" : []
                                }
                            ]
                        },
                        "references" : [
                            {
                                "uuid" : "ee5c094c-6a70-4e32-9a51-f1af339f180f",
                                "chapter" : "2.1"
                            }
                        ],
                        "experts" : []
                    },
                    {
                        "uuid" : "5ba53879-eb48-47f2-a73b-f7f7d83bf030",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you be using new types of data?",
                        "text" : "Sometimes the type of data you collect can not be stored in a commonly used data format. In such cases you may need to make your own, keeping interoperability as high as possible.",
                        "answers" : [
                            {
                                "uuid" : "c470de83-0a9b-4d7d-a2cd-903bfbedc8d1",
                                "label" : "No, all of my data will fit in common formats",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "bf76127e-0497-46eb-8b5d-e4cfcb84f622",
                                "label" : "Yes, I will need to use custom formats for some of my data",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "ae235844-e3b6-461c-8c1d-668148dbf53b",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you need to add fields in your data format to a data type registry?",
                                        "text" : "Even if the data format you are using is unique to your project, the discrete data items should be reused or reusable as much as possible. Data type registries can help with that.",
                                        "answers" : [
                                            {
                                                "uuid" : "48ed91ec-5dbc-497a-9f84-f26daa60c0bd",
                                                "label" : "No, all of my data types are described in a data type registry already",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "e4fdba2a-f6e6-4087-99a6-0e00c1bcd5f7",
                                                "label" : "Yes, I will add new types to an existing data type registry",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "28c5197f-a3fe-4b96-b0fb-83e271155a95",
                                                "label" : "Yes, I will create my own data type registry",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "7fe217e0-9af0-422f-a45b-6873f0344bcd",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeText",
                                        "title" : "Which data type registries will you use?",
                                        "text" : "",
                                        "answers" : null,
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "b211f896-b363-4978-9423-212644ff5d72",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Do you need to create vocabularies or ontologies for any of your data items?",
                                        "text" : "For good interoperability the use of controlled vocabularies for each discrete data item is advisable. If such vocabularies exist, it is best to reuse those.",
                                        "answers" : [
                                            {
                                                "uuid" : "efe50d9c-4dc2-49c5-ae61-1dc8ff0d7655",
                                                "label" : "No, suitable public controlled vocabularies or ontologies exist for all of my data types",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "668ecd34-4413-4aa9-8921-616325766ec0",
                                                "label" : "Yes, I will make and publish a vocabulary or ontology for some of my data",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "79c3b4ff-e265-412f-9c6d-8be6310094d1",
                                                "chapter" : "2.2.2"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "a9103296-915a-4bdf-b259-116775d5e676",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "How will you design your new data format?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "bda912f3-14a7-4242-b293-025f1b19892d",
                                                "label" : "There is a closely related more generic and open format that I can specialize",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "b4334e27-3e6c-4215-b4cb-f60eabd9c61e",
                                                "label" : "I will use a Linked Data format",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "974fa3ea-4047-4006-a49c-f7972bf9c961",
                                                "label" : "I will use a completely custom data format",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "88001c99-0082-4291-8c54-140f2ddc5112",
                                                "chapter" : "2.2.3"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "caba60b1-57b3-4288-8d3e-aa3542c28027",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you describe your new data format for others?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "057e84be-3329-4fa2-9cc8-d37f53a44cd0",
                                                "label" : "No, this is not needed",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "252757f4-e4ed-4c3f-8c13-b14f55133dfe",
                                                "label" : "Yes, I will register my standards at a data standards registry",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [
                            {
                                "uuid" : "fd42abc4-5567-456b-b5b5-0ef90ab6415f",
                                "chapter" : "2.2"
                            }
                        ],
                        "experts" : []
                    },
                    {
                        "uuid" : "8c962e6f-17ee-4b22-8ebb-9f06f779e3b3",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "How will you be storing metadata?",
                        "text" : "For the re-usability of your data by yourself or others at a later stage, a lot of information about the data, how it was collected and how it can be used should be stored with the data. Such data about the data is called metadata, and this set of questions are about this metadata",
                        "answers" : [
                            {
                                "uuid" : "f9dd59ef-6af1-4b48-be9d-97102b25f0c8",
                                "label" : "Skip",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "6d18bfff-0f53-469b-934e-9806dda9d4fb",
                                "label" : "Explore",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "6226d7f2-565f-4991-94b3-c00be6aca20e",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Do suitable 'Minimal Metadata About ...' (MIA...) standards exist for your experiments?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "a23664b1-a17c-41fe-855c-db274984b355",
                                                "label" : "No",
                                                "advice" : "Did you really check a service like fairsharing.org to verify this?",
                                                "followUps" : [
                                                    {
                                                        "uuid" : "44a9d6c0-f96c-47d2-8c7a-902a9c222067",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Do you have a good idea of what metadata is needed to make it possible for others to read and interpret your data in the future?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "49e35bb1-6145-4b44-b390-9e1fe896415e",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "b3acec51-6385-43f1-a28a-123ab28b798f",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            },
                                            {
                                                "uuid" : "08bd1365-7f0e-4b78-aeb1-3c0e85e84cb6",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "ed872b74-6bf7-4cba-8f36-4da4509e4a4f",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Do you know how and when you will be collecting the necessary metadata?",
                                        "text" : "Often it is easiest to make sure you collect the metadata as early as possible.",
                                        "answers" : [
                                            {
                                                "uuid" : "bc41fb1c-e64b-44cd-b0d6-45aebc3a9b84",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "2fb268e0-2c25-475a-b7e1-7794a05a3091",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "15573266-7ddf-4c6b-962e-45691d34cf61",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will you consider re-usability of your data beyond your original purpose?",
                                        "text" : "Adding more than the strict minimum metadata about your experiment will possibly allow more wide re-use of your data, with associated higher data citation rates. Please note that it is not easy for yourself to see all other ways in which others could be reusing your data.",
                                        "answers" : [
                                            {
                                                "uuid" : "b2ca0d26-7fdd-44f1-b064-bc4cd4c45661",
                                                "label" : "No, I will just document the bare minimum",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "cb765913-50c9-482e-bf10-5f5f91d0e473",
                                                "label" : "Yes, I will document more metadata than needed for reproducibility",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "73bf7531-3490-4c0f-b6a2-a9f4f2bc3e1a",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "How will you balance the extra efforts with the potential for added reusability?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "3002eaed-c31f-4bdd-83ed-49262ae79165",
                                                                "label" : "I will see what I can do",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "7ef32aea-b6a7-4297-92bd-0869a13276ad",
                                                                "label" : "I will use preselected additional standard modules of metadata",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "018fc8a0-c51e-48f4-99d5-77e8c9dcf51f",
                                                                "label" : "I will collect all metadata I can gather and document the data set beyond minimal standards",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "c961b59a-b635-4a77-87ba-bc1b449bb012",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Do you need to exchange your data with others?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "c0d4739d-2694-4fe2-a6d6-d7cc073fc9eb",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "fb7addc1-c3ba-4eeb-973c-109aeef11ced",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "02b3fed1-0b50-4a80-b8b6-a225a1107022",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Did you consider how to monitor data integrity?",
                                        "text" : "Working with large amounts of heterogenous data in a larger research group has implications for the data integrity. How do you make sure every step of the workflow is done with the right version of the data? How do you handle the situation when a mistake is uncovered? Will you be able to redo the strict minimum data handling?",
                                        "answers" : [
                                            {
                                                "uuid" : "e36f5a68-860e-41fa-9158-b785317610ff",
                                                "label" : "Skip",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "8608d3df-7d7f-4893-8f80-e5297167b213",
                                                "label" : "Explore",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "e0759fdc-7ce9-4020-816d-73119f634c7e",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will you be keeping a master list with checksums of certified/correct/canonical/verified data?",
                                                        "text" : "Data corruption or mistakes can happen with large amounts of files or large files. Keeping a master list with data checksums can be helpful to prevent expensive mistakes. It can also be helpful to keep the sample list under version control forcing that all changes are well documented.",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "637f4603-0a24-47d1-a2f7-96958b4841fd",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "e7a8b914-54f3-4a72-9136-74a99db73926",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "acb86509-e93a-4d87-a958-74def0678e10",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will you define a way to detect file or sample swaps, e.g. by measuring something independently?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "4bd98e96-3d02-4967-b56b-d94a7b0079c6",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "ec3718dd-c8f4-417d-91ca-93408fa89953",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "2eda3f51-e6f5-40a3-b8d0-8059973320b6",
                                                "chapter" : "2.3.1"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "ae28a862-5020-44c2-8c78-3abc185b190f",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Do all datasets you work with have a license?",
                                        "text" : "It is not always clear to everyone in the project (ad outside) what can and can not be done with a data set. It is helpful to associate each data set with a license as early as possible in the project. A data license should ideally be as free as possible: any restriction like 'only for non-commercial use' or 'attribution required' may reduce the reusability and thereby the number of citations. If possible, use a computer-readable and computer actionable license.",
                                        "answers" : [
                                            {
                                                "uuid" : "0b0e1e89-3b7b-4d9e-95f6-d96397b28c3b",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "6110d79f-1cf0-4a89-8a6d-84a472388fd8",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "0551adc9-7faa-4ddc-a07b-08fbde2a247b",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will you store the licenses with the data at all time?",
                                                        "text" : "It is very likely that data will be moved and copied. At some point people may lose track of the origins. It can be helpful to have the licenses (of coarse as open as possible) stored in close association with the data.",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "e95aca23-fa1b-4e91-b6f3-748aed0d0925",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "245e5ab0-baa7-48b3-97b8-55d2af5e4fa8",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [
                                                            {
                                                                "uuid" : "6b372aca-a25b-4cf6-b67f-21d03d8516f0",
                                                                "chapter" : "2.3.2"
                                                            }
                                                        ],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "daa06d1a-6314-4c6d-93a8-69954c7f5c54",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "How will you keep provenance?",
                                        "text" : "To make your experiments reproducible, all steps in the data processing must be documented in detail. The software you used, including version number, all options and parameters. This information together for every step of the analysis is part of the so-called data provenance. There are more questions regarding this in the chapter on data processing and curation.",
                                        "answers" : [
                                            {
                                                "uuid" : "5480e0b8-8e0b-4b66-929f-eef3b89e6113",
                                                "label" : "All steps will be documented in an (electronic) lab notebook",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "32a1e3ef-6aa8-4eae-a517-b1dcf866d118",
                                                "label" : "Our work flow system documents the provenance automatically and completely",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "8e886b55-3287-48e7-b353-daf6ab40f7d8",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "How will you do file naming and file organization?",
                                        "text" : "Putting some thoughts into file naming can save a lot of trouble later.",
                                        "answers" : [
                                            {
                                                "uuid" : "1ff5080d-ffdc-4eda-8ad8-4e6b69f01a07",
                                                "label" : "Skip",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "c05f27a2-30ac-44fd-9ac9-cd6f62b16d0c",
                                                "label" : "Explore",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "d4fe0b55-4aee-4d05-88d4-a3f4cad2cfa9",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Did you make a SOP (Standard Operating Procedure) for file naming?",
                                                        "text" : "It can help if everyone in the project uses the same naming scheme.",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "f449e055-8f13-495c-aea9-7440800f6a45",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "8b13234e-879b-4221-be12-4df24e6de00e",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "37d0367b-3b69-4b3e-9303-71e628b1f360",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will you be keeping the relationships between data clear in the file names?",
                                                        "text" : "Advice: Use the same identifiers for sample IDs etc throughout the entire project.",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "9beb6069-5477-4b99-b28c-921cf38e8470",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "8032494b-e0cb-4fe5-94e7-d8a203292537",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "7dfd022e-6d6e-4923-a81a-ca1f11024b57",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will all the metadata in the file names also be available in the proper metadata?",
                                                        "text" : "The file names are very useful as metadata for people involved in the project, but to computers they are just identifiers. To prevent accidents with e.g. renamed files metadata information should always also be available elsewhere and not only through the file name.",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "df53b7a2-1b1e-4f5d-99df-b9a7a1da91e9",
                                                                "label" : "No, the file names in the project are an essential part of the metadata",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "2f695e7c-0661-4b43-ba55-1a82a2c706ef",
                                                                "label" : "Yes, all metadata is also explicitly available elsewhere",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [
                            {
                                "uuid" : "27e5a2b0-7512-47b0-bc82-2c097eb2f623",
                                "chapter" : "2.3"
                            }
                        ],
                        "experts" : []
                    },
                    {
                        "uuid" : "d5784d24-0e66-4821-bd62-a711fb6d7a40",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you be archiving data (using so-called 'cold storage') for long term preservation?",
                        "text" : "Much of the raw data you have will need to be archived for your own later use somewhere. This is often done off-line on tape, not on the disks of the compute facility. Please note that this does not refer to the data publication; questions related to data publication will follow later.",
                        "answers" : [
                            {
                                "uuid" : "a547dc69-a3e8-46e4-bb4b-21fd104231f9",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "b16e76a9-34a8-4ba5-9420-b58bebaeac8d",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Can the original data be regenerated?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "5e55c2ce-614a-4af8-ab92-972e8f5baea7",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "8d1f370f-6e4f-46ae-a169-1f7e969335e1",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "fd49ed4f-4e3a-4cf7-9857-8ff20fe1fbcf",
                                                "chapter" : "2.5.1.2"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "7f8ff791-8593-4e00-93f9-e31c5dff5ffb",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "When is the raw data archived?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "fbe7ef0d-3199-47de-ad28-1b9299200156",
                                                "label" : "As soon as it comes in, in chunks",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "92dc2ba0-2cac-4710-8dc7-c1cf6e573235",
                                                "label" : "As soon as it has all arrived, in one session",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "c861d005-a681-4a3a-a54b-7a229b6662ee",
                                                "label" : "All at once with the results at the end of the project",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            },
                            {
                                "uuid" : "e81599af-4519-4987-b74e-9545428ed0e3",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "c659c36f-d142-4453-8d8b-db7c05f31371",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Is the archived data changing over time, needing re-archival?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "86e40411-deee-4dc0-88dc-a257c7f4a3ad",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "48d7128a-a237-4d32-8fc6-464089b46892",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "1147485f-b9a9-486c-8341-27e05718c508",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Do you need frequent backups?",
                                                        "text" : "The general term 'backup' is used for protection against two different kinds of problems: equipment failure and human error. Protections against these two may need different solutions. Both are considered backup for this and subsequent questions.",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "e2ad7b2b-cc9a-445b-8a94-c41032a7e787",
                                                                "label" : "No, data changes infrequently",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "67d52405-a902-4661-8158-91ab1d268832",
                                                                "label" : "Yes, data changes frequently",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "f6476ea3-1ec5-447b-a776-4f0bfca38f8f",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will you be relying on these backups to recover from human error (accidental changes or deletions)?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "6f307c89-c1b2-4dcc-8315-585a65f08a87",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "899b6dc9-59fa-4246-b152-915d4864f771",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "a4278d17-062e-42c2-affc-911baee339d4",
                                                "chapter" : "2.5.1.3"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "c2a38110-f7a3-432f-ae98-4e448e496b44",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will the archive be stored on disk or on tape?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "18f50141-094d-426b-8a75-4c4fce59389b",
                                                "label" : "Disk",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "ebbe4ef1-ec2a-44b8-bbc9-f15a743c438f",
                                                "label" : "Tape",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "45ad8d8d-4efc-47b1-946d-2de59aa1fcb4",
                                                "label" : "Other",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "41a8d19b-2468-4c60-9f5e-7a8fee26eedb",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will the archive be stored in a remote location, protecting the data against disasters?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "a15afda5-9e5a-40bd-a802-fc811fa39d20",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "4bc5017e-2808-4fc1-95b9-77bd2e755bb6",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "ed3d43ad-d2b5-4194-8b05-08c95da0a7f2",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will the archive need to be protected against loss or theft?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "f312f518-69cb-442b-95a4-9523a25770db",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "29498f8c-ea29-4855-82db-5b1228dc09ba",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "2c797727-559d-4bcf-a4a1-713accfcd602",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will the archive be encrypted?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "5e4bb06c-01ff-4964-890f-6f56bc4b819a",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "2a127f74-c296-40f9-8ff5-d2a5a0b4ab98",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "905d4cda-afa1-4cc0-8df6-3d989c7b8b2e",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Is it clear who has access to the key? Also in case of a required data restore?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "68a83934-190b-4e33-9ac0-138e34e5a206",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "05e2cbc6-fdac-4f74-b6e6-733012da2657",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "7458c3bc-60aa-44ba-8d59-a0672dc8de63",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Is it clear who has physical access to the archives?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "e9ab764f-e3fa-4704-9200-4d527a54f754",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "7658b228-018a-4eba-9970-f5bb2c4de70a",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "87abded2-1edb-4f69-bd6c-8b90f7706206",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will your project require the archives to be available on-line?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "bb1ba41d-9b1b-45dd-bdc7-c7b8ea451ab5",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "25b31312-053b-4ef8-ae37-c44b7182634d",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "b3caa015-cdd5-4812-84b5-596855a30545",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will data integrity be guaranteed?",
                                                        "text" : "If the 'master copy' of the data is available on line, it should probably be protected against being tampered with.",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "60ea11ca-2179-4f7d-9587-bd22c22e4daa",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "c5a2baff-b326-483f-834c-7f2b39979ea2",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "86459c42-89ee-4874-bb54-fec0881332e0",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Is there an interface and a defined process for people to request access to the data?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "1ec0c5b1-b905-4bb4-aa56-45f7321258c1",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "01322e43-a6ae-458c-9a01-5b70bd3b9be9",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "5365f996-a83c-4e0c-b554-e7ac276c6a19",
                                                "chapter" : "2.5.3"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "b7d9008e-9691-44f0-a026-0f63d808db39",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Has it been established who has access to the archive, and how fast?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "dc7c7529-58c6-4414-84cc-7a19325cb73b",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "d748ea2b-bbe7-4b05-82e2-bee5e10e8441",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "47559455-2969-41f4-aee8-e9ce558ab2f0",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Has it been established who can ask for a restore during the project?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "0591c012-e567-4383-b39f-22b0abfe0847",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "c406aebf-a807-4bda-bcdf-5066d0b1c17b",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "3d837696-3a99-4362-86a3-f460dd878b75",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "If the data is voluminous, will the project be able to cope with the time needed for a restore?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "acf31ff1-9d81-4671-89fb-fa4c60c5845a",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "dfb89c83-afc2-481e-895c-bff5468d6daf",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "feb954c1-c38e-4615-addd-4c714a06809a",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "has authority over the data been arranged for when the project is finished (potentially long ago)? Is there a data access committee?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "79bf552f-4230-4026-9976-984ecdb8d889",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "aad68118-2159-4871-8d35-aeab6e66314e",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "2dd94cbc-42f3-4077-b6da-83e0275290c5",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Has it been established how long the archived data need to be kept? For each of the different parts of the archive (raw data / results)?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "1d8e9897-3713-4480-98ba-9b1c71f41eaf",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "07fdfbfb-9c7c-4e9c-83fa-bccf85094fb6",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "bd56375c-4b3a-40d0-af06-7dfb66025150",
                                                "chapter" : "2.5.5"
                                            }
                                        ],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "e23201aa-df56-4ade-9406-baa1fb6b1c0c",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will the data still be understandable after a long time?",
                                        "text" : "See also all questions about keeping metadata and data formats. Make sure the metadata is kept close to the data in the archive, and that community supported data formats are used for all long term archiving.",
                                        "answers" : [
                                            {
                                                "uuid" : "2e2460d5-5233-434b-9229-7cd775cb13b2",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "31de7c36-5d2c-469a-82b8-af870cb3560c",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [
                                            {
                                                "uuid" : "6171537c-de59-4ce7-8042-387bad700619",
                                                "chapter" : "2.5.6"
                                            }
                                        ],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [
                            {
                                "uuid" : "c6e67e46-6f79-402b-adba-3d5a56346534",
                                "chapter" : "2.5.1.1"
                            }
                        ],
                        "experts" : []
                    },
                    {
                        "uuid" : "72099c46-16e7-47a2-a320-cc768b7085fe",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Will you need a shared working space to work with your data?",
                        "text" : "",
                        "answers" : [
                            {
                                "uuid" : "1d6ba101-fd62-43c8-ab53-76bc31847f58",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "c1e83bdf-5914-44c2-8414-0a4e7fbaf272",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Are data all project members store adequately backed up and traceable?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "0c93a407-b04a-4c89-a8e4-299a4d77c21e",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "ec92df2d-f6db-4cc8-8317-ae24ab4fa08a",
                                                "label" : "Yes, protected against both equipment failure and human error",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            },
                            {
                                "uuid" : "e7324e1a-bfcb-4f2b-855a-35701dfb1ad5",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "df36fb68-131c-4f31-a42b-684abf523bbc",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "How will you work with your data?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "9ad3ed65-7215-465a-b31a-f5a11fc6abdc",
                                                "label" : "Skip",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "5af813b7-cb41-4eb9-afe7-ca364771b74e",
                                                "label" : "Explore",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "73bfcfcf-a619-4f6a-a901-2e8ce01d039a",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeText",
                                                        "title" : "What kind of data will you have in your work space?",
                                                        "text" : "When making the work space, it helps to know whether you expect to work with very many small files, a few very large files, whether you will use a (SQL) database to store most of the data. Maybe your data is suitable for a system like Hadoop? Such information can be collected here.",
                                                        "answers" : null,
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "f6e66f4f-7b11-420f-a4b6-5fc972bc66d9",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Do you need the work space to be close to the compute capacity?",
                                                        "text" : "If you have large volumes of data that are intensely and repeatedly used by the computing work flow, it may be needed to keep the storage in the same place as where the computing takes place.",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "12f3cb44-db3c-4ea8-b4fb-2834a11da6a7",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "4cf287dd-6a24-4768-93b4-dd4a7767afb8",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [
                                                            {
                                                                "uuid" : "afc5ae15-69ef-4dd9-b701-eaa5dbb8fddb",
                                                                "chapter" : "2.7"
                                                            }
                                                        ],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "50f9d580-3e62-434b-81ff-86daed56aca8",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will you be working with your data in another form than the way it will be archived?",
                                                        "text" : "Archival and working with data have different requirements. You want archived information to be in a form that others could read and in a format that is also understandable in a number of years. When working with the data, you need to be able to address it efficiently. If the two differ, you need to plan for conversions.",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "df08a7fa-61af-418f-9eb8-965ba50efcc8",
                                                                "label" : "No, data format will be archived in the same way we work with it",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "02b8d129-3300-47cb-811a-e86a40c5d8bd",
                                                                "label" : "Yes, archival will require a conversion step",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "4ee4d67d-62c5-47f0-a741-f2629b65cf8a",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "How does the storage need change over time?",
                                                        "text" : "To perform capacity planning, it is important to know what the need for storage capacity at the beginning and the end of the project will be.",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "eef277cb-1032-401e-b242-e74afde575dc",
                                                                "label" : "Storage needs will be the same during the whole project",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "014d23a8-93a8-4fd2-ba1c-8656177f37b2",
                                                                "label" : "Storage needs are large at the beginning and will be reduced later",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "ab40db7f-ea89-4830-b507-4f2f08a9453e",
                                                                "label" : "Storage needs are small at the beginning and will grow later",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "51a8248e-8659-47fd-a8e3-112eed64f65b",
                                                                "label" : "Storage needs are largest in the middle of the project",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "aca843cd-e38e-4174-8ce1-477c2b405fdf",
                                                                "label" : "Explore",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "8526a988-9561-42c6-8af4-7c96aa4c536d",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "When will your raw data become available?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "adfc7ca3-c8e3-465b-8eb2-4327b8780684",
                                                                                "label" : "Raw data will come in right at the start",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "19755f9d-8ec2-4fd9-8bdf-b25d5889d7ee",
                                                                                "label" : "Raw data will come in during the project",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "01cd836b-915e-4751-b543-b326bf812594",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "How much of the raw data do you need to keep in the work space?",
                                                                        "text" : "Sometimes the raw data is relatively large, and it pays of to clean it quickly.",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "fe62c9a0-a621-46b6-bc42-881d0c3d8719",
                                                                                "label" : "Raw data will need to stay in the work space",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "aad7d438-a8af-4462-9728-77e2493bcacc",
                                                                                "label" : "Raw data can be cleaned out or archived quickly",
                                                                                "advice" : null,
                                                                                "followUps" : [
                                                                                    {
                                                                                        "uuid" : "ba381732-f499-419f-8744-e13807af83e3",
                                                                                        "shortUuid" : null,
                                                                                        "type" : "QuestionTypeOptions",
                                                                                        "title" : "Do your raw data need to be archived?",
                                                                                        "text" : "",
                                                                                        "answers" : [
                                                                                            {
                                                                                                "uuid" : "904859c4-b9bf-44b6-9a64-933e489089e6",
                                                                                                "label" : "No, it is also stored elsewhere can be recovered easily",
                                                                                                "advice" : null,
                                                                                                "followUps" : []
                                                                                            },
                                                                                            {
                                                                                                "uuid" : "e4da5030-9de8-4fe9-81d9-28a75edc09d9",
                                                                                                "label" : "No, it can be remeasured easily and more cheaply than archiving",
                                                                                                "advice" : null,
                                                                                                "followUps" : []
                                                                                            },
                                                                                            {
                                                                                                "uuid" : "44cad909-ea78-454b-aede-794503da9401",
                                                                                                "label" : "Yes",
                                                                                                "advice" : null,
                                                                                                "followUps" : []
                                                                                            }
                                                                                        ],
                                                                                        "answerItemTemplate" : null,
                                                                                        "references" : [],
                                                                                        "experts" : []
                                                                                    }
                                                                                ]
                                                                            },
                                                                            {
                                                                                "uuid" : "93555e7b-d17c-4341-9ad9-9f2f833a3193",
                                                                                "label" : "Raw data do not form a major part of the storage needs",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "0ddd0082-eae1-4c48-ba4a-30912f0643e7",
                                                                                "label" : "Raw data will only be loaded in the work space on demand from the primary archives",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "c0944ac9-f286-48f2-b864-4641d0fcdd55",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Did you plan how much intermediate data you will get during analysis and how long each step needs to be kept?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "96b1aa0f-56db-4613-b424-3fe33349386b",
                                                                                "label" : "The volume of intermediate data will not be significant",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "9a0cfd22-adce-46e6-b7ec-8ed581ac95be",
                                                                                "label" : "A large volume of intermediate data will be in the work space",
                                                                                "advice" : null,
                                                                                "followUps" : [
                                                                                    {
                                                                                        "uuid" : "deb647ec-a001-4370-87b8-a5b1e2479f87",
                                                                                        "shortUuid" : null,
                                                                                        "type" : "QuestionTypeOptions",
                                                                                        "title" : "Is it possible to store intermediate temporary data on a separate (scratch) file system that is not backed up?",
                                                                                        "text" : "If the intermediate results are in your main work space, a restore in case of a problem could take much more time. It may be faster to recover it by re-running computations",
                                                                                        "answers" : [
                                                                                            {
                                                                                                "uuid" : "55bab6f7-6ab9-4eb7-ae2b-1947cce2da84",
                                                                                                "label" : "We will use the main work space for temporary data",
                                                                                                "advice" : null,
                                                                                                "followUps" : []
                                                                                            },
                                                                                            {
                                                                                                "uuid" : "5cbefa56-bc0c-4704-a9a4-3535ec513057",
                                                                                                "label" : "We can offload intermediate results to a scratch file system that is not backed up",
                                                                                                "advice" : null,
                                                                                                "followUps" : [
                                                                                                    {
                                                                                                        "uuid" : "3a076e83-73b0-4cdd-bb71-c5d41469a191",
                                                                                                        "shortUuid" : null,
                                                                                                        "type" : "QuestionTypeOptions",
                                                                                                        "title" : "Are you sure you will not need a backup of the data stored on the scratch file systems (any scratch you use)?",
                                                                                                        "text" : "",
                                                                                                        "answers" : [
                                                                                                            {
                                                                                                                "uuid" : "fe80319d-f761-49ac-9faf-d4c5a9b349da",
                                                                                                                "label" : "No",
                                                                                                                "advice" : null,
                                                                                                                "followUps" : []
                                                                                                            },
                                                                                                            {
                                                                                                                "uuid" : "f4bf87cf-f5e6-4f06-adcf-0dcf9bb37e2a",
                                                                                                                "label" : "Yes",
                                                                                                                "advice" : null,
                                                                                                                "followUps" : []
                                                                                                            }
                                                                                                        ],
                                                                                                        "answerItemTemplate" : null,
                                                                                                        "references" : [],
                                                                                                        "experts" : []
                                                                                                    }
                                                                                                ]
                                                                                            }
                                                                                        ],
                                                                                        "answerItemTemplate" : null,
                                                                                        "references" : [],
                                                                                        "experts" : []
                                                                                    }
                                                                                ]
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "89f78871-68bd-4190-9ee7-57ec3b4c2356",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Will you have different versions of intermediate data that need to be kept?",
                                                                        "text" : "Consider storing only the workflow parameters if the data itself could be reproduced quickly",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "28452352-0aea-43ee-9b22-f0131d7eba51",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "ee686b11-0a50-4845-baae-d3ed3b3f5fcc",
                                                                                "label" : "Yes",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "50a9c060-91e9-4e34-8cb2-2d41fa493f9e",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Will you need to temporarily archive data sets (e.g. to tape)?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "09ac9b88-4350-441b-97f4-5f85ced2ddf2",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "9797dc5a-afc9-418e-a456-0d555a52424a",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "f4065e54-d27a-45de-be4c-10384feacd0d",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "How will your first data come in?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "2271d2cd-de50-460b-85a1-dddf9d28776c",
                                                                "label" : "No special planning is needed for the initial data",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "9ad1d4fd-997d-45a8-8cc4-f594a8e4fdf8",
                                                                "label" : "Initial data will need to be made available through a local network copy",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "202c1232-9f5e-4a68-b0e0-3986a7bc96fa",
                                                                "label" : "We will need a high-speed network connection to copy the initial data",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "94aa40a4-9b14-4923-a26a-3523566329e3",
                                                                "label" : "Initial data will arrive on separate media and will need to be copied to the work space",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "2ed7b5c7-2452-4087-b4ed-d15ca31a4e65",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "How will project partners access the work space?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "fe6bacd4-f8dd-4940-b5c7-1a335e116531",
                                                                "label" : "Skip",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "54e9c073-52ee-4f8d-a1aa-1d279ec0f61d",
                                                                "label" : "Explore",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "b1faddd1-2f9f-48a8-b9a7-d8f6f8c02470",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Who will arrange access control?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "d45f521d-f541-4f40-a3d8-d4a9ecdc65e3",
                                                                                "label" : "No special provisions are needed",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "9b723841-1bd4-487c-b45c-f0a369330b3a",
                                                                                "label" : "Project management will need to be able to give people access",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "4bb35a94-e387-4460-bcf3-d638f248e52c",
                                                                                "label" : "The work space should be connected to a single-sign-on system",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "f99580d5-b3ea-498c-86ac-f7326bd999c2",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Will the work space storage need to be remote mounted?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "68be7c83-54ed-46d4-a77f-b99488665f1c",
                                                                                "label" : "No",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "6baa53fe-c657-4da3-bb65-9b71f7141c6d",
                                                                                "label" : "Yes, for occasional exploration",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "5ab6d069-f46c-4514-8114-79c9fe01a93f",
                                                                                "label" : "Yes, for actual computations, requiring high performance",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    },
                                                                    {
                                                                        "uuid" : "f14750e9-3a39-4aae-b2c8-845583934c1d",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Will data be copied out and in to the workspace storage by remote users?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "2c2cd046-89e0-4f55-bafa-92eb5f77c3a8",
                                                                                "label" : "No, this should not be allowed",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "8fa969a0-7477-41be-aefc-a54a8045c954",
                                                                                "label" : "Yes, occasionally",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "06a34c86-3c48-4fa4-8888-0ae6b613068f",
                                                                                "label" : "Yes, for actual computations, requiring high performance",
                                                                                "advice" : null,
                                                                                "followUps" : [
                                                                                    {
                                                                                        "uuid" : "6b3f13db-407a-44e0-9d23-e6dd6a20ad59",
                                                                                        "shortUuid" : null,
                                                                                        "type" : "QuestionTypeOptions",
                                                                                        "title" : "Are data integrity and reliability requirements also met by the other storage spaces used in the project?",
                                                                                        "text" : "",
                                                                                        "answers" : [
                                                                                            {
                                                                                                "uuid" : "c7a0181c-1921-4b21-9481-f4274d48876c",
                                                                                                "label" : "This is not needed",
                                                                                                "advice" : null,
                                                                                                "followUps" : []
                                                                                            },
                                                                                            {
                                                                                                "uuid" : "a1c5e3e9-d286-4e5b-8223-c7e6bbc6e333",
                                                                                                "label" : "Yes",
                                                                                                "advice" : null,
                                                                                                "followUps" : []
                                                                                            }
                                                                                        ],
                                                                                        "answerItemTemplate" : null,
                                                                                        "references" : [],
                                                                                        "experts" : []
                                                                                    }
                                                                                ]
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "b171ec17-d7b5-4726-ae22-0d892b106cf8",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "How available/reliable should must the work space be?",
                                        "text" : "There are a number of questions that can help you to decide whether your work space will be reliable enough for your project. Do you want to skip those questions now, or go through them?",
                                        "answers" : [
                                            {
                                                "uuid" : "c4f91550-c713-4066-b9e1-0419513820ba",
                                                "label" : "Skip this section",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "0d64d3c6-4312-47a9-92ad-0bc9440d6e45",
                                                "label" : "Explore this with more questions",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "25e06912-08a2-40e4-af76-cfbc5ada9925",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "What is the acceptable risk for a total loss?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "44810299-8618-4a79-a732-13cc2ff21903",
                                                                "label" : "This is unacceptable",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "766e16e4-21c7-4030-9eda-1973f51061d4",
                                                                "label" : "All essential data is also stored elsewhere",
                                                                "advice" : null,
                                                                "followUps" : [
                                                                    {
                                                                        "uuid" : "19640692-1c97-4574-8b3d-f4f6e1e6b564",
                                                                        "shortUuid" : null,
                                                                        "type" : "QuestionTypeOptions",
                                                                        "title" : "Is there software in the work space? Can it also be restored quickly?",
                                                                        "text" : "",
                                                                        "answers" : [
                                                                            {
                                                                                "uuid" : "7b572fda-9d67-44e9-b1c3-6dfc42c34d89",
                                                                                "label" : "There is no software",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "1217ef1b-01fd-48e6-900d-60edb99b4621",
                                                                                "label" : "Software in the work space is only a copy",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            },
                                                                            {
                                                                                "uuid" : "af2e1ca5-9a21-4c3b-a973-e9f41fd613da",
                                                                                "label" : "Special care will be taken for the software and configurations",
                                                                                "advice" : null,
                                                                                "followUps" : []
                                                                            }
                                                                        ],
                                                                        "answerItemTemplate" : null,
                                                                        "references" : [
                                                                            {
                                                                                "uuid" : "7184e426-c352-4ace-8448-e3bc8bbe17af",
                                                                                "chapter" : "2.6"
                                                                            }
                                                                        ],
                                                                        "experts" : []
                                                                    }
                                                                ]
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "0548db0c-bd34-40ce-be73-7a13a5e8eb7d",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Can you handle it when the work space is off line for a while?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "ef611a20-d5f9-4ea6-bb02-a7122bfd7eed",
                                                                "label" : "We could handle a few days of offline time per year",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "d96d5e47-aec8-4576-9e3a-734ab58dfe62",
                                                                "label" : "We can only miss the work space for a few hours during work hours",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "b6da4858-f72c-498b-a827-d4632a458098",
                                                                "label" : "Problems during the evenings and weekends can not wait for work hours to be repaired",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "6531dd45-f628-4aa7-a2c5-82c639b79ab8",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "How long can you wait for a restore if the storage fails?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "c9d8c9ed-ba2d-4108-b798-493cdf9225df",
                                                                "label" : "We can wait for repair and a restore",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "27db87e3-46b2-4705-b98c-47ddf4b4cd56",
                                                                "label" : "A spare copy needs to be deployed quickly",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "5797b252-0550-46e1-bf13-c65edd2df45e",
                                                                "label" : "No waiting is possible, a hot copy must be ready to take over",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "aacf28a0-aab9-4436-8b2b-95ef755ae32a",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "How long can you wait for a restore if you accidentally damage a file?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "42fb53ea-76ed-494b-8582-f080d952a3ed",
                                                                "label" : "Days",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "91463928-1cf4-43e0-bb4e-d1fc7b077ac8",
                                                                "label" : "Hours",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "a639ec31-e226-404e-97fb-567d2f52e321",
                                                                "label" : "Any user needs to be able to restore an old copy instantaneously",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "98d9789b-32fc-4e2f-876a-47760ad7c7ec",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Do you need to make copies of project data that is not in the work space?",
                                                        "text" : "Are there any data files e.g. on laptops of project members? Also: supercomputing centers and other high performance computer centers often write in their terms of use that you need to take care of your own backups",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "a1ead63a-1bc8-42e6-8bdc-3e1e8a901307",
                                                                "label" : "There is no data elsewhere",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "278810f9-6396-4203-9fa6-6cd3a003159c",
                                                                "label" : "All data elsewhere is adequately backed up",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "0884ac19-9bde-4bf1-afdd-0794c2c69092",
                                                                "label" : "We make (automated) backups of all data stored outside of the working area",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "614ab69d-55a6-4214-b384-00ba21ce92a1",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Is the risk of information loss, leaks and vandalism acceptably low?",
                        "text" : "There are many factors that can contribute to the risk of information loss or information leaks. They are often part of the behavior of the people that are involved in the project, but can also be steered by properly planned infrastructure.",
                        "answers" : [
                            {
                                "uuid" : "875ec70a-5169-43b6-8fa2-b6de51c26575",
                                "label" : "Skip this set of questions",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "b2f76c0a-847a-403c-9ed6-09cad10e625e",
                                "label" : "Explore the factors",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "90324a7b-f3c9-4ed8-b301-edc9869cb7b3",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Do project members store data or software on computers in the lab or external hard drives connected to those computers?",
                                        "text" : "When assessing the risk, take into account who has access to the lab, who has (physical) access to the computer hardware itself. Also consider whether data on those systems is properly backed up",
                                        "answers" : [
                                            {
                                                "uuid" : "17172079-ddce-40a7-b93a-f86dc6611452",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "7134876a-bd96-44dd-b483-a0089bb1e5be",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "abaf4a70-17d4-449c-9b95-5b3bcfed7e9b",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Do project members carry data with them?",
                                        "text" : "Does anyone carry project data on laptops, USB sticks or other external media?",
                                        "answers" : [
                                            {
                                                "uuid" : "3c8f999d-c962-42a4-a7c3-05b21a4029ab",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "c9fc7d94-0f37-40af-9d12-7c828d1252fc",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "6aff9a57-acea-4f6d-ac98-9a067e03cc8e",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Are all data carriers encrypted? Are accounts on the laptop password protected?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "546d6e41-80ea-417b-b0e3-a1d9d75a3794",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "dda8513e-431d-474e-b00d-107ae995e0ee",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "84ec46bd-e714-4d98-8525-cdd0d58777ef",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Do project members store project data in cloud accounts?",
                                        "text" : "Think about services like Dropbox, but also about Google Drive, Apple iCloud accounts, or Microsoft's Office365",
                                        "answers" : [
                                            {
                                                "uuid" : "099bfaa6-157a-49e5-8ef7-dad0876654f7",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "b5e75eac-3db1-4736-8b22-7f8ed99218ab",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "2d57daa6-df2e-4bf9-b024-710e74781d88",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Do project members send project data or reports per e-mail or other messaging services?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "1846d736-7f9c-4177-9e36-308afde00a5a",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "eed11bd7-45b8-4ca7-8c46-95c3977f2c22",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "a454fffd-f4a4-426d-b758-6d70bc4ee96b",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Do all data centers where project data is stored carry sufficient certifications?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "9acf74b1-e045-477d-be96-695692bb22c3",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "a9629ff2-30b2-4295-ae35-98320cfdb254",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "39d8dad9-7666-405c-b35c-9e430c9691ed",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Are all project web services addressed via secure http (https://)?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "2b934f9d-d849-4e22-8e0c-65dbf7b9dbeb",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "bb176104-3bf1-4bc8-b8e8-a4b09e14b88c",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "dc327e53-cf4f-44e7-8e66-40ddf1b4ae0a",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Have project members been instructed about the risks (generic and specific to the project)?",
                                        "text" : "Project members may need to know about passwords (not sharing accounts, using different passwords for each service, and two factor authentication), about security for data they carry (encryption, backups), data stored in their own labs and in personal cloud accounts, and about the use of open WiFi and https",
                                        "answers" : [
                                            {
                                                "uuid" : "d802675e-f76b-4714-87ee-1b328da826ab",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "4f86bb87-06f6-4d52-a9c0-52fc97bb441c",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "67c34ee1-2e71-423c-97a7-fe6aa3723465",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Did you consider the possible impact to the project or organization if information is lost?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "4c5d329b-6e18-4f4e-92a9-f0298adf38fe",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "a8a4dc7d-bf8c-44e3-b8b2-3e04f206f460",
                                                "label" : "Yes; the effect is small",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "a0ec7128-1c2d-4034-b621-211bd01a9839",
                                                "label" : "Yes; the risk is acceptably low",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "928db5fa-e16f-4c28-a321-ad62e1849b90",
                                                "label" : "Yes; we will need to work on this.",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "a20ddc5d-e883-4bc4-9948-36a2cb477d10",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Did you consider the possible impact to the project or organization if information leaks?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "04dd5577-367f-4c34-b90c-4bc25d80c819",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "7c9c8543-b8f8-47b0-8279-757dbc14922f",
                                                "label" : "Yes; the effect is small",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "6306207c-7be6-4cdf-86c4-33e0d9898479",
                                                "label" : "Yes; the risk is acceptably low",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "efec7b38-d9a9-4495-8ba7-625dd7da6a09",
                                                "label" : "Yes; we will need to work on this.",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "8cd49d8f-9af0-4ea9-985a-5d45142ac388",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Did you consider the possible impact to the project or organization if information is vandalized?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "b4dbc287-8eed-4799-800e-5ce0ede13cd8",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "ae0c3b93-f054-4052-91c0-fa9c518f5ca8",
                                                "label" : "Yes; the effect is small",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "b1e1861b-b55f-406f-b915-b440c64d8747",
                                                "label" : "Yes; the risk is acceptably low",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "e8943baf-4c0c-4f30-8b57-88c06f3bc4ad",
                                                "label" : "Yes; we will need to work on this.",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    },
                    {
                        "uuid" : "e87ef779-2c4b-4c0d-a1f1-821290123a3c",
                        "shortUuid" : null,
                        "type" : "QuestionTypeOptions",
                        "title" : "Do you need to do compute capacity planning?",
                        "text" : "If you require substantial amounts of compute power, amounts that are not trivially absorbed in what you usually have abailable, some planning is necessary. Do you think you need to do compute capacity planning?",
                        "answers" : [
                            {
                                "uuid" : "3cc05034-3c04-4f08-bd9b-c37e87fb9c81",
                                "label" : "No",
                                "advice" : null,
                                "followUps" : []
                            },
                            {
                                "uuid" : "ae82cfc8-6b5f-4f06-a969-d430db181ac5",
                                "label" : "Yes",
                                "advice" : null,
                                "followUps" : [
                                    {
                                        "uuid" : "30027b9a-2077-4c74-9048-f0532ada9b03",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Do you know how much CPU power, memory and I/O band width a typical analysis will take?",
                                        "text" : "Did you run pilot jobs? Do you know this information from comparable projects? Did you test whether the work scales up as you expected if you run more than one job?",
                                        "answers" : [
                                            {
                                                "uuid" : "ea6d837d-19b7-4203-8821-136ae24738c4",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "34954537-51c3-4592-9275-a504cdc3e587",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "dd918787-ffb1-499e-98f8-42050758c7ce",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "What type of compute architecture is most suitable for your work? Will you have that available?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "75be8d48-34ba-4ac5-ba2c-b9fd435fe705",
                                                "label" : "We will use a compute cluster",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "3a67d096-5028-48bf-8028-407db2cd3083",
                                                "label" : "We will use grid computing",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "6ce8d928-2bb9-4df1-8d08-cbea2fc355fd",
                                                "label" : "We will use cloud computing",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "3a79dfdf-be0c-4835-bb89-4e1bc899cfbc",
                                                "label" : "We will use a mix of computing architectures for different parts of the work",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "851a72ac-9497-4d8a-ae1a-6509d021a20b",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Is there sufficient experience with the chosen computer in the project, and sufficient time to support the project partners?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "b1bdced0-f4f2-48f4-907f-d5fed58e15a2",
                                                "label" : "We will recruit",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "2756894c-fe93-47da-ba87-3a31a06a1b70",
                                                "label" : "Training has been arranged",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "0fb8abbe-26fa-4756-a1e9-43476e5e1fd1",
                                                "label" : "Our people will be able to call for help",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "23ce8320-0b90-4360-a9db-6001ad9e7582",
                                                "label" : "We have sufficient knowledge in the project",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "d8150021-19d7-4e92-90c3-d9c3d62d188e",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Have you established with the provider when will you need the compute capacity?",
                                        "text" : "Do you need the compute capacity also for development? Can you start developing locally and start with a deployment test later?",
                                        "answers" : [
                                            {
                                                "uuid" : "a4e0eb91-aebf-4d5c-909c-b85029477939",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "9029ca4e-1e6b-450f-a7fd-9a9c7a7809e4",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "4c71d62c-3184-43d7-bdec-f762c682035e",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Is all required compute capacity available close to the project working storage area?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "c536e726-722c-428a-b11b-25c763da1a54",
                                                "label" : "No",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "a39fb276-8c46-49e7-bfb9-af8ba876726f",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Did you plan the required network capacity between storage and compute services?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "010b4ce6-0f52-437d-9971-681acdf37524",
                                                                "label" : "There are no special networking requirements",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "acec64f8-f20b-4295-85d6-a8831ee6a1a3",
                                                                "label" : "Copying or network delays are considered to be acceptable",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "b477c114-fa99-41db-aa52-b63c36ce662d",
                                                                "label" : "We will be able to use a dedicated network connection",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    },
                                                    {
                                                        "uuid" : "3788d53f-c79f-43d2-b67b-7479f05cddb0",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Can all data be legally transported and processed at the computing site?",
                                                        "text" : "Are the risks of data leaks covered?",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "e208ad3c-97e0-45b3-93cf-646bde4b7b45",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "bd3c4a00-eeb5-4b8e-a3e3-a8f5c77e9745",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            },
                                            {
                                                "uuid" : "d439ecac-d31f-45eb-a04f-42a326e383ca",
                                                "label" : "Yes",
                                                "advice" : null,
                                                "followUps" : []
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    },
                                    {
                                        "uuid" : "410f3709-c283-4974-83c8-6c5d24899a68",
                                        "shortUuid" : null,
                                        "type" : "QuestionTypeOptions",
                                        "title" : "Will different groups work on different parts of the workflow? Will parts of the computing be done on 'local' infrastructure to the group?",
                                        "text" : "",
                                        "answers" : [
                                            {
                                                "uuid" : "be99bdcc-76f2-43c3-b277-064062360dc1",
                                                "label" : "All steps of the workflow will be performed at central computing locations",
                                                "advice" : null,
                                                "followUps" : []
                                            },
                                            {
                                                "uuid" : "a2c4514b-61e4-47cf-ae54-7006fea37ec2",
                                                "label" : "Some steps may be performed at local computing locations",
                                                "advice" : null,
                                                "followUps" : [
                                                    {
                                                        "uuid" : "9e34d326-5f7b-494b-bc2e-2131eef6f844",
                                                        "shortUuid" : null,
                                                        "type" : "QuestionTypeOptions",
                                                        "title" : "Is there sufficient network capacity to the other computing locations?",
                                                        "text" : "",
                                                        "answers" : [
                                                            {
                                                                "uuid" : "310adf1a-1b9c-42df-94ee-dd82699a2672",
                                                                "label" : "No",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            },
                                                            {
                                                                "uuid" : "d2ceec96-5471-4073-8a25-e5201ad41c68",
                                                                "label" : "Yes",
                                                                "advice" : null,
                                                                "followUps" : []
                                                            }
                                                        ],
                                                        "answerItemTemplate" : null,
                                                        "references" : [],
                                                        "experts" : []
                                                    }
                                                ]
                                            }
                                        ],
                                        "answerItemTemplate" : null,
                                        "references" : [],
                                        "experts" : []
                                    }
                                ]
                            }
                        ],
                        "answerItemTemplate" : null,
                        "references" : [],
                        "experts" : []
                    }
                ]
            }
        ]
    },
    "replies" : [],
    "createdAt" : ISODate("2018-06-27T16:07:48.629Z"),
    "updatedAt" : ISODate("2018-06-27T16:07:48.629Z")
})

endChange("Insert bookReferences")

// ------------------------------------------------------------------------
// FINALIZATION
// ------------------------------------------------------------------------
logEndMigrationProcess()
