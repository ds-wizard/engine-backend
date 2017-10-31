module Fixtures.KnowledgeModel.Experts where

import Model.KnowledgeModel.KnowledgeModel

expertDarth =
  Expert
  { _expUuid = "expDarth"
  , _expName = "Darth Vader"
  , _expEmail = "darth.vader@deadstar.com"
  }

expertDarthChanged =
  Expert
  { _expUuid = "expDarth"
  , _expName = "EDITED: Darth Vader"
  , _expEmail = "EDITED: darth.vader@deadstar.com"
  }

expertLuke =
  Expert
  { _expUuid = "expLuke"
  , _expName = "Luke Skywalker"
  , _expEmail = "luke.skywalker@tatooine.com"
  }

expertJohn =
  Expert
  { _expUuid = "expJohn"
  , _expName = "John Snow"
  , _expEmail = "john.snow@got.com"
  }
