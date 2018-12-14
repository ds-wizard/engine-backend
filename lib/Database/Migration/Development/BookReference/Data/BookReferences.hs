module Database.Migration.Development.BookReference.Data.BookReferences where

import Data.Maybe (fromJust)
import Data.Time

import Model.BookReference.BookReference

bookReferenceBvq =
  BookReference
  { _bookReferenceShortUuid = "bvq"
  , _bookReferenceBookChapter = "1.12"
  , _bookReferenceContent =
      "# Submit to an existing database?\n\n## What's up?\n\nIn some cases it might be wise to add your data to existing collections, such as international archives. More and more funding organisations and publishers will actually request that. This is in itself a reasonably straightforward process, as such public archives are usually maintained by dedicated institutes or consortia and these have rules, regulations, requested formats and in many cases clear instructions in how to upload and retrieve your data. Please note that also here, some environments with be 'inert' non-interoperable archive (or only meant to serve human re-use) and some may qualify as HPR environments, which may pose quite some different challenges for you as a data creator (see also earlier consideration on updates and versioning of growing or changing data sets or resources).\n\nA more detailed consideration may be whether you consider your data 'reference' data that may be offered for curation and including the addition to core data resources such as UniProt. In that case the procedures may be quite different and an active interaction with the core data resource custodians is in many cases the way to go. Currently, many of these resources have to painstakingly recover the data they want to use in their curated and value-added resource by *ocular extraction* (reading) or by text and data mining, both of which is cumbersome and error prone. The *direct addition* of your data in the proper, unambiguous format to these core resources is part of good data stewardship practice.\n\n## Do\n\n- Always consider the 'potential reference value' of your data (for instance, can my new findings enrich reference data sources such as Chembl, UniProt or Earthcube).\n- Submit (parts of) your data to the appropriate 'archives' but also provide selected parts of your data to reference core databases whenever appropriate\n- Use the correct formats and standards required by those data resources and if needed contact them.\n- (see resources, databases etc, by category in website?)\n\n## Don't\n\n- Just publish your data as 'supplementary files' with your article(s) and assume that custodians of archives, HPR environments and core reference data  bases will find and use them independently\n- Upload data to such resources without proper metadata and provenance attached.\n"
  , _bookReferenceCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _bookReferenceUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }
