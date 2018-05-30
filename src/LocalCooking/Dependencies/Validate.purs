module LocalCooking.Dependencies.Validate where


import Sparrow.Client.Queue (SparrowStaticClientQueues)

import Data.Date (Date)
import Data.Date.JSON (JSONDate (..))
import Data.String.Permalink (Permalink)
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson, fail)
import Text.Email.Validate (EmailAddress)


type UniqueEmailSparrowClientQueues eff =
  SparrowStaticClientQueues eff EmailAddress JSONUnit

type UniqueChefPermalinkSparrowClientQueues eff =
  SparrowStaticClientQueues eff Permalink JSONUnit

newtype IsUniqueMenuDeadline = IsUniqueMenuDeadline
  { chef :: Permalink
  , deadline :: Date
  }

instance encodeJsonIsUniqueMenuDeadline :: EncodeJson IsUniqueMenuDeadline where
  encodeJson (IsUniqueMenuDeadline {chef,deadline})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> jsonEmptyObject

type UniqueMenuDeadlineSparrowClientQueues eff =
  SparrowStaticClientQueues eff IsUniqueMenuDeadline JSONUnit

newtype IsUniqueMealPermalink = IsUniqueMealPermalink
  { chef :: Permalink
  , deadline :: Date
  , meal :: Permalink
  }

-- TODO FIXME prove isomorphic caps

instance encodeJsonIsUniqueMealPermalink :: EncodeJson IsUniqueMealPermalink where
  encodeJson (IsUniqueMealPermalink {chef,deadline,meal})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> "meal" := meal
    ~> jsonEmptyObject

type UniqueMealDeadlineSparrowClientQueues eff =
  SparrowStaticClientQueues eff IsUniqueMealPermalink JSONUnit
