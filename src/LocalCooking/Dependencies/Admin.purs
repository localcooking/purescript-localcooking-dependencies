module LocalCooking.Dependencies.Admin where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Semantics.Common (User, Register)

import Sparrow.Client.Queue (SparrowStaticClientQueues)

import Data.Date (Date)
import Data.Date.JSON (JSONDate (..))
import Data.String.Permalink (Permalink)
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson, fail)
import Data.Generic (class Generic)
import Text.Email.Validate (EmailAddress)


type GetUsersSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken JSONUnit) (Array User)


type SetUsersSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken User) JSONUnit


type NewUsersSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken Register) JSONUnit
