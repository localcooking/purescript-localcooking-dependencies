module LocalCooking.Dependencies.Common where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Semantics.Common (Register, User)
import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Sparrow.Client.Queue (SparrowStaticClientQueues)

import Data.Argonaut.JSONUnit (JSONUnit)


type RegisterSparrowClientQueues eff =
  SparrowStaticClientQueues eff Register JSONUnit

type GetUserSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken JSONUnit) User

type SetUserSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken User) JSONUnit
