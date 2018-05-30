module LocalCooking.Dependencies.Common where

import LocalCooking.Semantics.Common (Register)
import Data.Argonaut.JSONUnit (JSONUnit)

import Sparrow.Client.Queue (SparrowStaticClientQueues)


type RegisterSparrowClientQueues eff = SparrowStaticClientQueues eff
  Register JSONUnit
