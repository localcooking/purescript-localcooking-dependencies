module LocalCooking.Dependencies.Common where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Semantics.Common (Register, RegisterError, User)
import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue (SparrowStaticClientQueues, sparrowStaticClientQueues, newSparrowStaticClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Functor.Singleton (class SingletonFunctor)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  | eff)


type CommonQueues eff =
  { registerQueues :: RegisterSparrowClientQueues eff
  , getUserQueues :: GetUserSparrowClientQueues eff
  , setUserQueues :: SetUserSparrowClientQueues eff
  }


newCommonQueues :: forall eff. Eff (Effects eff) (CommonQueues (Effects eff))
newCommonQueues = do
  registerQueues <- newSparrowStaticClientQueues
  getUserQueues <- newSparrowStaticClientQueues
  setUserQueues <- newSparrowStaticClientQueues
  pure
    { registerQueues
    , getUserQueues
    , setUserQueues
    }


commonDependencies :: forall eff stM m
                      . MonadBaseControl (Eff (Effects eff)) m stM
                     => MonadEff (Effects eff) m
                     => SingletonFunctor stM
                     => CommonQueues (Effects eff)
                     -> SparrowClientT (Effects eff) m Unit
commonDependencies
  { registerQueues
  , getUserQueues
  , setUserQueues
  } = do
  unpackClient (Topic ["common","register"]) (sparrowStaticClientQueues registerQueues)
  unpackClient (Topic ["common","getUser"]) (sparrowStaticClientQueues getUserQueues)
  unpackClient (Topic ["common","setUser"]) (sparrowStaticClientQueues setUserQueues)


type RegisterSparrowClientQueues eff =
  SparrowStaticClientQueues eff Register RegisterError

type GetUserSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken JSONUnit) User

type SetUserSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken User) JSONUnit
