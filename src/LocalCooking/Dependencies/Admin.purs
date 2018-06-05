module LocalCooking.Dependencies.Admin where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Semantics.Common (User, Register)

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

type AdminQueues eff =
  { getUsersQueues :: GetUsersSparrowClientQueues eff
  , setUserQueues :: SetUserSparrowClientQueues eff
  , newUserQueues :: NewUserSparrowClientQueues eff
  }


newAdminQueues :: forall eff. Eff (Effects eff) (AdminQueues (Effects eff))
newAdminQueues = do
  getUsersQueues <- newSparrowStaticClientQueues
  setUserQueues <- newSparrowStaticClientQueues
  newUserQueues <- newSparrowStaticClientQueues
  pure
    { getUsersQueues
    , setUserQueues
    , newUserQueues
    }


adminDependencies :: forall eff stM m
                   . MonadBaseControl (Eff (Effects eff)) m stM
                  => MonadEff (Effects eff) m
                  => SingletonFunctor stM
                  => AdminQueues (Effects eff)
                  -> SparrowClientT (Effects eff) m Unit
adminDependencies {getUsersQueues,setUserQueues,newUserQueues} = do
  unpackClient (Topic ["admin","getUsers"]) (sparrowStaticClientQueues getUsersQueues)
  unpackClient (Topic ["admin","setUser"]) (sparrowStaticClientQueues setUserQueues)
  unpackClient (Topic ["admin","newUser"]) (sparrowStaticClientQueues newUserQueues)


type GetUsersSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken JSONUnit) (Array User)


type SetUserSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken User) JSONUnit


type NewUserSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken Register) JSONUnit
