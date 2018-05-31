module LocalCooking.Dependencies.Admin where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Semantics.Common (User, Register)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue (SparrowStaticClientQueues, sparrowStaticClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Date (Date)
import Data.Date.JSON (JSONDate (..))
import Data.String.Permalink (Permalink)
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson, fail)
import Data.Generic (class Generic)
import Data.Functor.Singleton (class SingletonFunctor)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Text.Email.Validate (EmailAddress)



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  | eff)


adminDependencies :: forall eff stM m
                   . MonadBaseControl (Eff (Effects eff)) m stM
                  => MonadEff (Effects eff) m
                  => SingletonFunctor stM
                  => { getUsersQueues :: GetUsersSparrowClientQueues (Effects eff)
                     , setUserQueues  :: SetUserSparrowClientQueues (Effects eff)
                     , newUserQueues  :: NewUserSparrowClientQueues (Effects eff)
                     }
                  -> SparrowClientT (Effects eff) m Unit
adminDependencies {getUsersQueues,setUserQueues,newUserQueues} = do
  unpackClient (Topic ["admin","getUsers"]) (sparrowStaticClientQueues getUsersQueues)


type GetUsersSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken JSONUnit) (Array User)


type SetUserSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken User) JSONUnit


type NewUserSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken Register) JSONUnit
