module LocalCooking.Dependencies.Admin where

import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Database.Schema (StoredEditorId)
import LocalCooking.Semantics.ContentRecord.Variant (ContentRecordVariant)
import LocalCooking.Semantics.Common (User)
import LocalCooking.Semantics.User (UserExists, HasRole, UserUnique)
import LocalCooking.Semantics.Admin
  (SetUser, NewUser, GetSetSubmissionPolicy, SubmissionPolicyUnique)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue (SparrowStaticClientQueues, sparrowStaticClientQueues, newSparrowStaticClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Argonaut.JSONTuple (JSONTuple)
import Data.Functor.Singleton (class SingletonFunctor)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , console :: CONSOLE
  | eff)

type AdminQueues eff =
  { getUsersQueues :: GetUsersSparrowClientQueues eff
  , setUserQueues :: SetUserSparrowClientQueues eff
  , newUserQueues :: NewUserSparrowClientQueues eff
  , getSubmissionPolicyQueues :: GetSubmissionPolicySparrowClientQueues eff
  , setSubmissionPolicyQueues :: SetSubmissionPolicySparrowClientQueues eff
  , assignSubmissionPolicyQueues :: AssignSubmissionPolicySparrowClientQueues eff
  }


newAdminQueues :: forall eff. Eff (Effects eff) (AdminQueues (Effects eff))
newAdminQueues = do
  getUsersQueues <- newSparrowStaticClientQueues
  setUserQueues <- newSparrowStaticClientQueues
  newUserQueues <- newSparrowStaticClientQueues
  getSubmissionPolicyQueues <- newSparrowStaticClientQueues
  setSubmissionPolicyQueues <- newSparrowStaticClientQueues
  assignSubmissionPolicyQueues <- newSparrowStaticClientQueues
  pure
    { getUsersQueues
    , setUserQueues
    , newUserQueues
    , getSubmissionPolicyQueues
    , setSubmissionPolicyQueues
    , assignSubmissionPolicyQueues
    }


adminDependencies :: forall eff stM m
                   . MonadBaseControl (Eff (Effects eff)) m stM
                  => MonadEff (Effects eff) m
                  => SingletonFunctor stM
                  => AdminQueues (Effects eff)
                  -> SparrowClientT (Effects eff) m Unit
adminDependencies
  { getUsersQueues
  , setUserQueues
  , newUserQueues
  , getSubmissionPolicyQueues
  , setSubmissionPolicyQueues
  , assignSubmissionPolicyQueues
  } = do
  unpackClient (Topic ["admin","getUsers"]) (sparrowStaticClientQueues getUsersQueues)
  unpackClient (Topic ["admin","setUser"]) (sparrowStaticClientQueues setUserQueues)
  unpackClient (Topic ["admin","newUser"]) (sparrowStaticClientQueues newUserQueues)
  unpackClient (Topic ["admin","getSubmissionPolicy"]) (sparrowStaticClientQueues getSubmissionPolicyQueues)
  unpackClient (Topic ["admin","setSubmissionPolicy"]) (sparrowStaticClientQueues setSubmissionPolicyQueues)
  unpackClient (Topic ["admin","assignSubmissionPolicy"]) (sparrowStaticClientQueues assignSubmissionPolicyQueues)


type GetUsersSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple AuthToken JSONUnit) (UserExists (HasRole (Array User)))

type SetUserSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple AuthToken SetUser) (UserExists (HasRole JSONUnit))

type NewUserSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple AuthToken NewUser) (UserExists (HasRole (UserUnique JSONUnit)))

type GetSubmissionPolicySparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple AuthToken ContentRecordVariant) (UserExists (HasRole (SubmissionPolicyUnique GetSetSubmissionPolicy)))

type SetSubmissionPolicySparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple AuthToken GetSetSubmissionPolicy) (UserExists (HasRole JSONUnit))

type AssignSubmissionPolicySparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple AuthToken (JSONTuple StoredEditorId ContentRecordVariant)) (UserExists (HasRole (SubmissionPolicyUnique JSONUnit)))
