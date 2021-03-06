module LocalCooking.Dependencies.Content where

import LocalCooking.Semantics.User (UserExists, HasRole)
import LocalCooking.Semantics.Content
  ( SetEditor, GetRecordSubmissionPolicy
  , EditorExists, SubmissionExists, SubmissionPolicy)
import LocalCooking.Semantics.Content.Approval (GetEditor, GetRecordSubmission)
import LocalCooking.Semantics.ContentRecord.Variant (ContentRecordVariant)
import LocalCooking.Database.Schema (StoredRecordSubmissionId)
import Auth.AccessToken.Session (SessionToken)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue (SparrowStaticClientQueues, sparrowStaticClientQueues, newSparrowStaticClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Argonaut.JSONTuple (JSONTuple)
import Data.Functor.Singleton (class SingletonFunctor)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , console :: CONSOLE
  | eff)


type ContentQueues eff =
  { getEditorQueues :: GetEditorSparrowClientQueues eff
  , setEditorQueues :: SetEditorSparrowClientQueues eff
  , getSubmissionPolicyQueues :: GetSubmissionPolicySparrowClientQueues eff
  , approveSubmissionQueues :: ApproveSubmissionSparrowClientQueues eff
  , getSubmissionsQueues :: GetSubmissionsSparrowClientQueues eff
  }


newContentQueues :: forall eff. Eff (Effects eff) (ContentQueues (Effects eff))
newContentQueues = do
  getEditorQueues <- newSparrowStaticClientQueues
  setEditorQueues <- newSparrowStaticClientQueues
  getSubmissionPolicyQueues <- newSparrowStaticClientQueues
  approveSubmissionQueues <- newSparrowStaticClientQueues
  getSubmissionsQueues <- newSparrowStaticClientQueues
  pure
    { getEditorQueues
    , setEditorQueues
    , getSubmissionPolicyQueues
    , approveSubmissionQueues
    , getSubmissionsQueues
    }


contentDependencies :: forall eff stM m
                      . MonadBaseControl (Eff (Effects eff)) m stM
                     => MonadEff (Effects eff) m
                     => SingletonFunctor stM
                     => ContentQueues (Effects eff)
                     -> SparrowClientT (Effects eff) m Unit
contentDependencies
  { getEditorQueues
  , setEditorQueues
  , getSubmissionPolicyQueues
  , approveSubmissionQueues
  , getSubmissionsQueues
  } = do
  unpackClient (Topic ["content","getEditor"]) (sparrowStaticClientQueues getEditorQueues)
  unpackClient (Topic ["content","setEditor"]) (sparrowStaticClientQueues setEditorQueues)
  unpackClient (Topic ["content","getSubmissionPolicy"]) (sparrowStaticClientQueues getSubmissionPolicyQueues)
  unpackClient (Topic ["content","approveSubmission"]) (sparrowStaticClientQueues approveSubmissionQueues)
  unpackClient (Topic ["content","getSubmissions"]) (sparrowStaticClientQueues getSubmissionsQueues)



type GetEditorSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken JSONUnit) (UserExists (HasRole (EditorExists GetEditor)))

type SetEditorSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken SetEditor) (UserExists (HasRole JSONUnit))

type GetSubmissionPolicySparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken ContentRecordVariant) (UserExists (HasRole (SubmissionPolicy GetRecordSubmissionPolicy)))

type ApproveSubmissionSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken StoredRecordSubmissionId) (UserExists (HasRole (EditorExists (SubmissionExists (SubmissionPolicy JSONUnit)))))

type GetSubmissionsSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken ContentRecordVariant) (UserExists (HasRole (Array (JSONTuple StoredRecordSubmissionId GetRecordSubmission))))
