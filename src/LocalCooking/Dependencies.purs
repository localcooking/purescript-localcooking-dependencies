module LocalCooking.Dependencies where

import LocalCooking.Dependencies.Validate (validateDependencies, ValidateQueues, newValidateQueues)
import LocalCooking.Dependencies.Common (commonDependencies, CommonQueues, newCommonQueues)
import LocalCooking.Semantics.Common (Login)
import LocalCooking.Global.Error (LoginError)
import Auth.AccessToken.Session (sessionTokenDependencies, SessionTokenQueues, newSessionTokenQueues)

import Sparrow.Client.Types (SparrowClientT)

import Prelude
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

type DependenciesQueues eff =
  { sessionTokenQueues :: SessionTokenQueues Login LoginError eff
  , validateQueues :: ValidateQueues eff
  , commonQueues :: CommonQueues eff
  }


newQueues :: forall eff
           . Eff (Effects eff) (DependenciesQueues (Effects eff))
newQueues = do
  sessionTokenQueues <- newSessionTokenQueues
  validateQueues <- newValidateQueues
  commonQueues <- newCommonQueues
  pure {sessionTokenQueues,validateQueues,commonQueues}


dependencies :: forall eff m stM
              . MonadBaseControl (Eff (Effects eff)) m stM
             => MonadEff (Effects eff) m
             => SingletonFunctor stM
             => DependenciesQueues (Effects eff)
             -> SparrowClientT (Effects eff) m Unit
             -> SparrowClientT (Effects eff) m Unit
dependencies {sessionTokenQueues,validateQueues,commonQueues} deps = do
  sessionTokenDependencies sessionTokenQueues
  validateDependencies validateQueues
  commonDependencies commonQueues
  deps
