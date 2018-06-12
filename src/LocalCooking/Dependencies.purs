module LocalCooking.Dependencies where

import LocalCooking.Dependencies.AuthToken (authTokenDependencies, AuthTokenQueues, newAuthTokenQueues)
import LocalCooking.Dependencies.Validate (validateDependencies, ValidateQueues, newValidateQueues)
import LocalCooking.Dependencies.Common (commonDependencies, CommonQueues, newCommonQueues)

import Sparrow.Client.Types (SparrowClientT)

import Prelude
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

type DependenciesQueues siteQueues eff =
  { authTokenQueues :: AuthTokenQueues eff
  , validateQueues :: ValidateQueues eff
  , commonQueues :: CommonQueues eff
  , siteQueues :: siteQueues
  }


newQueues :: forall eff siteQueues
           . siteQueues
          -> Eff (Effects eff) (DependenciesQueues siteQueues (Effects eff))
newQueues siteQueues = do
  authTokenQueues <- newAuthTokenQueues
  validateQueues <- newValidateQueues
  commonQueues <- newCommonQueues
  pure {authTokenQueues,validateQueues,commonQueues,siteQueues}


dependencies :: forall eff m siteQueues stM
              . MonadBaseControl (Eff (Effects eff)) m stM
             => MonadEff (Effects eff) m
             => SingletonFunctor stM
             => DependenciesQueues siteQueues (Effects eff)
             -> (siteQueues -> SparrowClientT (Effects eff) m Unit)
             -> SparrowClientT (Effects eff) m Unit
dependencies {authTokenQueues,validateQueues,commonQueues,siteQueues} deps = do
  authTokenDependencies authTokenQueues
  validateDependencies validateQueues
  commonDependencies commonQueues
  deps siteQueues
