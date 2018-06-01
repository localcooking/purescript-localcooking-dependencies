module LocalCooking.Dependencies where

import LocalCooking.Dependencies.AuthToken (authTokenDependencies, AuthTokenQueues, newAuthTokenQueues)
import LocalCooking.Dependencies.Validate (validateDependencies, ValidateQueues, newValidateQueues)
import LocalCooking.Dependencies.Common (commonDependencies, CommonQueues, newCommonQueues)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue (SparrowStaticClientQueues, sparrowStaticClientQueues, newSparrowStaticClientQueues)
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

type Queues siteQueues eff =
  { authTokenQueues :: AuthTokenQueues eff
  , validateQueues :: ValidateQueues eff
  , commonQueues :: CommonQueues eff
  , siteQueues :: siteQueues
  }


newQueues :: forall eff siteQueues
           . Eff (Effects eff) siteQueues
          -> Eff (Effects eff) (Queues siteQueues (Effects eff))
newQueues newSiteQueues = do
  authTokenQueues <- newAuthTokenQueues
  validateQueues <- newValidateQueues
  commonQueues <- newCommonQueues
  siteQueues <- newSiteQueues
  pure {authTokenQueues,validateQueues,commonQueues,siteQueues}


dependencies :: forall eff m siteQueues stM
              . MonadBaseControl (Eff (Effects eff)) m stM
             => MonadEff (Effects eff) m
             => SingletonFunctor stM
             => Queues siteQueues (Effects eff)
             -> (siteQueues -> SparrowClientT (Effects eff) m Unit)
             -> SparrowClientT (Effects eff) m Unit
dependencies {authTokenQueues,validateQueues,commonQueues,siteQueues} deps = do
  authTokenDependencies authTokenQueues
  validateDependencies validateQueues
  commonDependencies commonQueues
  deps siteQueues
