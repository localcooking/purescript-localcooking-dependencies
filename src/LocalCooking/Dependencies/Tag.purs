module LocalCooking.Dependencies.Tag where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Meal (MealTag)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue (SparrowStaticClientQueues, sparrowStaticClientQueues, newSparrowStaticClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Functor.Singleton (class SingletonFunctor)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  | eff)


type TagQueues eff =
  { searchChefTagsQueues :: SearchChefTagsSparrowClientQueues eff
  , searchMealTagsQueues :: SearchMealTagsSparrowClientQueues eff
  , submitChefTagQueues :: SubmitChefTagSparrowClientQueues eff
  , submitMealTagQueues :: SubmitMealTagSparrowClientQueues eff
  }


newTagQueues :: forall eff. Eff (Effects eff) (TagQueues (Effects eff))
newTagQueues = do
  searchChefTagsQueues <- newSparrowStaticClientQueues
  searchMealTagsQueues <- newSparrowStaticClientQueues
  submitChefTagQueues <- newSparrowStaticClientQueues
  submitMealTagQueues <- newSparrowStaticClientQueues
  pure
    { searchChefTagsQueues
    , searchMealTagsQueues
    , submitChefTagQueues
    , submitMealTagQueues
    }

tagDependencies :: forall eff stM m
                 . MonadBaseControl (Eff (Effects eff)) m stM
                => MonadEff (Effects eff) m
                => SingletonFunctor stM
                => TagQueues (Effects eff)
                -> SparrowClientT (Effects eff) m Unit
tagDependencies
  { searchChefTagsQueues
  , searchMealTagsQueues
  , submitChefTagQueues
  , submitMealTagQueues
  } = do
  unpackClient (Topic ["tag","search","chef"]) (sparrowStaticClientQueues searchChefTagsQueues)
  unpackClient (Topic ["tag","search","meal"]) (sparrowStaticClientQueues searchMealTagsQueues)
  unpackClient (Topic ["tag","submit","chef"]) (sparrowStaticClientQueues submitChefTagQueues)
  unpackClient (Topic ["tag","submit","meal"]) (sparrowStaticClientQueues submitMealTagQueues)



type SearchChefTagsSparrowClientQueues eff =
  SparrowStaticClientQueues eff String (Array ChefTag)

type SearchMealTagsSparrowClientQueues eff =
  SparrowStaticClientQueues eff String (Array MealTag)

type SubmitChefTagSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken ChefTag) JSONUnit

type SubmitMealTagSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken MealTag) JSONUnit
