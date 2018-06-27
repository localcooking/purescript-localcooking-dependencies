module LocalCooking.Dependencies.Tag where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Culture (CultureTag)
import LocalCooking.Common.Tag.Diet (DietTag)
import LocalCooking.Common.Tag.Farm (FarmTag)
import LocalCooking.Common.Tag.Ingredient (IngredientTag)
import LocalCooking.Common.Tag.Meal (MealTag)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue
   ( SparrowStaticClientQueues, sparrowStaticClientQueues, newSparrowStaticClientQueues
   , SparrowClientQueues, sparrowClientQueues, newSparrowClientQueues
   )
import Sparrow.Types (Topic (..))

import Prelude
import Data.Maybe (Maybe)
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
  , searchCultureTagsQueues :: SearchCultureTagsSparrowClientQueues eff
  , searchDietTagsQueues :: SearchDietTagsSparrowClientQueues eff
  , searchFarmTagsQueues :: SearchFarmTagsSparrowClientQueues eff
  , searchIngredientTagsQueues :: SearchIngredientTagsSparrowClientQueues eff
  , searchMealTagsQueues :: SearchMealTagsSparrowClientQueues eff
  , submitChefTagQueues :: SubmitChefTagSparrowClientQueues eff
  , submitCultureTagQueues :: SubmitCultureTagSparrowClientQueues eff
  , submitDietTagQueues :: SubmitDietTagSparrowClientQueues eff
  , submitFarmTagQueues :: SubmitFarmTagSparrowClientQueues eff
  , submitIngredientTagQueues :: SubmitIngredientTagSparrowClientQueues eff
  , submitMealTagQueues :: SubmitMealTagSparrowClientQueues eff
  }


newTagQueues :: forall eff. Eff (Effects eff) (TagQueues (Effects eff))
newTagQueues = do
  searchChefTagsQueues <- newSparrowClientQueues
  searchCultureTagsQueues <- newSparrowClientQueues
  searchDietTagsQueues <- newSparrowClientQueues
  searchFarmTagsQueues <- newSparrowClientQueues
  searchIngredientTagsQueues <- newSparrowClientQueues
  searchMealTagsQueues <- newSparrowClientQueues
  submitChefTagQueues <- newSparrowStaticClientQueues
  submitCultureTagQueues <- newSparrowStaticClientQueues
  submitDietTagQueues <- newSparrowStaticClientQueues
  submitFarmTagQueues <- newSparrowStaticClientQueues
  submitIngredientTagQueues <- newSparrowStaticClientQueues
  submitMealTagQueues <- newSparrowStaticClientQueues
  pure
    { searchChefTagsQueues
    , searchCultureTagsQueues
    , searchDietTagsQueues
    , searchFarmTagsQueues
    , searchIngredientTagsQueues
    , searchMealTagsQueues
    , submitChefTagQueues
    , submitCultureTagQueues
    , submitDietTagQueues
    , submitFarmTagQueues
    , submitIngredientTagQueues
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
  , searchCultureTagsQueues
  , searchDietTagsQueues
  , searchFarmTagsQueues
  , searchIngredientTagsQueues
  , searchMealTagsQueues
  , submitChefTagQueues
  , submitCultureTagQueues
  , submitDietTagQueues
  , submitFarmTagQueues
  , submitIngredientTagQueues
  , submitMealTagQueues
  } = do
  unpackClient (Topic ["tag","search","chef"]) (sparrowClientQueues searchChefTagsQueues)
  unpackClient (Topic ["tag","search","culture"]) (sparrowClientQueues searchCultureTagsQueues)
  unpackClient (Topic ["tag","search","diet"]) (sparrowClientQueues searchDietTagsQueues)
  unpackClient (Topic ["tag","search","farm"]) (sparrowClientQueues searchFarmTagsQueues)
  unpackClient (Topic ["tag","search","ingredient"]) (sparrowClientQueues searchIngredientTagsQueues)
  unpackClient (Topic ["tag","search","meal"]) (sparrowClientQueues searchMealTagsQueues)
  unpackClient (Topic ["tag","submit","chef"]) (sparrowStaticClientQueues submitChefTagQueues)
  unpackClient (Topic ["tag","submit","culture"]) (sparrowStaticClientQueues submitCultureTagQueues)
  unpackClient (Topic ["tag","submit","diet"]) (sparrowStaticClientQueues submitDietTagQueues)
  unpackClient (Topic ["tag","submit","farm"]) (sparrowStaticClientQueues submitFarmTagQueues)
  unpackClient (Topic ["tag","submit","ingredient"]) (sparrowStaticClientQueues submitIngredientTagQueues)
  unpackClient (Topic ["tag","submit","meal"]) (sparrowStaticClientQueues submitMealTagQueues)



type SearchChefTagsSparrowClientQueues eff =
  SparrowClientQueues eff JSONUnit JSONUnit String (Maybe (Array ChefTag))

type SearchCultureTagsSparrowClientQueues eff =
  SparrowClientQueues eff JSONUnit JSONUnit String (Maybe (Array CultureTag))

type SearchDietTagsSparrowClientQueues eff =
  SparrowClientQueues eff JSONUnit JSONUnit String (Maybe (Array DietTag))

type SearchFarmTagsSparrowClientQueues eff =
  SparrowClientQueues eff JSONUnit JSONUnit String (Maybe (Array FarmTag))

type SearchIngredientTagsSparrowClientQueues eff =
  SparrowClientQueues eff JSONUnit JSONUnit String (Maybe (Array IngredientTag))

type SearchMealTagsSparrowClientQueues eff =
  SparrowClientQueues eff JSONUnit JSONUnit String (Maybe (Array MealTag))

type SubmitChefTagSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken ChefTag) JSONUnit

type SubmitCultureTagSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken CultureTag) JSONUnit

type SubmitDietTagSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken DietTag) JSONUnit

type SubmitFarmTagSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken FarmTag) JSONUnit

type SubmitIngredientTagSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken IngredientTag) JSONUnit

type SubmitMealTagSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken MealTag) JSONUnit
