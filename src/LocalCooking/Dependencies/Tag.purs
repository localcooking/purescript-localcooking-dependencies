module LocalCooking.Dependencies.Tag where

import LocalCooking.Semantics.User (UserExists)
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
   , mountSparrowClientQueuesSingleton)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Data.Argonaut.JSONTuple (JSONTuple)
import Data.Functor.Singleton (class SingletonFunctor)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Queue.Types (writeOnly)
import Queue.One as One



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , console :: CONSOLE
  | eff)


type TagQueues eff =
  { searchChefTagsQueues       :: SearchTagsSparrowClientQueues eff ChefTag
  , searchCultureTagsQueues    :: SearchTagsSparrowClientQueues eff CultureTag
  , searchDietTagsQueues       :: SearchTagsSparrowClientQueues eff DietTag 
  , searchFarmTagsQueues       :: SearchTagsSparrowClientQueues eff FarmTag
  , searchIngredientTagsQueues :: SearchTagsSparrowClientQueues eff IngredientTag
  , searchMealTagsQueues       :: SearchTagsSparrowClientQueues eff MealTag
  , submitChefTagQueues        :: SubmitTagSparrowClientQueues eff ChefTag
  , submitCultureTagQueues     :: SubmitTagSparrowClientQueues eff CultureTag
  , submitDietTagQueues        :: SubmitTagSparrowClientQueues eff DietTag
  , submitFarmTagQueues        :: SubmitTagSparrowClientQueues eff FarmTag
  , submitIngredientTagQueues  :: SubmitTagSparrowClientQueues eff IngredientTag
  , submitMealTagQueues        :: SubmitTagSparrowClientQueues eff MealTag
  }


newTagQueues :: forall eff. Eff (Effects eff) (TagQueues (Effects eff))
newTagQueues = do
  searchChefTagsQueues       <- newSparrowClientQueues
  searchCultureTagsQueues    <- newSparrowClientQueues
  searchDietTagsQueues       <- newSparrowClientQueues
  searchFarmTagsQueues       <- newSparrowClientQueues
  searchIngredientTagsQueues <- newSparrowClientQueues
  searchMealTagsQueues       <- newSparrowClientQueues
  submitChefTagQueues        <- newSparrowStaticClientQueues
  submitCultureTagQueues     <- newSparrowStaticClientQueues
  submitDietTagQueues        <- newSparrowStaticClientQueues
  submitFarmTagQueues        <- newSparrowStaticClientQueues
  submitIngredientTagQueues  <- newSparrowStaticClientQueues
  submitMealTagQueues        <- newSparrowStaticClientQueues
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



type TagSearch eff =
  { searchChefTags       :: String -> Eff eff Unit
  , searchCultureTags    :: String -> Eff eff Unit
  , searchDietTags       :: String -> Eff eff Unit
  , searchFarmTags       :: String -> Eff eff Unit
  , searchIngredientTags :: String -> Eff eff Unit
  , searchMealTags       :: String -> Eff eff Unit
  }


type TagSearchResults eff =
  { onChefTagSearchResult       :: Array ChefTag       -> Eff eff Unit
  , onCultureTagSearchResult    :: Array CultureTag    -> Eff eff Unit
  , onDietTagSearchResult       :: Array DietTag       -> Eff eff Unit
  , onFarmTagSearchResult       :: Array FarmTag       -> Eff eff Unit
  , onIngredientTagSearchResult :: Array IngredientTag -> Eff eff Unit
  , onMealTagSearchResult       :: Array MealTag       -> Eff eff Unit
  }


mountTagSearchQueues :: forall eff
                      . TagQueues (Effects eff)
                     -> TagSearchResults (Effects eff)
                     -> Eff (Effects eff) (TagSearch (Effects eff))
mountTagSearchQueues
  tagQueues
  { onChefTagSearchResult
  , onCultureTagSearchResult
  , onDietTagSearchResult
  , onFarmTagSearchResult
  , onIngredientTagSearchResult
  , onMealTagSearchResult
  } = do

  searchChefTags       <- mountOne onChefTagSearchResult       tagQueues.searchChefTagsQueues
  searchCultureTags    <- mountOne onCultureTagSearchResult    tagQueues.searchCultureTagsQueues
  searchDietTags       <- mountOne onDietTagSearchResult       tagQueues.searchDietTagsQueues
  searchFarmTags       <- mountOne onFarmTagSearchResult       tagQueues.searchFarmTagsQueues
  searchIngredientTags <- mountOne onIngredientTagSearchResult tagQueues.searchIngredientTagsQueues
  searchMealTags       <- mountOne onMealTagSearchResult       tagQueues.searchMealTagsQueues


  pure
    { searchChefTags
    , searchCultureTags
    , searchDietTags
    , searchFarmTags
    , searchIngredientTags
    , searchMealTags
    }
  where
    mountOne :: forall tag
              . (Array tag -> Eff (Effects eff) Unit)
             -> SearchTagsSparrowClientQueues (Effects eff) tag
             -> Eff (Effects eff) (String -> Eff (Effects eff) Unit)
    mountOne onResult queues = do
      deltaInQueue <- writeOnly <$> One.newQueue
      initInQueue <- writeOnly <$> One.newQueue

      let onDeltaOut deltaOut = case deltaOut of
            Nothing -> pure unit
            Just chefTags -> onResult chefTags
          onInitOut mInitOut = case mInitOut of
              -- FIXME apply to error queue
            Nothing -> pure unit
            Just JSONUnit -> pure unit

      _ <- mountSparrowClientQueuesSingleton queues
        deltaInQueue initInQueue onDeltaOut onInitOut

      One.putQueue initInQueue JSONUnit

      pure (One.putQueue deltaInQueue)



-- FIXME encode input delta with "continue" for more results, and align with Sphinx
type SearchTagsSparrowClientQueues eff tag =
  SparrowClientQueues eff JSONUnit JSONUnit String (Maybe (Array tag))


type SubmitTagSparrowClientQueues eff tag =
  SparrowStaticClientQueues eff (JSONTuple AuthToken tag) (UserExists JSONUnit)
