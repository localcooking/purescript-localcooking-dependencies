module LocalCooking.Dependencies.Restaurant where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
-- import LocalCooking.Common.Tag.Restaurant (RestaurantTag)
import LocalCooking.Semantics.Restaurant ()
import LocalCooking.Database.Schema (StoredMealId, StoredMenuId)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue (SparrowStaticClientQueues, sparrowStaticClientQueues, newSparrowStaticClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson)
import Data.Generic (class Generic)
import Data.Functor.Singleton (class SingletonFunctor)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , console :: CONSOLE
  | eff)


type RestaurantQueues (eff :: # Effect) =
  {}


newRestaurantQueues :: forall eff. Eff (Effects eff) (RestaurantQueues (Effects eff))
newRestaurantQueues = do
  -- addMealTagQueues <- newSparrowStaticClientQueues
  -- addRestaurantTagQueues <- newSparrowStaticClientQueues
  -- getRestaurantQueues <- newSparrowStaticClientQueues
  -- setRestaurantQueues <- newSparrowStaticClientQueues
  -- getMenusQueues <- newSparrowStaticClientQueues
  -- setMenuQueues <- newSparrowStaticClientQueues
  -- newMenuQueues <- newSparrowStaticClientQueues
  -- getMealsQueues <- newSparrowStaticClientQueues
  -- setMealQueues <- newSparrowStaticClientQueues
  -- newMealQueues <- newSparrowStaticClientQueues
  pure {}
    -- { addMealTagQueues
    -- , addRestaurantTagQueues
    -- , getRestaurantQueues
    -- , setRestaurantQueues
    -- , getMenusQueues
    -- , setMenuQueues
    -- , newMenuQueues
    -- , getMealsQueues
    -- , setMealQueues
    -- , newMealQueues
    -- }


restaurantDependencies :: forall eff stM m
                      . MonadBaseControl (Eff (Effects eff)) m stM
                     => MonadEff (Effects eff) m
                     => SingletonFunctor stM
                     => RestaurantQueues (Effects eff)
                     -> SparrowClientT (Effects eff) m Unit
restaurantDependencies
  {} = do
  pure unit
  -- unpackClient (Topic ["restaurant","addMealTag"]) (sparrowStaticClientQueues addMealTagQueues)
  -- unpackClient (Topic ["restaurant","addRestaurantTag"]) (sparrowStaticClientQueues addRestaurantTagQueues)
  -- unpackClient (Topic ["restaurant","getRestaurant"]) (sparrowStaticClientQueues getRestaurantQueues)
  -- unpackClient (Topic ["restaurant","setRestaurant"]) (sparrowStaticClientQueues setRestaurantQueues)
  -- unpackClient (Topic ["restaurant","getMenus"]) (sparrowStaticClientQueues getMenusQueues)
  -- unpackClient (Topic ["restaurant","setMenu"]) (sparrowStaticClientQueues setMenuQueues)
  -- unpackClient (Topic ["restaurant","newMenu"]) (sparrowStaticClientQueues newMenuQueues)
  -- unpackClient (Topic ["restaurant","getMeals"]) (sparrowStaticClientQueues getMealsQueues)
  -- unpackClient (Topic ["restaurant","setMeal"]) (sparrowStaticClientQueues setMealQueues)
  -- unpackClient (Topic ["restaurant","newMeal"]) (sparrowStaticClientQueues newMealQueues)
