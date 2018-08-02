module LocalCooking.Dependencies.Farm where

-- import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
-- import LocalCooking.Common.Tag.Farm (FarmTag)
import LocalCooking.Semantics.Farm ()
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


type FarmQueues (eff :: # Effect) =
  {}


newFarmQueues :: forall eff. Eff (Effects eff) (FarmQueues (Effects eff))
newFarmQueues = do
  -- addMealTagQueues <- newSparrowStaticClientQueues
  -- addFarmTagQueues <- newSparrowStaticClientQueues
  -- getFarmQueues <- newSparrowStaticClientQueues
  -- setFarmQueues <- newSparrowStaticClientQueues
  -- getMenusQueues <- newSparrowStaticClientQueues
  -- setMenuQueues <- newSparrowStaticClientQueues
  -- newMenuQueues <- newSparrowStaticClientQueues
  -- getMealsQueues <- newSparrowStaticClientQueues
  -- setMealQueues <- newSparrowStaticClientQueues
  -- newMealQueues <- newSparrowStaticClientQueues
  pure {}
    -- { addMealTagQueues
    -- , addFarmTagQueues
    -- , getFarmQueues
    -- , setFarmQueues
    -- , getMenusQueues
    -- , setMenuQueues
    -- , newMenuQueues
    -- , getMealsQueues
    -- , setMealQueues
    -- , newMealQueues
    -- }


farmDependencies :: forall eff stM m
                      . MonadBaseControl (Eff (Effects eff)) m stM
                     => MonadEff (Effects eff) m
                     => SingletonFunctor stM
                     => FarmQueues (Effects eff)
                     -> SparrowClientT (Effects eff) m Unit
farmDependencies
  {} = do
  pure unit
  -- unpackClient (Topic ["farm","addMealTag"]) (sparrowStaticClientQueues addMealTagQueues)
  -- unpackClient (Topic ["farm","addFarmTag"]) (sparrowStaticClientQueues addFarmTagQueues)
  -- unpackClient (Topic ["farm","getFarm"]) (sparrowStaticClientQueues getFarmQueues)
  -- unpackClient (Topic ["farm","setFarm"]) (sparrowStaticClientQueues setFarmQueues)
  -- unpackClient (Topic ["farm","getMenus"]) (sparrowStaticClientQueues getMenusQueues)
  -- unpackClient (Topic ["farm","setMenu"]) (sparrowStaticClientQueues setMenuQueues)
  -- unpackClient (Topic ["farm","newMenu"]) (sparrowStaticClientQueues newMenuQueues)
  -- unpackClient (Topic ["farm","getMeals"]) (sparrowStaticClientQueues getMealsQueues)
  -- unpackClient (Topic ["farm","setMeal"]) (sparrowStaticClientQueues setMealQueues)
  -- unpackClient (Topic ["farm","newMeal"]) (sparrowStaticClientQueues newMealQueues)

