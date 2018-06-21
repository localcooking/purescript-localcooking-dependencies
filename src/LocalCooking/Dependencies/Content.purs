module LocalCooking.Dependencies.Content where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
-- import LocalCooking.Common.Tag.Content (ContentTag)
import LocalCooking.Semantics.Content ()
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
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  | eff)


type ContentQueues (eff :: # Effect) =
  {}


newContentQueues :: forall eff. Eff (Effects eff) (ContentQueues (Effects eff))
newContentQueues = do
  -- addMealTagQueues <- newSparrowStaticClientQueues
  -- addContentTagQueues <- newSparrowStaticClientQueues
  -- getContentQueues <- newSparrowStaticClientQueues
  -- setContentQueues <- newSparrowStaticClientQueues
  -- getMenusQueues <- newSparrowStaticClientQueues
  -- setMenuQueues <- newSparrowStaticClientQueues
  -- newMenuQueues <- newSparrowStaticClientQueues
  -- getMealsQueues <- newSparrowStaticClientQueues
  -- setMealQueues <- newSparrowStaticClientQueues
  -- newMealQueues <- newSparrowStaticClientQueues
  pure {}
    -- { addMealTagQueues
    -- , addContentTagQueues
    -- , getContentQueues
    -- , setContentQueues
    -- , getMenusQueues
    -- , setMenuQueues
    -- , newMenuQueues
    -- , getMealsQueues
    -- , setMealQueues
    -- , newMealQueues
    -- }


contentDependencies :: forall eff stM m
                      . MonadBaseControl (Eff (Effects eff)) m stM
                     => MonadEff (Effects eff) m
                     => SingletonFunctor stM
                     => ContentQueues (Effects eff)
                     -> SparrowClientT (Effects eff) m Unit
contentDependencies
  {} = do
  pure unit
  -- unpackClient (Topic ["content","addMealTag"]) (sparrowStaticClientQueues addMealTagQueues)
  -- unpackClient (Topic ["content","addContentTag"]) (sparrowStaticClientQueues addContentTagQueues)
  -- unpackClient (Topic ["content","getContent"]) (sparrowStaticClientQueues getContentQueues)
  -- unpackClient (Topic ["content","setContent"]) (sparrowStaticClientQueues setContentQueues)
  -- unpackClient (Topic ["content","getMenus"]) (sparrowStaticClientQueues getMenusQueues)
  -- unpackClient (Topic ["content","setMenu"]) (sparrowStaticClientQueues setMenuQueues)
  -- unpackClient (Topic ["content","newMenu"]) (sparrowStaticClientQueues newMenuQueues)
  -- unpackClient (Topic ["content","getMeals"]) (sparrowStaticClientQueues getMealsQueues)
  -- unpackClient (Topic ["content","setMeal"]) (sparrowStaticClientQueues setMealQueues)
  -- unpackClient (Topic ["content","newMeal"]) (sparrowStaticClientQueues newMealQueues)
