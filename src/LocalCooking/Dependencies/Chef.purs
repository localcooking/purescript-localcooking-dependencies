module LocalCooking.Dependencies.Chef where

import LocalCooking.Semantics.User (UserExists, HasRole)
import LocalCooking.Semantics.Mitch (MenuExists, MealExists)
import LocalCooking.Semantics.Chef
  ( SetChef, ChefValid, MenuSettings, MealSettings
  , ChefUnique, ChefExists)
import LocalCooking.Database.Schema (StoredChefId, StoredMealId, StoredMenuId)
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


type ChefQueues eff =
  { getChefQueues :: GetChefSparrowClientQueues eff
  , setChefQueues :: SetChefSparrowClientQueues eff
  , getMenusQueues :: GetMenusSparrowClientQueues eff
  , setMenuQueues :: SetMenuSparrowClientQueues eff
  , newMenuQueues :: NewMenuSparrowClientQueues eff
  , getMealsQueues :: GetMealsSparrowClientQueues eff
  , setMealQueues :: SetMealSparrowClientQueues eff
  , newMealQueues :: NewMealSparrowClientQueues eff
  }


newChefQueues :: forall eff. Eff (Effects eff) (ChefQueues (Effects eff))
newChefQueues = do
  getChefQueues <- newSparrowStaticClientQueues
  setChefQueues <- newSparrowStaticClientQueues
  getMenusQueues <- newSparrowStaticClientQueues
  setMenuQueues <- newSparrowStaticClientQueues
  newMenuQueues <- newSparrowStaticClientQueues
  getMealsQueues <- newSparrowStaticClientQueues
  setMealQueues <- newSparrowStaticClientQueues
  newMealQueues <- newSparrowStaticClientQueues
  pure
    { getChefQueues
    , setChefQueues
    , getMenusQueues
    , setMenuQueues
    , newMenuQueues
    , getMealsQueues
    , setMealQueues
    , newMealQueues
    }


chefDependencies :: forall eff stM m
                      . MonadBaseControl (Eff (Effects eff)) m stM
                     => MonadEff (Effects eff) m
                     => SingletonFunctor stM
                     => ChefQueues (Effects eff)
                     -> SparrowClientT (Effects eff) m Unit
chefDependencies
  { getChefQueues
  , setChefQueues
  , getMenusQueues
  , setMenuQueues
  , newMenuQueues
  , getMealsQueues
  , setMealQueues
  , newMealQueues
  } = do
  unpackClient (Topic ["chef","getChef"]) (sparrowStaticClientQueues getChefQueues)
  unpackClient (Topic ["chef","setChef"]) (sparrowStaticClientQueues setChefQueues)
  unpackClient (Topic ["chef","getMenus"]) (sparrowStaticClientQueues getMenusQueues)
  unpackClient (Topic ["chef","setMenu"]) (sparrowStaticClientQueues setMenuQueues)
  unpackClient (Topic ["chef","newMenu"]) (sparrowStaticClientQueues newMenuQueues)
  unpackClient (Topic ["chef","getMeals"]) (sparrowStaticClientQueues getMealsQueues)
  unpackClient (Topic ["chef","setMeal"]) (sparrowStaticClientQueues setMealQueues)
  unpackClient (Topic ["chef","newMeal"]) (sparrowStaticClientQueues newMealQueues)



type GetChefSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken JSONUnit) (UserExists (HasRole (ChefUnique (ChefExists ChefValid))))

type SetChefSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken SetChef) (UserExists (HasRole StoredChefId))

type GetMenusSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken JSONUnit) (UserExists (HasRole (ChefUnique (Array (MenuExists (JSONTuple StoredMenuId MenuSettings))))))

type NewMenuSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken MenuSettings) (UserExists (HasRole JSONUnit))

type SetMenuSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken (JSONTuple StoredMenuId MenuSettings)) (UserExists (HasRole JSONUnit))

type GetMealsSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken StoredMenuId) (UserExists (HasRole (Array (MealExists (JSONTuple StoredMealId MealSettings)))))

type NewMealSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken (JSONTuple StoredMenuId MealSettings)) (UserExists (HasRole JSONUnit))

type SetMealSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken (JSONTuple StoredMenuId (JSONTuple StoredMealId MealSettings))) (UserExists (HasRole JSONUnit))
