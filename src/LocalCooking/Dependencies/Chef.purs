module LocalCooking.Dependencies.Chef where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Semantics.Chef
  ( ChefSettings, MenuSettings, MealSettings)
import LocalCooking.Database.Schema (StoredChefId, StoredMealId, StoredMenuId)

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


type ChefQueues eff =
  { addMealTagQueues :: AddMealTagSparrowClientQueues eff
  , addChefTagQueues :: AddChefTagSparrowClientQueues eff
  , getChefQueues :: GetChefSparrowClientQueues eff
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
  addMealTagQueues <- newSparrowStaticClientQueues
  addChefTagQueues <- newSparrowStaticClientQueues
  getChefQueues <- newSparrowStaticClientQueues
  setChefQueues <- newSparrowStaticClientQueues
  getMenusQueues <- newSparrowStaticClientQueues
  setMenuQueues <- newSparrowStaticClientQueues
  newMenuQueues <- newSparrowStaticClientQueues
  getMealsQueues <- newSparrowStaticClientQueues
  setMealQueues <- newSparrowStaticClientQueues
  newMealQueues <- newSparrowStaticClientQueues
  pure
    { addMealTagQueues
    , addChefTagQueues
    , getChefQueues
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
  { addMealTagQueues
  , addChefTagQueues
  , getChefQueues
  , setChefQueues
  , getMenusQueues
  , setMenuQueues
  , newMenuQueues
  , getMealsQueues
  , setMealQueues
  , newMealQueues
  } = do
  unpackClient (Topic ["chef","addMealTag"]) (sparrowStaticClientQueues addMealTagQueues)
  unpackClient (Topic ["chef","addChefTag"]) (sparrowStaticClientQueues addChefTagQueues)
  unpackClient (Topic ["chef","getChef"]) (sparrowStaticClientQueues getChefQueues)
  unpackClient (Topic ["chef","setChef"]) (sparrowStaticClientQueues setChefQueues)
  unpackClient (Topic ["chef","getMenus"]) (sparrowStaticClientQueues getMenusQueues)
  unpackClient (Topic ["chef","setMenu"]) (sparrowStaticClientQueues setMenuQueues)
  unpackClient (Topic ["chef","newMenu"]) (sparrowStaticClientQueues newMenuQueues)
  unpackClient (Topic ["chef","getMeals"]) (sparrowStaticClientQueues getMealsQueues)
  unpackClient (Topic ["chef","setMeal"]) (sparrowStaticClientQueues setMealQueues)
  unpackClient (Topic ["chef","newMeal"]) (sparrowStaticClientQueues newMealQueues)


type AddMealTagSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken MealTag) JSONUnit


type AddChefTagSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken ChefTag) JSONUnit


type GetChefSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken JSONUnit) ChefSettings


type SetChefSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken ChefSettings) StoredChefId


newtype WithId k a = WithId
  { id :: k
  , content :: a
  }

derive instance genericWithId :: (Generic k, Generic a) => Generic (WithId k a)

instance encodeJsonWithId :: (EncodeJson k, EncodeJson a) => EncodeJson (WithId k a) where
  encodeJson (WithId {id,content})
    =  "id" := id
    ~> "content" := content
    ~> jsonEmptyObject

instance decodeJsonWithId :: (DecodeJson k, DecodeJson a) => DecodeJson (WithId k a) where
  decodeJson json = do
    o <- decodeJson json
    id <- o .? "id"
    content <- o .? "content"
    pure (WithId {id,content})

type GetMenusSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken JSONUnit) (Array (WithId StoredMenuId MenuSettings))


type NewMenuSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken MenuSettings) StoredMenuId


type SetMenuSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken (WithId StoredMenuId MenuSettings)) JSONUnit


type GetMealsSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken StoredMenuId) (Array (WithId StoredMealId MealSettings))


type NewMealSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken (WithId StoredMenuId MealSettings)) StoredMealId


type SetMealSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken (WithId StoredMenuId (WithId StoredMealId MealSettings))) JSONUnit
