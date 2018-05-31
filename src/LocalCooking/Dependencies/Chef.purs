module LocalCooking.Dependencies.Chef where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Semantics.Chef
  ( ChefSettings, MenuSettings, MealSettings)
import LocalCooking.Database.Schema (StoredChefId, StoredMealId, StoredMenuId)

import Sparrow.Client.Queue (SparrowStaticClientQueues)

import Prelude
import Data.Date (Date)
import Data.Date.JSON (JSONDate (..))
import Data.String.Permalink (Permalink)
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson, fail)
import Data.Generic (class Generic)
import Text.Email.Validate (EmailAddress)


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
