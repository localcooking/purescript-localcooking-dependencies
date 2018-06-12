module LocalCooking.Dependencies.Mitch where


import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Semantics.Mitch
  ( Customer, MealSynopsis, MenuSynopsis, ChefSynopsis, Meal, Menu, Chef
  , Review, CartEntry, Order)
import LocalCooking.Database.Schema
  ( StoredOrderId, StoredReviewId, StoredMealId, StoredMenuId, StoredChefId)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue
  ( SparrowStaticClientQueues, SparrowClientQueues
  , sparrowStaticClientQueues, newSparrowStaticClientQueues
  , sparrowClientQueues, newSparrowClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Maybe (Maybe)
import Data.Image.Source (ImageSource)
import Data.Date (Date)
import Data.Date.JSON (JSONDate (..))
import Data.String.Permalink (Permalink)
import Data.String.Markdown (MarkdownText)
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson, fail)
import Data.Generic (class Generic)
import Data.Functor.Singleton (class SingletonFunctor)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  | eff)


type MitchQueues eff =
  { setCustomerQueues :: SetCustomerSparrowClientQueues eff
  , getCustomerQueues :: SetCustomerSparrowClientQueues eff
  , submitReviewQueues :: SetCustomerSparrowClientQueues eff
  , getReviewQueues :: SetCustomerSparrowClientQueues eff
  , getMealSynopsisQueues :: SetCustomerSparrowClientQueues eff
  , getChefSynopsisQueues :: SetCustomerSparrowClientQueues eff
  , getChefMenuSynopsesQueues :: SetCustomerSparrowClientQueues eff
  , getMenuMealSynopsesQueues :: SetCustomerSparrowClientQueues eff
  , browseChefQueues :: SetCustomerSparrowClientQueues eff
  , browseMenuQueues :: SetCustomerSparrowClientQueues eff
  , browseMealQueues :: SetCustomerSparrowClientQueues eff
  , getCartQueues :: SetCustomerSparrowClientQueues eff
  , addToCartQueues :: SetCustomerSparrowClientQueues eff
  , getOrdersQueues :: SetCustomerSparrowClientQueues eff
  , searchMealTagsQueues :: SearchMealTagsSparrowClientQueues eff
  , searchChefTagsQueues :: SearchChefTagsSparrowClientQueues eff
  }


newMitchQueues :: forall eff. Eff (Effects eff) (MitchQueues (Effects eff))
newMitchQueues = do
  setCustomerQueues <- newSparrowStaticClientQueues
  getCustomerQueues <- newSparrowStaticClientQueues
  submitReviewQueues <- newSparrowStaticClientQueues
  getReviewQueues <- newSparrowStaticClientQueues
  getMealSynopsisQueues <- newSparrowStaticClientQueues
  getChefSynopsisQueues <- newSparrowStaticClientQueues
  getChefMenuSynopsesQueues <- newSparrowStaticClientQueues
  getMenuMealSynopsesQueues <- newSparrowStaticClientQueues
  browseChefQueues <- newSparrowStaticClientQueues
  browseMenuQueues <- newSparrowStaticClientQueues
  browseMealQueues <- newSparrowStaticClientQueues
  getCartQueues <- newSparrowStaticClientQueues
  addToCartQueues <- newSparrowStaticClientQueues
  getOrdersQueues <- newSparrowStaticClientQueues
  searchMealTagsQueues <- newSparrowClientQueues
  searchChefTagsQueues <- newSparrowClientQueues
  pure
    { setCustomerQueues
    , getCustomerQueues
    , submitReviewQueues
    , getReviewQueues
    , getMealSynopsisQueues
    , getChefSynopsisQueues
    , getChefMenuSynopsesQueues
    , getMenuMealSynopsesQueues
    , browseChefQueues
    , browseMenuQueues
    , browseMealQueues
    , getCartQueues
    , addToCartQueues
    , getOrdersQueues
    , searchMealTagsQueues
    , searchChefTagsQueues
    }


mitchDependencies :: forall eff stM m
                   . MonadBaseControl (Eff (Effects eff)) m stM
                  => MonadEff (Effects eff) m
                  => SingletonFunctor stM
                  => MitchQueues (Effects eff)
                  -> SparrowClientT (Effects eff) m Unit
mitchDependencies
  { setCustomerQueues
  , getCustomerQueues
  , submitReviewQueues
  , getReviewQueues
  , getMealSynopsisQueues
  , getChefSynopsisQueues
  , getChefMenuSynopsesQueues
  , getMenuMealSynopsesQueues
  , browseChefQueues
  , browseMenuQueues
  , browseMealQueues
  , getCartQueues
  , addToCartQueues
  , getOrdersQueues
  , searchMealTagsQueues
  , searchChefTagsQueues
  } = do
  unpackClient (Topic ["mitch","setCustomer"]) (sparrowStaticClientQueues setCustomerQueues)
  unpackClient (Topic ["mitch","getCustomer"]) (sparrowStaticClientQueues getCustomerQueues)
  unpackClient (Topic ["mitch","submitReview"]) (sparrowStaticClientQueues submitReviewQueues)
  unpackClient (Topic ["mitch","getReview"]) (sparrowStaticClientQueues getReviewQueues)
  unpackClient (Topic ["mitch","getMealSynopsis"]) (sparrowStaticClientQueues getMealSynopsisQueues)
  unpackClient (Topic ["mitch","getChefSynopsis"]) (sparrowStaticClientQueues getChefSynopsisQueues)
  unpackClient (Topic ["mitch","getChefMenuSynopses"]) (sparrowStaticClientQueues getChefMenuSynopsesQueues)
  unpackClient (Topic ["mitch","getMenuMealSynopses"]) (sparrowStaticClientQueues getMenuMealSynopsesQueues)
  unpackClient (Topic ["mitch","browseChef"]) (sparrowStaticClientQueues browseChefQueues)
  unpackClient (Topic ["mitch","browseMenu"]) (sparrowStaticClientQueues browseMenuQueues)
  unpackClient (Topic ["mitch","browseMeal"]) (sparrowStaticClientQueues browseMealQueues)
  unpackClient (Topic ["mitch","getCart"]) (sparrowStaticClientQueues getCartQueues)
  unpackClient (Topic ["mitch","addToCart"]) (sparrowStaticClientQueues addToCartQueues)
  unpackClient (Topic ["mitch","getOrders"]) (sparrowStaticClientQueues getOrdersQueues)
  unpackClient (Topic ["mitch","searchMealTags"]) (sparrowClientQueues searchMealTagsQueues)
  unpackClient (Topic ["mitch","searchChefTags"]) (sparrowClientQueues searchChefTagsQueues)


type SetCustomerSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken Customer) JSONUnit

type GetCustomerSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken JSONUnit) Customer


newtype SubmitReview = SubmitReview
  { order :: StoredOrderId
  , rating :: Rating
  , heading :: String
  , body :: MarkdownText
  , images :: Array ImageSource
  }

derive instance genericSubmitReview :: Generic SubmitReview

instance encodeJsonSubmitReview :: EncodeJson SubmitReview where
  encodeJson (SubmitReview {order,rating,heading,body,images})
    =  "order" := order
    ~> "rating" := rating
    ~> "heading" := heading
    ~> "body" := body
    ~> "images" := images
    ~> jsonEmptyObject

type SubmitReviewSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken SubmitReview) StoredReviewId


type GetReviewSparrowClientQueues eff =
  SparrowStaticClientQueues eff StoredReviewId Review


type GetMealSynopsisSparrowClientQueues eff =
  SparrowStaticClientQueues eff StoredMealId MealSynopsis


type GetChefSynopsisSparrowClientQueues eff =
  SparrowStaticClientQueues eff StoredChefId ChefSynopsis


type GetChefMenuSynopsesSparrowClientQueues eff =
  SparrowStaticClientQueues eff StoredChefId (Array MenuSynopsis)


type GetMenuMealSynopsesSparrowClientQueues eff =
  SparrowStaticClientQueues eff StoredMenuId (Array MealSynopsis)


type BrowseChefSparrowClientQueues eff =
  SparrowStaticClientQueues eff Permalink Chef


newtype BrowseMenu = BrowseMenu
  { chef :: Permalink
  , deadline :: Date
  }

derive instance genericBrowseMenu :: Generic BrowseMenu

instance encodeJsonBrowseMenu :: EncodeJson BrowseMenu where
  encodeJson (BrowseMenu {chef,deadline})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> jsonEmptyObject


type BrowseMenuSparrowClientQueues eff =
  SparrowStaticClientQueues eff BrowseMenu Menu


newtype BrowseMeal = BrowseMeal
  { chef :: Permalink
  , deadline :: Date
  , meal :: Permalink
  }

derive instance genericBrowseMeal :: Generic BrowseMeal

instance encodeJsonBrowseMeal :: EncodeJson BrowseMeal where
  encodeJson (BrowseMeal {chef,deadline,meal})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> "meal" := meal
    ~> jsonEmptyObject


type BrowseMealSparrowClientQueues eff =
  SparrowStaticClientQueues eff BrowseMeal Meal


type GetCartSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken JSONUnit) (Array CartEntry)


newtype AddToCart = AddToCart
  { chef :: Permalink
  , deadline :: Date
  , meal :: Permalink
  , volume :: Int
  }

derive instance genericAddToCart :: Generic AddToCart

instance encodeJsonAddToCart :: EncodeJson AddToCart where
  encodeJson (AddToCart {chef,deadline,meal,volume})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> "meal" := meal
    ~> "volume" := volume
    ~> jsonEmptyObject


type AddToCartSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken AddToCart) JSONUnit


type GetOrdersSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken AddToCart) (Array Order)


type SearchMealTagsSparrowClientQueues eff =
  SparrowClientQueues eff JSONUnit JSONUnit String (Maybe (Array MealTag))


type SearchChefTagsSparrowClientQueues eff =
  SparrowClientQueues eff JSONUnit JSONUnit String (Maybe (Array ChefTag))
