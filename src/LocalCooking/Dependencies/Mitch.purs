module LocalCooking.Dependencies.Mitch where


import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Semantics.Mitch
  ( Customer, Diets, Allergies, MealSynopsis, MenuSynopsis, ChefSynopsis, Meal, Menu, Chef
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
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson)
import Data.Generic (class Generic, gEq, gShow)
import Data.Functor.Singleton (class SingletonFunctor)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Test.QuickCheck (class Arbitrary, arbitrary)



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  | eff)


type MitchQueues eff =
  { setCustomerQueues :: SetCustomerSparrowClientQueues eff
  , getCustomerQueues :: GetCustomerSparrowClientQueues eff
  , setDietsQueues :: SetDietsSparrowClientQueues eff
  , getDietsQueues :: GetDietsSparrowClientQueues eff
  , setAllergiesQueues :: SetAllergiesSparrowClientQueues eff
  , getAllergiesQueues :: GetAllergiesSparrowClientQueues eff
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
  setDietsQueues <- newSparrowStaticClientQueues
  getDietsQueues <- newSparrowStaticClientQueues
  setAllergiesQueues <- newSparrowStaticClientQueues
  getAllergiesQueues <- newSparrowStaticClientQueues
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
    , setDietsQueues
    , getDietsQueues
    , setAllergiesQueues
    , getAllergiesQueues
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
  , setDietsQueues
  , getDietsQueues
  , setAllergiesQueues
  , getAllergiesQueues
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
  unpackClient (Topic ["mitch","setDiets"]) (sparrowStaticClientQueues setDietsQueues)
  unpackClient (Topic ["mitch","getDiets"]) (sparrowStaticClientQueues getDietsQueues)
  unpackClient (Topic ["mitch","setAllergies"]) (sparrowStaticClientQueues setAllergiesQueues)
  unpackClient (Topic ["mitch","getAllergies"]) (sparrowStaticClientQueues getAllergiesQueues)
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


type SetDietsSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken Diets) JSONUnit

type GetDietsSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken JSONUnit) Diets


type SetAllergiesSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken Allergies) JSONUnit

type GetAllergiesSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken JSONUnit) Allergies


newtype SubmitReview = SubmitReview
  { order :: StoredOrderId
  , rating :: Rating
  , heading :: String
  , body :: MarkdownText
  , images :: Array ImageSource
  }

derive instance genericSubmitReview :: Generic SubmitReview

instance eqSubmitReview :: Eq SubmitReview where
  eq = gEq

instance showSubmitReview :: Show SubmitReview where
  show = gShow

instance arbitrarySubmitReview :: Arbitrary SubmitReview where
  arbitrary = do
    order <- arbitrary
    rating <- arbitrary
    heading <- arbitrary
    body <- arbitrary
    images <- arbitrary
    pure (SubmitReview {order,rating,heading,body,images})

instance encodeJsonSubmitReview :: EncodeJson SubmitReview where
  encodeJson (SubmitReview {order,rating,heading,body,images})
    =  "order" := order
    ~> "rating" := rating
    ~> "heading" := heading
    ~> "body" := body
    ~> "images" := images
    ~> jsonEmptyObject

instance decodeJsonSubmitReview :: DecodeJson SubmitReview where
  decodeJson json = do
    o <- decodeJson json
    order <- o .? "order"
    rating <- o .? "rating"
    heading <- o .? "heading"
    body <- o .? "body"
    images <- o .? "images"
    pure (SubmitReview {order,rating,heading,body,images})

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

instance eqBrowseMenu :: Eq BrowseMenu where
  eq = gEq

instance showBrowseMenu :: Show BrowseMenu where
  show = gShow


instance arbitraryBrowseMenu :: Arbitrary BrowseMenu where
  arbitrary = do
    chef <- arbitrary
    JSONDate deadline <- arbitrary
    pure (BrowseMenu {chef,deadline})


instance encodeJsonBrowseMenu :: EncodeJson BrowseMenu where
  encodeJson (BrowseMenu {chef,deadline})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> jsonEmptyObject

instance decodeJsonBrowseMenu :: DecodeJson BrowseMenu where
  decodeJson json = do
    o <- decodeJson json
    chef <- o .? "chef"
    JSONDate deadline <- o .? "deadline"
    pure (BrowseMenu {chef,deadline})


type BrowseMenuSparrowClientQueues eff =
  SparrowStaticClientQueues eff BrowseMenu Menu


newtype BrowseMeal = BrowseMeal
  { chef :: Permalink
  , deadline :: Date
  , meal :: Permalink
  }

derive instance genericBrowseMeal :: Generic BrowseMeal

instance eqBrowseMeal :: Eq BrowseMeal where
  eq = gEq

instance showBrowseMeal :: Show BrowseMeal where
  show = gShow


instance arbitraryBrowseMeal :: Arbitrary BrowseMeal where
  arbitrary = do
    chef <- arbitrary
    JSONDate deadline <- arbitrary
    meal <- arbitrary
    pure (BrowseMeal {chef,deadline,meal})

instance encodeJsonBrowseMeal :: EncodeJson BrowseMeal where
  encodeJson (BrowseMeal {chef,deadline,meal})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> "meal" := meal
    ~> jsonEmptyObject

instance decodeJsonBrowseMeal :: DecodeJson BrowseMeal where
  decodeJson json = do
    o <- decodeJson json
    chef <- o .? "chef"
    JSONDate deadline <- o .? "deadline"
    meal <- o .? "meal"
    pure (BrowseMeal {chef,deadline,meal})


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

instance eqAddToCart :: Eq AddToCart where
  eq = gEq

instance showAddToCart :: Show AddToCart where
  show = gShow

instance arbitraryAddToCart :: Arbitrary AddToCart where
  arbitrary = do
    chef <- arbitrary
    JSONDate deadline <- arbitrary
    meal <- arbitrary
    volume <- arbitrary
    pure (AddToCart {chef,deadline,meal,volume})

instance encodeJsonAddToCart :: EncodeJson AddToCart where
  encodeJson (AddToCart {chef,deadline,meal,volume})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> "meal" := meal
    ~> "volume" := volume
    ~> jsonEmptyObject

instance decodeJsonAddToCart :: DecodeJson AddToCart where
  decodeJson json = do
    o <- decodeJson json
    chef <- o .? "chef"
    JSONDate deadline <- o .? "deadline"
    meal <- o .? "meal"
    volume <- o .? "volume"
    pure (AddToCart {chef,deadline,meal,volume})


type AddToCartSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken AddToCart) JSONUnit


type GetOrdersSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken AddToCart) (Array Order)


type SearchMealTagsSparrowClientQueues eff =
  SparrowClientQueues eff JSONUnit JSONUnit String (Maybe (Array MealTag))


type SearchChefTagsSparrowClientQueues eff =
  SparrowClientQueues eff JSONUnit JSONUnit String (Maybe (Array ChefTag))
