module LocalCooking.Dependencies.Mitch where


import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Semantics.Mitch
  ( Customer, MealSynopsis, MenuSynopsis, ChefSynopsis, Meal, Menu, Chef
  , Review, CartEntry, Order)
import LocalCooking.Database.Schema
  ( StoredOrderId, StoredReviewId, StoredMealId, StoredMenuId, StoredChefId)

import Sparrow.Client.Queue (SparrowStaticClientQueues)

import Data.Image.Source (ImageSource)
import Data.Date (Date)
import Data.Date.JSON (JSONDate (..))
import Data.String.Permalink (Permalink)
import Data.String.Markdown (MarkdownText)
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson, fail)
import Data.Generic (class Generic)
import Text.Email.Validate (EmailAddress)


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
