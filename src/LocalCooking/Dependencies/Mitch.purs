module LocalCooking.Dependencies.Mitch where

import LocalCooking.Semantics.User (UserExists, HasRole)
import LocalCooking.Semantics.Mitch
  ( SetCustomer, CustomerValid, Diets, Allergies, MealSynopsis, MenuSynopsis, ChefSynopsis, Meal, Menu, Chef
  , Review, CartEntry, Order, SubmitReview, AddToCart, BrowseMeal, BrowseMenu
  , CustomerExists, CustomerUnique, ReviewExists, MealExists, MenuExists
  , MenuUnique, MealUnique, RatingExists, MenuPublished, OrderExists)
import LocalCooking.Semantics.Chef (ChefUnique, ChefExists)
import LocalCooking.Database.Schema
  (StoredReviewId, StoredMenuId, StoredChefId)
import Auth.AccessToken.Session (SessionToken)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue
  ( SparrowStaticClientQueues
  , sparrowStaticClientQueues, newSparrowStaticClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.String.Permalink (Permalink)
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


type MitchQueues eff =
  { setCustomerQueues :: SetCustomerSparrowClientQueues eff
  , getCustomerQueues :: GetCustomerSparrowClientQueues eff
  , setDietsQueues :: SetDietsSparrowClientQueues eff
  , getDietsQueues :: GetDietsSparrowClientQueues eff
  , setAllergiesQueues :: SetAllergiesSparrowClientQueues eff
  , getAllergiesQueues :: GetAllergiesSparrowClientQueues eff
  , submitReviewQueues :: SubmitReviewSparrowClientQueues eff
  , getReviewQueues :: GetReviewSparrowClientQueues eff
  , getChefSynopsisQueues :: GetChefSynopsisSparrowClientQueues eff
  , getChefMenuSynopsesQueues :: GetChefMenuSynopsesSparrowClientQueues eff
  , getMenuMealSynopsesQueues :: GetMenuMealSynopsesSparrowClientQueues eff
  , browseChefQueues :: BrowseChefSparrowClientQueues eff
  , browseMenuQueues :: BrowseMenuSparrowClientQueues eff
  , browseMealQueues :: BrowseMealSparrowClientQueues eff
  , getCartQueues :: GetCartSparrowClientQueues eff
  , addToCartQueues :: AddToCartSparrowClientQueues eff
  , getOrdersQueues :: GetOrdersSparrowClientQueues eff
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
  getChefSynopsisQueues <- newSparrowStaticClientQueues
  getChefMenuSynopsesQueues <- newSparrowStaticClientQueues
  getMenuMealSynopsesQueues <- newSparrowStaticClientQueues
  browseChefQueues <- newSparrowStaticClientQueues
  browseMenuQueues <- newSparrowStaticClientQueues
  browseMealQueues <- newSparrowStaticClientQueues
  getCartQueues <- newSparrowStaticClientQueues
  addToCartQueues <- newSparrowStaticClientQueues
  getOrdersQueues <- newSparrowStaticClientQueues
  pure
    { setCustomerQueues
    , getCustomerQueues
    , setDietsQueues
    , getDietsQueues
    , setAllergiesQueues
    , getAllergiesQueues
    , submitReviewQueues
    , getReviewQueues
    , getChefSynopsisQueues
    , getChefMenuSynopsesQueues
    , getMenuMealSynopsesQueues
    , browseChefQueues
    , browseMenuQueues
    , browseMealQueues
    , getCartQueues
    , addToCartQueues
    , getOrdersQueues
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
  , getChefSynopsisQueues
  , getChefMenuSynopsesQueues
  , getMenuMealSynopsesQueues
  , browseChefQueues
  , browseMenuQueues
  , browseMealQueues
  , getCartQueues
  , addToCartQueues
  , getOrdersQueues
  } = do
  unpackClient (Topic ["mitch","setCustomer"]) (sparrowStaticClientQueues setCustomerQueues)
  unpackClient (Topic ["mitch","getCustomer"]) (sparrowStaticClientQueues getCustomerQueues)
  unpackClient (Topic ["mitch","setDiets"]) (sparrowStaticClientQueues setDietsQueues)
  unpackClient (Topic ["mitch","getDiets"]) (sparrowStaticClientQueues getDietsQueues)
  unpackClient (Topic ["mitch","setAllergies"]) (sparrowStaticClientQueues setAllergiesQueues)
  unpackClient (Topic ["mitch","getAllergies"]) (sparrowStaticClientQueues getAllergiesQueues)
  unpackClient (Topic ["mitch","submitReview"]) (sparrowStaticClientQueues submitReviewQueues)
  unpackClient (Topic ["mitch","getReview"]) (sparrowStaticClientQueues getReviewQueues)
  unpackClient (Topic ["mitch","getChefSynopsis"]) (sparrowStaticClientQueues getChefSynopsisQueues)
  unpackClient (Topic ["mitch","getChefMenuSynopses"]) (sparrowStaticClientQueues getChefMenuSynopsesQueues)
  unpackClient (Topic ["mitch","getMenuMealSynopses"]) (sparrowStaticClientQueues getMenuMealSynopsesQueues)
  unpackClient (Topic ["mitch","browseChef"]) (sparrowStaticClientQueues browseChefQueues)
  unpackClient (Topic ["mitch","browseMenu"]) (sparrowStaticClientQueues browseMenuQueues)
  unpackClient (Topic ["mitch","browseMeal"]) (sparrowStaticClientQueues browseMealQueues)
  unpackClient (Topic ["mitch","getCart"]) (sparrowStaticClientQueues getCartQueues)
  unpackClient (Topic ["mitch","addToCart"]) (sparrowStaticClientQueues addToCartQueues)
  unpackClient (Topic ["mitch","getOrders"]) (sparrowStaticClientQueues getOrdersQueues)


type SetCustomerSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken SetCustomer)
    (UserExists JSONUnit)

type GetCustomerSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken JSONUnit)
    (UserExists (CustomerExists CustomerValid))

type SetDietsSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken Diets)
    (UserExists (CustomerUnique JSONUnit))

type GetDietsSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken JSONUnit)
    (UserExists (CustomerUnique (CustomerExists Diets)))

type SetAllergiesSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken Allergies)
    (UserExists (CustomerUnique JSONUnit))

type GetAllergiesSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken JSONUnit)
    (UserExists (CustomerUnique (CustomerExists Allergies)))

type SubmitReviewSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken SubmitReview)
    (UserExists (HasRole (CustomerUnique (OrderExists StoredReviewId))))

type GetReviewSparrowClientQueues eff =
  SparrowStaticClientQueues eff StoredReviewId
    (ReviewExists Review)

type GetChefSynopsisSparrowClientQueues eff =
  SparrowStaticClientQueues eff StoredChefId
    (ChefExists (ReviewExists ChefSynopsis))

type GetChefMenuSynopsesSparrowClientQueues eff =
  SparrowStaticClientQueues eff StoredChefId
    (ChefExists (Array (MenuExists MenuSynopsis)))

type GetMenuMealSynopsesSparrowClientQueues eff =
  SparrowStaticClientQueues eff StoredMenuId
    (MenuExists (Array (MealExists (RatingExists MealSynopsis))))

type BrowseChefSparrowClientQueues eff =
  SparrowStaticClientQueues eff Permalink
    (ChefUnique (ChefExists (ReviewExists Chef)))

type BrowseMenuSparrowClientQueues eff =
  SparrowStaticClientQueues eff BrowseMenu
    (ChefUnique (MenuUnique (MenuPublished (ChefExists (ReviewExists (MenuExists Menu))))))

type BrowseMealSparrowClientQueues eff =
  SparrowStaticClientQueues eff BrowseMeal
    (ChefUnique (MenuUnique (MealUnique (MealExists (RatingExists Meal)))))

type GetCartSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken JSONUnit)
    (UserExists (Array CartEntry))

type AddToCartSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken AddToCart)
    (UserExists (MealExists JSONUnit))

type GetOrdersSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken AddToCart)
    (UserExists (CustomerUnique (Array (MealExists (RatingExists Order)))))
