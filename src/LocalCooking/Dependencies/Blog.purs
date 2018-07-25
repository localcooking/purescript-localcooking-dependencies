module LocalCooking.Dependencies.Blog where

import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Blog (BlogPostVariant)
import LocalCooking.Semantics.Blog
  ( GetBlogPost, NewBlogPost, SetBlogPost, BlogPostSynopsis
  , GetBlogPostCategory, NewBlogPostCategory, SetBlogPostCategory, BlogPostCategorySynopsis)
import LocalCooking.Database.Schema (StoredBlogPostId, StoredBlogPostCategoryId)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue (SparrowStaticClientQueues, sparrowStaticClientQueues, newSparrowStaticClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Argonaut.JSONTuple (JSONTuple)
import Data.Functor.Singleton (class SingletonFunctor)
import Data.String.Permalink (Permalink)
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


type BlogQueues eff =
  { getBlogPostCategoryQueues :: GetBlogPostCategorySparrowClientQueues eff
  , newBlogPostCategoryQueues :: NewBlogPostCategorySparrowClientQueues eff
  , setBlogPostCategoryQueues :: SetBlogPostCategorySparrowClientQueues eff
  , getBlogPostCategoriesQueues :: GetBlogPostCategoriesSparrowClientQueues eff
  , getBlogPostQueues :: GetBlogPostSparrowClientQueues eff
  , newBlogPostQueues :: NewBlogPostSparrowClientQueues eff
  , setBlogPostQueues :: SetBlogPostSparrowClientQueues eff
  , getBlogPostsQueues :: GetBlogPostsSparrowClientQueues eff
  }


newBlogQueues :: forall eff. Eff (Effects eff) (BlogQueues (Effects eff))
newBlogQueues = do
  getBlogPostCategoryQueues <- newSparrowStaticClientQueues
  newBlogPostCategoryQueues <- newSparrowStaticClientQueues
  setBlogPostCategoryQueues <- newSparrowStaticClientQueues
  getBlogPostCategoriesQueues <- newSparrowStaticClientQueues
  getBlogPostQueues <- newSparrowStaticClientQueues
  newBlogPostQueues <- newSparrowStaticClientQueues
  setBlogPostQueues <- newSparrowStaticClientQueues
  getBlogPostsQueues <- newSparrowStaticClientQueues
  pure
    { getBlogPostCategoryQueues
    , newBlogPostCategoryQueues
    , setBlogPostCategoryQueues
    , getBlogPostCategoriesQueues
    , getBlogPostQueues
    , newBlogPostQueues
    , setBlogPostQueues
    , getBlogPostsQueues
    }


blogDependencies :: forall eff stM m
                      . MonadBaseControl (Eff (Effects eff)) m stM
                     => MonadEff (Effects eff) m
                     => SingletonFunctor stM
                     => BlogQueues (Effects eff)
                     -> SparrowClientT (Effects eff) m Unit
blogDependencies
  { getBlogPostCategoryQueues
  , newBlogPostCategoryQueues
  , setBlogPostCategoryQueues
  , getBlogPostCategoriesQueues
  , getBlogPostQueues
  , newBlogPostQueues
  , setBlogPostQueues
  , getBlogPostsQueues
  } = do
  unpackClient (Topic ["blog","getBlogPostCategory"]) (sparrowStaticClientQueues getBlogPostCategoryQueues)
  unpackClient (Topic ["blog","newBlogPostCategory"]) (sparrowStaticClientQueues newBlogPostCategoryQueues)
  unpackClient (Topic ["blog","setBlogPostCategory"]) (sparrowStaticClientQueues setBlogPostCategoryQueues)
  unpackClient (Topic ["blog","getBlogPostCategories"]) (sparrowStaticClientQueues getBlogPostCategoriesQueues)
  unpackClient (Topic ["blog","getBlogPost"]) (sparrowStaticClientQueues getBlogPostQueues)
  unpackClient (Topic ["blog","newBlogPost"]) (sparrowStaticClientQueues newBlogPostQueues)
  unpackClient (Topic ["blog","setBlogPost"]) (sparrowStaticClientQueues setBlogPostQueues)
  unpackClient (Topic ["blog","getBlogPosts"]) (sparrowStaticClientQueues getBlogPostsQueues)


type GetBlogPostCategoriesSparrowClientQueues eff =
  SparrowStaticClientQueues eff BlogPostVariant (Array BlogPostCategorySynopsis)

type GetBlogPostCategorySparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple BlogPostVariant Permalink) GetBlogPostCategory

type NewBlogPostCategorySparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple AuthToken NewBlogPostCategory) StoredBlogPostCategoryId

type SetBlogPostCategorySparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple AuthToken SetBlogPostCategory) JSONUnit


type GetBlogPostsSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple BlogPostVariant Permalink) (Array BlogPostSynopsis)

type GetBlogPostSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple BlogPostVariant (JSONTuple Permalink Permalink)) GetBlogPost

type NewBlogPostSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple AuthToken NewBlogPost) StoredBlogPostId

type SetBlogPostSparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple AuthToken SetBlogPost) JSONUnit
