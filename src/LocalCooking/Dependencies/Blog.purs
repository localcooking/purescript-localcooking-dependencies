module LocalCooking.Dependencies.Blog where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Semantics.Common (WithId)
import LocalCooking.Semantics.Blog
  (GetBlogPost, NewBlogPost, SetBlogPost, BlogPostSynopsis)
import LocalCooking.Database.Schema (StoredBlogPostId)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue (SparrowStaticClientQueues, sparrowStaticClientQueues, newSparrowStaticClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Functor.Singleton (class SingletonFunctor)
import Data.String.Permalink (Permalink)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  | eff)


type BlogQueues eff =
  { getBlogPostQueues :: GetBlogPostSparrowClientQueues eff
  , newBlogPostQueues :: NewBlogPostSparrowClientQueues eff
  , setBlogPostQueues :: SetBlogPostSparrowClientQueues eff
  , getBlogPostsQueues :: GetBlogPostsSparrowClientQueues eff
  }


newBlogQueues :: forall eff. Eff (Effects eff) (BlogQueues (Effects eff))
newBlogQueues = do
  getBlogPostQueues <- newSparrowStaticClientQueues
  newBlogPostQueues <- newSparrowStaticClientQueues
  setBlogPostQueues <- newSparrowStaticClientQueues
  getBlogPostsQueues <- newSparrowStaticClientQueues
  pure
    { getBlogPostQueues
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
  { getBlogPostQueues
  , newBlogPostQueues
  , setBlogPostQueues
  , getBlogPostsQueues
  } = do
  unpackClient (Topic ["blog","getBlogPost"]) (sparrowStaticClientQueues getBlogPostQueues)
  unpackClient (Topic ["blog","newBlogPost"]) (sparrowStaticClientQueues newBlogPostQueues)
  unpackClient (Topic ["blog","setBlogPost"]) (sparrowStaticClientQueues setBlogPostQueues)
  unpackClient (Topic ["blog","getBlogPosts"]) (sparrowStaticClientQueues getBlogPostsQueues)


type GetBlogPostsSparrowClientQueues eff =
  SparrowStaticClientQueues eff JSONUnit (Array (WithId StoredBlogPostId BlogPostSynopsis))

type GetBlogPostSparrowClientQueues eff =
  SparrowStaticClientQueues eff Permalink GetBlogPost

type NewBlogPostSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken NewBlogPost) StoredBlogPostId

type SetBlogPostSparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken (WithId StoredBlogPostId SetBlogPost)) JSONUnit
