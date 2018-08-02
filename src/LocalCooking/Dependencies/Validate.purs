module LocalCooking.Dependencies.Validate where

import LocalCooking.Semantics.User (UserExists)
import LocalCooking.Semantics.Validate
  (PasswordVerifyUnauth, IsUniqueMealPermalink, IsUniqueMenuDeadline)
import Auth.AccessToken.Session (SessionToken)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue (SparrowStaticClientQueues, sparrowStaticClientQueues, newSparrowStaticClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Password (HashedPassword)
import Data.Date (Date)
import Data.String.Permalink (Permalink)
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Argonaut.JSONTuple (JSONTuple)
import Data.Argonaut.JSONDate (JSONDate (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson)
import Data.Generic (class Generic, gEq, gShow)
import Data.Functor.Singleton (class SingletonFunctor)
import Text.Email.Validate (EmailAddress)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Test.QuickCheck (class Arbitrary, arbitrary)



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , console :: CONSOLE
  | eff)


type ValidateQueues eff =
  { uniqueEmailQueues :: UniqueEmailSparrowClientQueues eff
  , uniqueChefPermalinkQueues :: UniqueChefPermalinkSparrowClientQueues eff
  , uniqueMenuDeadlineQueues :: UniqueMenuDeadlineSparrowClientQueues eff
  , uniqueMealPermalinkQueues :: UniqueMealPermalinkSparrowClientQueues eff
  , passwordVerifyQueues :: PasswordVerifySparrowClientQueues eff
  , passwordVerifyUnauthQueues :: PasswordVerifyUnauthSparrowClientQueues eff
  }


newValidateQueues :: forall eff. Eff (Effects eff) (ValidateQueues (Effects eff))
newValidateQueues = do
  uniqueEmailQueues <- newSparrowStaticClientQueues
  uniqueChefPermalinkQueues <- newSparrowStaticClientQueues
  uniqueMenuDeadlineQueues <- newSparrowStaticClientQueues
  uniqueMealPermalinkQueues <- newSparrowStaticClientQueues
  passwordVerifyQueues <- newSparrowStaticClientQueues
  passwordVerifyUnauthQueues <- newSparrowStaticClientQueues
  pure
    { uniqueEmailQueues
    , uniqueChefPermalinkQueues
    , uniqueMenuDeadlineQueues
    , uniqueMealPermalinkQueues
    , passwordVerifyQueues
    , passwordVerifyUnauthQueues
    }


validateDependencies :: forall eff stM m
                      . MonadBaseControl (Eff (Effects eff)) m stM
                     => MonadEff (Effects eff) m
                     => SingletonFunctor stM
                     => ValidateQueues (Effects eff)
                     -> SparrowClientT (Effects eff) m Unit
validateDependencies
  { uniqueEmailQueues
  , uniqueChefPermalinkQueues
  , uniqueMenuDeadlineQueues
  , uniqueMealPermalinkQueues
  , passwordVerifyQueues
  , passwordVerifyUnauthQueues
  } = do
  unpackClient (Topic ["validate","uniqueEmail"]) (sparrowStaticClientQueues uniqueEmailQueues)
  unpackClient (Topic ["validate","uniqueChefPermalink"]) (sparrowStaticClientQueues uniqueChefPermalinkQueues)
  unpackClient (Topic ["validate","uniqueMenuDeadline"]) (sparrowStaticClientQueues uniqueMenuDeadlineQueues)
  unpackClient (Topic ["validate","uniqueMealPermalink"]) (sparrowStaticClientQueues uniqueMealPermalinkQueues)
  unpackClient (Topic ["validate","passwordVerify"]) (sparrowStaticClientQueues passwordVerifyQueues)
  unpackClient (Topic ["validate","passwordVerifyUnauth"]) (sparrowStaticClientQueues passwordVerifyUnauthQueues)


type UniqueEmailSparrowClientQueues eff =
  SparrowStaticClientQueues eff EmailAddress JSONUnit

type UniqueChefPermalinkSparrowClientQueues eff =
  SparrowStaticClientQueues eff Permalink JSONUnit

type UniqueMenuDeadlineSparrowClientQueues eff =
  SparrowStaticClientQueues eff IsUniqueMenuDeadline JSONUnit

type UniqueMealPermalinkSparrowClientQueues eff =
  SparrowStaticClientQueues eff IsUniqueMealPermalink JSONUnit

type PasswordVerifySparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple SessionToken HashedPassword) (UserExists Boolean)

type PasswordVerifyUnauthSparrowClientQueues eff =
  SparrowStaticClientQueues eff PasswordVerifyUnauth (UserExists Boolean)
