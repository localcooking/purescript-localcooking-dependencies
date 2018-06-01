module LocalCooking.Dependencies.Validate where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Auth (AuthToken)

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

newtype IsUniqueMenuDeadline = IsUniqueMenuDeadline
  { chef :: Permalink
  , deadline :: Date
  }

derive instance genericIsUniqueMenuDeadline :: Generic IsUniqueMenuDeadline

instance encodeJsonIsUniqueMenuDeadline :: EncodeJson IsUniqueMenuDeadline where
  encodeJson (IsUniqueMenuDeadline {chef,deadline})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> jsonEmptyObject

type UniqueMenuDeadlineSparrowClientQueues eff =
  SparrowStaticClientQueues eff IsUniqueMenuDeadline JSONUnit

newtype IsUniqueMealPermalink = IsUniqueMealPermalink
  { chef :: Permalink
  , deadline :: Date
  , meal :: Permalink
  }

-- TODO FIXME prove isomorphic

instance encodeJsonIsUniqueMealPermalink :: EncodeJson IsUniqueMealPermalink where
  encodeJson (IsUniqueMealPermalink {chef,deadline,meal})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> "meal" := meal
    ~> jsonEmptyObject

type UniqueMealPermalinkSparrowClientQueues eff =
  SparrowStaticClientQueues eff IsUniqueMealPermalink JSONUnit


type PasswordVerifySparrowClientQueues eff =
  SparrowStaticClientQueues eff (AccessInitIn AuthToken HashedPassword) JSONUnit


newtype PasswordVerifyUnauth = PasswordVerifyUnauth
  { email :: EmailAddress
  , password :: HashedPassword
  }

derive instance genericPasswordVerifyUnauth :: Generic PasswordVerifyUnauth

instance encodeJsonPasswordVerifyUnauth :: EncodeJson PasswordVerifyUnauth where
  encodeJson (PasswordVerifyUnauth {email,password})
    =  "email" := email
    ~> "password" := password
    ~> jsonEmptyObject


type PasswordVerifyUnauthSparrowClientQueues eff =
  SparrowStaticClientQueues eff PasswordVerifyUnauth JSONUnit
