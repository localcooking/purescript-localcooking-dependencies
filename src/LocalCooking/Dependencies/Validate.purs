module LocalCooking.Dependencies.Validate where

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
import Data.Argonaut.JSONTuple (JSONTuple)
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

newtype IsUniqueMenuDeadline = IsUniqueMenuDeadline
  { chef :: Permalink
  , deadline :: Date
  }

derive instance genericIsUniqueMenuDeadline :: Generic IsUniqueMenuDeadline

instance eqIsUniqueMenuDeadline :: Eq IsUniqueMenuDeadline where
  eq = gEq

instance showIsUniqueMenuDeadline :: Show IsUniqueMenuDeadline where
  show = gShow

instance arbitraryIsUniqueMenuDeadline :: Arbitrary IsUniqueMenuDeadline where
  arbitrary = do
    chef <- arbitrary
    JSONDate deadline <- arbitrary
    pure (IsUniqueMenuDeadline {chef,deadline})

instance encodeJsonIsUniqueMenuDeadline :: EncodeJson IsUniqueMenuDeadline where
  encodeJson (IsUniqueMenuDeadline {chef,deadline})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> jsonEmptyObject

instance decodeJsonIsUniqueMenuDeadline :: DecodeJson IsUniqueMenuDeadline where
  decodeJson json = do
    o <- decodeJson json
    chef <- o .? "chef"
    JSONDate deadline <- o .? "deadline"
    pure (IsUniqueMenuDeadline {chef,deadline})

type UniqueMenuDeadlineSparrowClientQueues eff =
  SparrowStaticClientQueues eff IsUniqueMenuDeadline JSONUnit

newtype IsUniqueMealPermalink = IsUniqueMealPermalink
  { chef :: Permalink
  , deadline :: Date
  , meal :: Permalink
  }

derive instance genericIsUniqueMealPermalink :: Generic IsUniqueMealPermalink

instance eqIsUniqueMealPermalink :: Eq IsUniqueMealPermalink where
  eq = gEq

instance showIsUniqueMealPermalink :: Show IsUniqueMealPermalink where
  show = gShow

instance arbitraryIsUniqueMealPermalink :: Arbitrary IsUniqueMealPermalink where
  arbitrary = do
    chef <- arbitrary
    JSONDate deadline <- arbitrary
    meal <- arbitrary
    pure (IsUniqueMealPermalink {chef,deadline,meal})

instance encodeJsonIsUniqueMealPermalink :: EncodeJson IsUniqueMealPermalink where
  encodeJson (IsUniqueMealPermalink {chef,deadline,meal})
    =  "chef" := chef
    ~> "deadline" := JSONDate deadline
    ~> "meal" := meal
    ~> jsonEmptyObject

instance decodeJsonIsUniqueMealPermalink :: DecodeJson IsUniqueMealPermalink where
  decodeJson json = do
    o <- decodeJson json
    chef <- o .? "chef"
    JSONDate deadline <- o .? "deadline"
    meal <- o .? "meal"
    pure (IsUniqueMealPermalink {chef,deadline,meal})

type UniqueMealPermalinkSparrowClientQueues eff =
  SparrowStaticClientQueues eff IsUniqueMealPermalink JSONUnit


type PasswordVerifySparrowClientQueues eff =
  SparrowStaticClientQueues eff (JSONTuple AuthToken HashedPassword) JSONUnit


newtype PasswordVerifyUnauth = PasswordVerifyUnauth
  { email :: EmailAddress
  , password :: HashedPassword
  }

derive instance genericPasswordVerifyUnauth :: Generic PasswordVerifyUnauth

instance eqPasswordVerifyUnauth :: Eq PasswordVerifyUnauth where
  eq = gEq

instance showPasswordVerifyUnauth :: Show PasswordVerifyUnauth where
  show = gShow

instance arbitraryPasswordVerifyUnauth :: Arbitrary PasswordVerifyUnauth where
  arbitrary = do
    email <- arbitrary
    password <- arbitrary
    pure (PasswordVerifyUnauth {email,password})

instance encodeJsonPasswordVerifyUnauth :: EncodeJson PasswordVerifyUnauth where
  encodeJson (PasswordVerifyUnauth {email,password})
    =  "email" := email
    ~> "password" := password
    ~> jsonEmptyObject

instance decodeJsonPasswordVerifyUnauth :: DecodeJson PasswordVerifyUnauth where
  decodeJson json = do
    o <- decodeJson json
    email <- o .? "email"
    password <- o .? "password"
    pure (PasswordVerifyUnauth {email,password})


type PasswordVerifyUnauthSparrowClientQueues eff =
  SparrowStaticClientQueues eff PasswordVerifyUnauth JSONUnit
