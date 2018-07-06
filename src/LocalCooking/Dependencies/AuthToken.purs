module LocalCooking.Dependencies.AuthToken where

import LocalCooking.Dependencies.AccessToken.Generic
  (class AccessTokenInitIn, class AccessTokenInitOut, class AccessTokenDeltaOut)
import LocalCooking.Semantics.Common (Login, SocialLogin)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Global.Error (AuthTokenFailure)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue (SparrowClientQueues, sparrowClientQueues, newSparrowClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Generic (class Generic)
import Data.Newtype (unwrap, wrap)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson, fail, encodeJson)
import Data.Functor.Singleton (class SingletonFunctor)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary)


type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , console :: CONSOLE
  | eff)

type AuthTokenQueues eff =
  { authTokenQueues :: AuthTokenSparrowClientQueues eff
  }


newAuthTokenQueues :: forall eff. Eff (Effects eff) (AuthTokenQueues (Effects eff))
newAuthTokenQueues = do
  authTokenQueues <- newSparrowClientQueues
  pure
    { authTokenQueues
    }


authTokenDependencies :: forall eff stM m
                       . MonadBaseControl (Eff (Effects eff)) m stM
                      => MonadEff (Effects eff) m
                      => SingletonFunctor stM
                      => AuthTokenQueues (Effects eff)
                      -> SparrowClientT (Effects eff) m Unit
authTokenDependencies {authTokenQueues} = do
  unpackClient (Topic ["authToken"]) (sparrowClientQueues authTokenQueues)


data AuthTokenInitIn
  = AuthTokenInitInLogin Login
  | AuthTokenInitInSocialLogin SocialLogin
  | AuthTokenInitInExists AuthToken

instance accessTokenInitInAuthTokenInitIn :: AccessTokenInitIn AuthTokenInitIn where
  makeExists = AuthTokenInitInExists <<< wrap

instance encodeJsonAuthTokenInitIn :: EncodeJson AuthTokenInitIn where
  encodeJson x = case x of
    AuthTokenInitInLogin y -> "login" := y ~> jsonEmptyObject
    AuthTokenInitInSocialLogin y -> "socialLogin" := y ~> jsonEmptyObject
    AuthTokenInitInExists y -> "exists" := y ~> jsonEmptyObject


newtype PreliminaryAuthToken = PreliminaryAuthToken (Either AuthTokenFailure AuthToken)

derive instance genericPreliminaryAuthToken :: Generic PreliminaryAuthToken
derive newtype instance eqPreliminaryAuthToken :: Eq PreliminaryAuthToken
derive newtype instance showPreliminaryAuthToken :: Show PreliminaryAuthToken
derive newtype instance arbitraryPreliminaryAuthToken :: Arbitrary PreliminaryAuthToken

instance encodeJsonPreliminaryAuthToken :: EncodeJson PreliminaryAuthToken where
  encodeJson (PreliminaryAuthToken eTkn) = case eTkn of
    Left e -> "err" := e ~> jsonEmptyObject
    Right x -> "token" := x ~> jsonEmptyObject

instance decodeJsonPreliminaryAuthToken :: DecodeJson PreliminaryAuthToken where
  decodeJson json = do
    o <- decodeJson json
    let err = PreliminaryAuthToken <<< Left <$> o .? "err"
        token = PreliminaryAuthToken <<< Right <$> o .? "token"
    err <|> token
        



data AuthTokenInitOut
  = AuthTokenInitOutSuccess AuthToken
  | AuthTokenInitOutFailure AuthTokenFailure

instance accessTokenInitOutAuthTokenInitOut :: AccessTokenInitOut AuthTokenInitOut AuthTokenFailure where
  getSuccess x = case x of
    AuthTokenInitOutSuccess y -> Just (unwrap y)
    _ -> Nothing
  getFailure x = case x of
    AuthTokenInitOutFailure y -> Just y
    _ -> Nothing

instance decodeJsonAuthTokenInitOut :: DecodeJson AuthTokenInitOut where
  decodeJson json = do
    o <- decodeJson json
    let f = AuthTokenInitOutFailure <$> o .? "failure"
        s = AuthTokenInitOutSuccess <$> o .? "success"
    f <|> s



data AuthTokenDeltaIn
  = AuthTokenDeltaInLogout

instance encodeJsonAuthTokenDeltaIn :: EncodeJson AuthTokenDeltaIn where
  encodeJson AuthTokenDeltaInLogout = encodeJson "logout"



data AuthTokenDeltaOut
  = AuthTokenDeltaOutRevoked

instance accessTokenDeltaOutAuthTokenDeltaOut :: AccessTokenDeltaOut AuthTokenDeltaOut where
  getRevoke x = case x of
    AuthTokenDeltaOutRevoked -> true

instance decodeJsonAuthTokenDeltaOut :: DecodeJson AuthTokenDeltaOut where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "revoke" -> pure AuthTokenDeltaOutRevoked
        | otherwise -> fail "AuthTokenDeltaOut"



type AuthTokenSparrowClientQueues eff =
  SparrowClientQueues eff AuthTokenInitIn AuthTokenInitOut AuthTokenDeltaIn AuthTokenDeltaOut
