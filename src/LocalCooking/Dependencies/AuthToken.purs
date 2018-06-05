module LocalCooking.Dependencies.AuthToken where

import LocalCooking.Dependencies.AccessToken.Generic
  (class AccessTokenInitIn, class AccessTokenInitOut, class AccessTokenDeltaOut)
import LocalCooking.Semantics.Common (Login, SocialLogin)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Global.Error (AuthTokenFailure (..))
import Facebook.Types (FacebookUserId, FacebookLoginReturnError)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue (SparrowClientQueues, sparrowClientQueues, newSparrowClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Newtype (unwrap, wrap)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson, fail, encodeJson)
import Data.Functor.Singleton (class SingletonFunctor)
import Data.Generic (class Generic, gShow)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Alternative ((<|>))


type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
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


newtype PreliminaryAuthToken = PreliminaryAuthToken
  (Maybe (Either AuthTokenFailure AuthToken))

instance decodeJsonPreliminaryAuthToken :: DecodeJson PreliminaryAuthToken where
  decodeJson json = do
    mO <- decodeJson json
    case mO of
      Nothing -> pure (PreliminaryAuthToken Nothing)
      Just o -> do
        let err = PreliminaryAuthToken <<< Just <<< Left <$> o .? "err"
            token = PreliminaryAuthToken <<< Just <<< Right <$> o .? "token"
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
    _ -> false

instance decodeJsonAuthTokenDeltaOut :: DecodeJson AuthTokenDeltaOut where
  decodeJson json = do
    s <- decodeJson json
    case unit of
      _ | s == "revoke" -> pure AuthTokenDeltaOutRevoked
        | otherwise -> fail "AuthTokenDeltaOut"



type AuthTokenSparrowClientQueues eff =
  SparrowClientQueues eff AuthTokenInitIn AuthTokenInitOut AuthTokenDeltaIn AuthTokenDeltaOut
