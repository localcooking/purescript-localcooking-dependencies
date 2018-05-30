module LocalCooking.Dependencies.AuthToken where

import LocalCooking.Dependencies.AccessToken.Generic
  (class AccessTokenInitIn, class AccessTokenInitOut, class AccessTokenDeltaOut)
import LocalCooking.Semantics.Common (Login, SocialLogin)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import Facebook.Types (FacebookUserId, FacebookLoginReturnError)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Newtype (unwrap, wrap)



data AuthTokenInitIn
  = AuthTokenInitInLogin Login
  | AuthTokenInitInSocialLogin SocialLogin
  | AuthTokenInitInExists AuthToken

instance accessTokenInitInAuthTokenInitIn :: AccessTokenInitIn AuthTokenInitIn where
  makeExists = AuthTokenInitInExists <<< wrap


data AuthTokenFailure
  = FBLoginReturnBad String String
  | FBLoginReturnDenied String
  | FBLoginReturnBadParse
  | FBLoginReturnNoUser FacebookUserId
  | FBLoginReturnError FacebookLoginReturnError
  | AuthTokenLoginFailure


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


data AuthTokenDeltaIn
  = AuthTokenDeltaInLogout


data AuthTokenDeltaOut
  = AuthTokenDeltaOutRevoked

instance accessTokenDeltaOutAuthTokenDeltaOut :: AccessTokenDeltaOut AuthTokenDeltaOut where
  getRevoke x = case x of
    AuthTokenDeltaOutRevoked -> true
    _ -> false
