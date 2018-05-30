module LocalCooking.Dependencies.AuthToken where

import LocalCooking.Dependencies.AccessToken.Generic
  (class AccessTokenInitIn, class AccessTokenInitOut, class AccessTokenDeltaOut)
import LocalCooking.Semantics.Common (Login, SocialLogin)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import Facebook.Types (FacebookUserId, FacebookLoginReturnError)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Newtype (unwrap, wrap)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson, fail)
import Control.Alternative ((<|>))



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


data AuthTokenFailure
  = FBLoginReturnBad String String
  | FBLoginReturnDenied String
  | FBLoginReturnBadParse
  | FBLoginReturnNoUser FacebookUserId
  | FBLoginReturnError FacebookLoginReturnError
  | AuthTokenLoginFailure

instance decodeJsonAuthTokenFailure :: DecodeJson AuthTokenFailure where
  decodeJson json = do
    let obj = do
          o <- decodeJson json
          let fbBad = do
                o' <- o .? "fbBad"
                code <- o' .? "code"
                msg <- o .? "msg"
                pure $ FBLoginReturnBad code msg
              fbDenied = do
                o' <- o .? "fbDenied"
                desc <- o' .? "desc"
                pure $ FBLoginReturnDenied desc
              fbNoUser = do
                x <- o .? "no-user"
                pure $ FBLoginReturnNoUser x
              fbReturn = do
                x <- o .? "fbLoginReturnError"
                pure $ FBLoginReturnError x
          fbBad <|> fbDenied <|> fbNoUser <|> fbReturn
        str = do
          s <- decodeJson json
          case unit of
            _ | s == "bad-parse" -> pure FBLoginReturnBadParse
              | s == "loginFailure" -> pure AuthTokenLoginFailure
              | otherwise -> fail "Not a AuthTokenFailure"
    obj <|> str


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
