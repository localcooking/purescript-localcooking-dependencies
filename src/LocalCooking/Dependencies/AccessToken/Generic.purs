module LocalCooking.Dependencies.AccessToken.Generic where

import LocalCooking.Common.AccessToken (AccessToken)

import Prelude
import Data.Maybe (Maybe)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson, fail)
import Control.Alternative ((<|>))



class AccessTokenInitIn initIn where
  makeExists :: AccessToken -> initIn

class AccessTokenInitOut initOut err | initOut -> err where
  getSuccess :: initOut -> Maybe AccessToken
  getFailure :: initOut -> Maybe err

class AccessTokenDeltaOut deltaOut where
  getRevoke :: deltaOut -> Boolean





newtype AccessInitIn k a = AccessInitIn
  { token :: k
  , subj  :: a
  }

instance encodeJsonAccessInitIn :: (EncodeJson k, EncodeJson a) => EncodeJson (AccessInitIn k a) where
  encodeJson (AccessInitIn {token,subj})
    =  "token" := token
    ~> "subj" := subj
    ~> jsonEmptyObject


data AccessInitOut a
  = AccessInitOutNoAccess
  | AccessInitOut
    { subj :: a
    }


instance decodeJsonAccessInitOut :: DecodeJson a => DecodeJson (AccessInitOut a) where
  decodeJson json = do
    let str = do
          s <- decodeJson json
          if s == "no-access"
            then pure AccessInitOutNoAccess
            else fail "Not a AccessInitOut"
        obj = do
          o <- decodeJson json
          subj <- o .? "subj"
          pure (AccessInitOut {subj})
    str <|> obj
