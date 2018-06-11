module LocalCooking.Dependencies.AccessToken.Generic where

import LocalCooking.Common.AccessToken (AccessToken)

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Maybe (Maybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, (:=), (.?), (~>), jsonEmptyObject, decodeJson, fail)
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as QC



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

derive instance genericAccessInitIn :: (Generic k, Generic a) => Generic (AccessInitIn k a)

instance eqAccessInitIn :: (Generic k, Generic a) => Eq (AccessInitIn k a) where
  eq = gEq

instance showAccessInitIn :: (Generic k, Generic a) => Show (AccessInitIn k a) where
  show = gShow

instance arbitraryAccessInitIn :: (Arbitrary k, Arbitrary a) => Arbitrary (AccessInitIn k a) where
  arbitrary = do
    token <- arbitrary
    subj <- arbitrary
    pure (AccessInitIn {token,subj})

instance encodeJsonAccessInitIn :: (EncodeJson k, EncodeJson a) => EncodeJson (AccessInitIn k a) where
  encodeJson (AccessInitIn {token,subj})
    =  "token" := token
    ~> "subj" := subj
    ~> jsonEmptyObject

instance decodeJsonAccessInitIN :: (DecodeJson k, DecodeJson a) => DecodeJson (AccessInitIn k a) where
  decodeJson json = do
    o <- decodeJson json
    token <- o .? "token"
    subj <- o .? "subj"
    pure (AccessInitIn {token,subj})


data AccessInitOut a
  = AccessInitOutNoAccess
  | AccessInitOut
    { subj :: a
    }

derive instance genericAccessInitOut :: (Generic a) => Generic (AccessInitOut a)

instance eqAccessInitOut :: (Generic a) => Eq (AccessInitOut a) where
  eq = gEq

instance showAccessInitOut :: (Generic a) => Show (AccessInitOut a) where
  show = gShow

instance arbitraryAccessInitOut :: Arbitrary a => Arbitrary (AccessInitOut a) where
  arbitrary = QC.oneOf $ NonEmpty
    ( pure AccessInitOutNoAccess )
    [ do subj <- arbitrary
         pure (AccessInitOut {subj})
    ]

instance encodeJsonAccessInitOut :: EncodeJson a => EncodeJson (AccessInitOut a) where
  encodeJson x = case x of
    AccessInitOutNoAccess -> encodeJson "no-access"
    AccessInitOut {subj} -> "subj" := subj ~> jsonEmptyObject

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
