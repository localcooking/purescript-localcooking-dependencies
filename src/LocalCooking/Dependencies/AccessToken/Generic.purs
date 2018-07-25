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


