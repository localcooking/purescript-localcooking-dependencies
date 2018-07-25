module LocalCooking.Dependencies.AccessToken.Generic where

import LocalCooking.Common.AccessToken (AccessToken)

import Data.Maybe (Maybe)



class AccessTokenInitIn initIn where
  makeExists :: AccessToken -> initIn

class AccessTokenInitOut initOut err | initOut -> err where
  getSuccess :: initOut -> Maybe AccessToken
  getFailure :: initOut -> Maybe err

class AccessTokenDeltaOut deltaOut where
  getRevoke :: deltaOut -> Boolean


