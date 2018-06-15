module Test.Main where

import LocalCooking.Dependencies.AuthToken (PreliminaryAuthToken)
import LocalCooking.Dependencies.Common as Common
import LocalCooking.Dependencies.Mitch as Mitch
import LocalCooking.Dependencies.Validate as Validate
import LocalCooking.Dependencies.AccessToken.Generic as Access

import Prelude
import Data.Either (Either (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Type.Proxy (Proxy (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.QuickCheck (class Arbitrary, quickCheck, Result (..))



main :: Eff _ Unit
main = do
  log "JSON Iso:"
  jsonIsoAssert "LocalCooking.Dependencies.AuthToken.PreliminaryAuthToken" (Proxy :: Proxy PreliminaryAuthToken)
  log "-------"
  jsonIsoAssert "LocalCooking.Dependencies.AccessToken.Generic.AccessInitIn" (Proxy :: Proxy (Access.AccessInitIn Unit Unit))
  jsonIsoAssert "LocalCooking.Dependencies.AccessToken.Generic.AccessInitOut" (Proxy :: Proxy (Access.AccessInitOut Unit))
  log "-------"
  jsonIsoAssert "LocalCooking.Dependencies.Common.UserInitIn" (Proxy :: Proxy Common.UserInitIn)
  jsonIsoAssert "LocalCooking.Dependencies.Common.UserInitOut" (Proxy :: Proxy Common.UserInitOut)
  jsonIsoAssert "LocalCooking.Dependencies.Common.UserDeltaIn" (Proxy :: Proxy Common.UserDeltaIn)
  jsonIsoAssert "LocalCooking.Dependencies.Common.UserDeltaOut" (Proxy :: Proxy Common.UserDeltaOut)
  log "-------"
  jsonIsoAssert "LocalCooking.Dependencies.Mitch.SubmitReview" (Proxy :: Proxy Mitch.SubmitReview)
  jsonIsoAssert "LocalCooking.Dependencies.Mitch.BrowseMenu" (Proxy :: Proxy Mitch.BrowseMenu)
  jsonIsoAssert "LocalCooking.Dependencies.Mitch.BrowseMeal" (Proxy :: Proxy Mitch.BrowseMeal)
  jsonIsoAssert "LocalCooking.Dependencies.Mitch.AddToCart" (Proxy :: Proxy Mitch.AddToCart)
  log "-------"
  jsonIsoAssert "LocalCooking.Dependencies.Validate.IsUniqueMenuDeadline" (Proxy :: Proxy Validate.IsUniqueMenuDeadline)
  jsonIsoAssert "LocalCooking.Dependencies.Validate.IsUniqueMealPermalink" (Proxy :: Proxy Validate.IsUniqueMealPermalink)
  jsonIsoAssert "LocalCooking.Dependencies.Validate.PasswordVerifyUnauth" (Proxy :: Proxy Validate.PasswordVerifyUnauth)


jsonIsoAssert :: forall a
               . EncodeJson a
              => DecodeJson a
              => Eq a
              => Show a
              => Arbitrary a
              => String -> Proxy a -> Eff _ Unit
jsonIsoAssert name Proxy = do
  log ("    " <> name)
  quickCheck (\(x :: a) -> jsonIso x)
  log ""


jsonIso :: forall a. EncodeJson a => DecodeJson a => Eq a => Show a => a -> Result
jsonIso x = case decodeJson (encodeJson x) of
  Left y -> Failed $ "decoding failure: " <> y <> ", " <> show y
  Right y
    | x == y -> Success
    | otherwise -> Failed $ "Not identical: " <> show x <> ", " <> show y
