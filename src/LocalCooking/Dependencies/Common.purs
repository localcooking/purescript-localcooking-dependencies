module LocalCooking.Dependencies.Common where

import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn)
import LocalCooking.Semantics.Common (Register, RegisterError, User, SetUser)
import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Sparrow.Client (unpackClient)
import Sparrow.Client.Types (SparrowClientT)
import Sparrow.Client.Queue (SparrowClientQueues, SparrowStaticClientQueues, sparrowStaticClientQueues, sparrowClientQueues, newSparrowStaticClientQueues, newSparrowClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.NonEmpty (NonEmpty (..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?), fail)
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Functor.Singleton (class SingletonFunctor)
import Control.Alternative ((<|>))
import Control.Monad.Trans.Control (class MonadBaseControl)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as QC



type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , console :: CONSOLE
  | eff)


type CommonQueues eff =
  { registerQueues :: RegisterSparrowClientQueues eff
  , userQueues :: UserSparrowClientQueues eff
  }


newCommonQueues :: forall eff. Eff (Effects eff) (CommonQueues (Effects eff))
newCommonQueues = do
  registerQueues <- newSparrowStaticClientQueues
  userQueues <- newSparrowClientQueues
  pure
    { registerQueues
    , userQueues
    }


commonDependencies :: forall eff stM m
                      . MonadBaseControl (Eff (Effects eff)) m stM
                     => MonadEff (Effects eff) m
                     => SingletonFunctor stM
                     => CommonQueues (Effects eff)
                     -> SparrowClientT (Effects eff) m Unit
commonDependencies
  { registerQueues
  , userQueues
  } = do
  unpackClient (Topic ["common","register"]) (sparrowStaticClientQueues registerQueues)
  unpackClient (Topic ["common","user"]) (sparrowClientQueues userQueues)



newtype UserInitIn = UserInitIn (AccessInitIn AuthToken JSONUnit)

derive instance genericUserInitIn :: Generic UserInitIn
derive newtype instance eqUserInitIn :: Eq UserInitIn
derive newtype instance showUserInitIn :: Show UserInitIn
derive newtype instance arbitraryInitIn :: Arbitrary UserInitIn
derive newtype instance encodeJsonUserInitIn :: EncodeJson UserInitIn
derive newtype instance decodeJsonUserInitIn :: DecodeJson UserInitIn

newtype UserInitOut = UserInitOut User

derive instance genericUserInitOut :: Generic UserInitOut
derive newtype instance eqUserInitOut :: Eq UserInitOut
derive newtype instance showUserInitOut :: Show UserInitOut
derive newtype instance arbitraryInitOut :: Arbitrary UserInitOut
derive newtype instance encodeJsonUserInitOut :: EncodeJson UserInitOut
derive newtype instance decodeJsonUserInitOut :: DecodeJson UserInitOut

data UserDeltaIn
  = UserDeltaInSetUser SetUser

derive instance genericUserDeltaIn :: Generic UserDeltaIn

instance eqUserDeltaIn :: Eq UserDeltaIn where
  eq = gEq
instance showUserDeltaIn :: Show UserDeltaIn where
  show = gShow
instance arbitraryUserDeltaIn :: Arbitrary UserDeltaIn where
  arbitrary = QC.oneOf $ NonEmpty
    (UserDeltaInSetUser <$> arbitrary)
    []
instance encodeJsonUserDeltaIn :: EncodeJson UserDeltaIn where
  encodeJson x = case x of
    UserDeltaInSetUser y -> "setUser" := y ~> jsonEmptyObject
instance decodeJsonUserDeltaIn :: DecodeJson UserDeltaIn where
  decodeJson json = do
    o <- decodeJson json
    let setUser = UserDeltaInSetUser <$> o .? "setUser"
    setUser

data UserDeltaOut
  = UserDeltaOutUser User
  | UserDeltaOutSetUserFailure
  | UserDeltaOutSetUserSuccess

derive instance genericUserDeltaOut :: Generic UserDeltaOut

instance eqUserDeltaOut :: Eq UserDeltaOut where
  eq = gEq
instance showUserDeltaOut :: Show UserDeltaOut where
  show = gShow
instance arbitraryUserDeltaOut :: Arbitrary UserDeltaOut where
  arbitrary = QC.oneOf $ NonEmpty
    (UserDeltaOutUser <$> arbitrary)
    [ pure UserDeltaOutSetUserFailure
    , pure UserDeltaOutSetUserSuccess
    ]
instance encodeJsonUserDeltaOut :: EncodeJson UserDeltaOut where
  encodeJson x = case x of
    UserDeltaOutUser y -> "user" := y ~> jsonEmptyObject
    UserDeltaOutSetUserFailure -> encodeJson "setUserFailure"
    UserDeltaOutSetUserSuccess -> encodeJson "setUserSuccess"
instance decodeJsonUserDeltaOut :: DecodeJson UserDeltaOut where
  decodeJson json = do
    let obj = do
          o <- decodeJson json
          let user = UserDeltaOutUser <$> o .? "user"
          user
        str = do
          s <- decodeJson json
          case unit of
            _ | s == "setUserFailure" -> pure UserDeltaOutSetUserFailure
              | s == "setUserSuccess" -> pure UserDeltaOutSetUserSuccess
              | otherwise -> fail "Not a UserDeltaOut"
    obj <|> str



type RegisterSparrowClientQueues eff =
  SparrowStaticClientQueues eff Register RegisterError

type UserSparrowClientQueues eff =
  SparrowClientQueues eff UserInitIn UserInitOut UserDeltaIn UserDeltaOut
