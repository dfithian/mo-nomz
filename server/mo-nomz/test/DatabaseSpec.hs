module DatabaseSpec where

import NomzPrelude

import Test.Hspec (Spec, describe, it, shouldBe)

import Auth (BcryptedAuthorization(..))
import TestEnv (Env(..), runEnv)

import Database

spec :: Env -> Spec
spec env = describe "Database" $ do
  it "health" $ runEnv env health

  it "insertToken then fetchToken" $ do
    let token = BcryptedAuthorization "foobar"
    actual <- runEnv env $ \c -> do
      userId <- insertToken c token
      fetchToken c userId
    actual `shouldBe` Just token
