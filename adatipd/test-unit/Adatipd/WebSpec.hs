-- SPDX-License-Identifier: AGPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Adatipd.WebSpec
  ( spec
  ) where

import Adatipd.E2ETest (runE2E, sendRequest)
import Data.Foldable (for_)
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Adatipd.WaiUtil as Wai
import qualified Data.Text as Text (unpack)
import qualified Network.HTTP.Types.Status as Status

spec :: Spec
spec =
  describe "handle" $
    describe "/~creator" creatorSpec

creatorSpec :: Spec
creatorSpec =
  for_ [[], ["tips"], ["tiers"]] $ \page ->
    describe (foldMap (("/" <>) . Text.unpack) page) $ do

      it "redirects if an old nickname is given" $ do
        response <- runE2E id $ sendRequest "GET" ("~ingriddevries" : page)
        Wai.responseStatus response `shouldBe` Status.movedPermanently301
        let location = lookup "Location" (Wai.responseHeaders response)
        let expectedLocation = foldMap ("/" <>) ("~henkdevries" : page)
        location `shouldBe` Just (encodeUtf8 expectedLocation)

      it "does not redirect if the latest nickname is given" $ do
        response <- runE2E id $ sendRequest "GET" ("~henkdevries" : page)
        Wai.responseStatus response `shouldBe` Status.ok200
        let location = lookup "Location" (Wai.responseHeaders response)
        location `shouldBe` Nothing

      it "returns not found if a non-existing nickname is given" $ do
        response <- runE2E id $ sendRequest "GET" ("~steve" : page)
        Wai.responseStatus response `shouldBe` Status.notFound404
