module Adatipd.SubscriberSpec
  ( spec
  ) where

import Adatipd.Cardano.Testnet (Testnet (..), withTestnet)
import System.Process (callProcess)
import Test.Hspec (Spec, it)

spec :: Spec
spec =

  it "withTestnet scratchpad" $
    withTestnet $
      \Testnet {..} ->
        callProcess "tree" [ tDirectory ]
