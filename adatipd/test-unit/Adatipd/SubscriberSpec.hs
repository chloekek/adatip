module Adatipd.SubscriberSpec
  ( spec
  ) where

import Adatipd.Cardano.Testnet (Testnet (..), withTestnet)
import Control.Concurrent (threadDelay)
import System.Process (callProcess)
import Test.Hspec (Spec, it)

spec :: Spec
spec =

  it "withTestnet scratchpad" $
    withTestnet $
      \Testnet {..} -> do
        callProcess "tree" [ tDirectory ]
        threadDelay 1_000_000
