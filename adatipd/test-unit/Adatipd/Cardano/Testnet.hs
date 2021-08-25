{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Cardano.Testnet
  ( Testnet (..)
  , withTestnet
  ) where

import Adatipd.Cardano (Lovelace (..))
import Control.Exception (bracket)
import Data.Aeson ((.=))
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Set (Set)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word16)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (callProcess)

import qualified Data.Aeson as Ae (Value, encode, encodeFile, object)
import qualified Data.ByteString.Lazy as LBS (hPutStr)
import qualified Data.Set as Set (delete, fromList, toList)

--------------------------------------------------------------------------------
-- Testnet

-- |
-- Information about a testnet created by 'withTestnet'.
data Testnet =
  Testnet
    { tDirectory :: FilePath }

-- |
-- Start up a new Cardano testnet.
-- Run the given action and return its result.
-- Then shut down the testnet and delete it.
--
-- The testnet is advanced to the Alonzo era
-- before the given action is run.
withTestnet :: (Testnet -> IO a) -> IO a
withTestnet action =
  withSystemTempDirectory "adatipd-cardano-testnet" $
    \directory ->
      bracket
        (setupTestnet directory)
        teardownTestnet
        action

setupTestnet :: FilePath -> IO Testnet
setupTestnet directory = do
  let nodes = Set.fromList [3000, 3001, 3002, 3003]
  writeTopologies directory nodes
  writeByronGenesis directory
  pure Testnet { tDirectory = directory }

teardownTestnet :: Testnet -> IO ()
teardownTestnet Testnet {..} =
  pure ()

--------------------------------------------------------------------------------
-- Topology files

-- A topology file tells a node how to connect to other nodes (peers).
-- Essentially it just lists the addresses and port numbers of peers.
-- It also contains a ‘valency’ field for each peer,
-- which indicates how many nodes run on the address.
-- This only makes sense for DNS addresses that can resolve to multiple hosts.
-- We always connect to 127.0.0.1, which only resolves to one host,
-- so we always set the valency to 1.

-- |
-- Write the topology file for each node,
-- given the port number of each node.
writeTopologies :: FilePath -> Set Word16 -> IO ()
writeTopologies directory nodes =
  for_ (nodeTopologies nodes) $
    \(node, topology) -> do
      let nodeDirectory = directory </> "node-" <> show node
      let topologyPath = nodeDirectory </> "topology.json"
      createDirectoryIfMissing False nodeDirectory
      Ae.encodeFile @Ae.Value topologyPath topology

-- |
-- Generate the topology file for each node,
-- given the port number of each node.
nodeTopologies :: Set Word16 -> [(Word16, Ae.Value)]
nodeTopologies nodes =
  fmap
    (fmap (nodeTopology . Set.toList))
    (nodesPeers nodes)

-- |
-- Generate the topology file for a node,
-- given the port numbers of the other nodes.
nodeTopology :: [Word16] -> Ae.Value
nodeTopology peers =
  Ae.object
    [ "Producers" .=
        [ Ae.object
            [ "addr"    .= id @String "127.0.0.1"
            , "port"    .= peer
            , "valency" .= id @Int 1 ]
        | peer <- peers ] ]

-- |
-- For each node, return the set of other nodes.
nodesPeers :: Ord a => Set a -> [(a, Set a)]
nodesPeers nodes =
  [ (node, Set.delete node nodes)
  | node <- Set.toList nodes ]

--------------------------------------------------------------------------------
-- Byron genesis files

-- |
-- Write the Byron genesis files.
writeByronGenesis :: FilePath -> IO ()
writeByronGenesis directory =

  -- Write the Byron protocol parameters to a file.
  -- This file will be read by generateByronGenesis.
  -- We do not need the file anymore afterwards (I hope).
  withSystemTempFile "" $
    \byronGenesisSpecPath byronGenesisSpecFile -> do
      LBS.hPutStr byronGenesisSpecFile $
        Ae.encode @Ae.Value byronGenesisSpec
      hClose byronGenesisSpecFile

      -- Write the Byron genesis files.
      -- Directory will be created by generateByronGenesis,
      -- and must not already exist otherwise it crashes.
      let byronDirectory = directory </> "byron"
      generateByronGenesis
        byronDirectory
        byronGenesisSpecPath
        5
        5
        (Lovelace 1_000_000_000_000)
        0.5

-- |
-- Byron protocol parameters.
byronGenesisSpec :: Ae.Value
byronGenesisSpec =
  Ae.object
    [ -- Heavyweight delegation threshold.
      -- TODO: Update comment to explain heavyweight delegation.
      "heavyDelThd" .= id @String "300000000000"

      -- Maximum number of bytes in a block/header/transaction.
    , "maxBlockSize"  .= id @String "2000000"
    , "maxHeaderSize" .= id @String "2000000"
    , "maxTxSize"     .= id @String "4096"

      -- Maximum number of bytes in an update proposal.
      -- Update proposals are about upgrading the protocol.
    , "maxProposalSize" .= id @String "700"

      -- Plutus smart contract language version,
      -- but there are no smart contracts in the Byron era,
      -- so while this parameter is mandatory it is ignored.
    , "scriptVersion" .= id @Int 0

      -- How many milliseconds a slot takes.
    , "slotDuration" .= id @String "1000"

      -- The documentation is unclear about this.
      -- It has something to do with OBFT.
    , "unlockStakeEpoch"  .= id @String "18446744073709551615"

      -- No longer used after OBFT era.
    , "mpcThd" .= id @String "20000000000000"

      -- The number of slots a proposal has to gather a majority of votes.
      -- If a majority of votes has not been reached before this period,
      -- then the proposal is rejected.
    , "updateImplicit" .= id @String "10000"

      -- Stake threshold for creating/voting a proposal.
    , "updateProposalThd" .= id @String "100000000000000"
    , "updateVoteThd"     .= id @String "1000000000000"

      -- Stake threshold for participating in softfork resolution.
    , "softforkRule" .=
        Ae.object
          [ "initThd"      .= id @String "900000000000000"
          , "minThd"       .= id @String "600000000000000"
          , "thdDecrement" .= id @String "50000000000000" ]

      -- Transaction fee = summand + multiplier × bytes.
    , "txFeePolicy" .=
        Ae.object
          [ "multiplier" .= id @String "43946000000"
          , "summand"    .= id @String "155381000000000" ] ]

-- |
-- Generate the Byron genesis files.
generateByronGenesis
  :: FilePath
  -> FilePath
  -> Int
  -> Int
  -> Lovelace
  -> Double
  -> IO ()
generateByronGenesis
  byronDirectory
  byronGenesisSpecPath
  delegateAddresses
  poorAddresses
  (Lovelace totalBalance)
  delegateShare = do

  -- cardano-cli help says that the start time must be given in ‘POSIXSECONDS’
  -- so we use getPOSIXTime rather than getCurrentTime.
  startTime <- getPOSIXTime
  let startTimeSeconds =
        round @_ @Int64 $
          nominalDiffTimeToSeconds startTime

  -- The cardano-cli byron genesis genesis command
  -- will generate the necessary configuration files.
  callProcess
    "cardano-cli"
    [ "byron", "genesis", "genesis"

      -- Input and output file paths for this command.
    , "--protocol-parameters-file", byronGenesisSpecPath
    , "--genesis-output-dir", byronDirectory

      -- The time at which the first slot occurs.
      -- This should be close to the current time,
      -- because current slot is determined by current time.
    , "--start-time", show startTimeSeconds

      -- Identifies the Cardano network.
    , "--protocol-magic", show protocolMagic

      -- The security parameter of the Ouroboros protocol.
      -- This is the maximum number of blocks that can be rolled back.
      -- The example uses 10, so we shall use 10.
    , "--k", "10"

      -- How many addresses to generate.
      -- Delegate addresses get delegateShare of totalBalance.
      -- Poor addresses get the remaining share of totalBalance.
      -- The balance is distributed equally across members of each group.
    , "--n-delegate-addresses", show delegateAddresses
    , "--n-poor-addresses", show poorAddresses
    , "--total-balance", show totalBalance
    , "--delegate-share", show delegateShare

      -- AVVM stands for Ada Voucher Vending Machine.
      -- It contains Ada presale balances.
      -- We do not care, so they are zero.
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0" ]

--------------------------------------------------------------------------------
-- Protocol magic

-- |
-- Identifies the Cardano network.
protocolMagic :: Int
protocolMagic = 0xDEADBEEF
