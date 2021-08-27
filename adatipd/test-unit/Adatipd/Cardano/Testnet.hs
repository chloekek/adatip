{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Cardano.Testnet
  ( Testnet (..)
  , withTestnet
  ) where

import Adatipd.Cardano (Lovelace (..))
import Data.Aeson ((.=))
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Int (Int32, Int64)
import Data.Set (Set)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word16)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (CreateProcess, callProcess, withCreateProcess)

import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as LBS (hPutStr)
import qualified Data.HashMap.Strict as HM (insert)
import qualified Data.Set as Set (delete, fromList, toList)
import qualified System.Process as P (CreateProcess (..), proc)

--------------------------------------------------------------------------------
-- Testnet

-- |
-- Information about a testnet created by 'withTestnet'.
data Testnet =
  Testnet
    { tDirectory :: FilePath
    , tProtocolMagic :: Int32 }

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
    \directory -> do
      let nodes = [3000, 3001, 3002, 3003]
      setupTestnet directory nodes
      withCardanoNodes directory nodes $
        action
          Testnet
            { tDirectory = directory
            , tProtocolMagic = protocolMagic }

setupTestnet :: FilePath -> [Word16] -> IO ()
setupTestnet directory nodes = do
  writeTopologies directory (Set.fromList nodes)
  writeByronGenesis directory
  writeShelleyGenesis directory
  writeConfiguration directory

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
-- Shelley genesis files

-- |
-- Write the Shelley genesis files.
writeShelleyGenesis :: FilePath -> IO ()
writeShelleyGenesis directory = do

  -- Writing the Shelley genesis files happens in two phases.
  -- First we let cardano-cli generate a default genesis spec (phase 1).
  -- Then we patch the genesis spec by reading and writing it (phase 2).
  -- Then we let cardano-cli regenerate the genesis files (phase 3).

  let shelleyDirectory = directory </> "shelley"
  generateShellyGenesisPhase1 shelleyDirectory
  generateShellyGenesisPhase2 shelleyDirectory
  generateShellyGenesisPhase3 shelleyDirectory

generateShellyGenesisPhase1 :: FilePath -> IO ()
generateShellyGenesisPhase1 shelleyDirectory =
  callProcess
    "cardano-cli"
    [ "genesis", "create"
    , "--genesis-dir", shelleyDirectory
    , "--testnet-magic", show protocolMagic ]

generateShellyGenesisPhase2 :: FilePath -> IO ()
generateShellyGenesisPhase2 shelleyDirectory = do
  let shelleyGenesisSpecPath = shelleyDirectory </> "genesis.spec.json"
  Just spec <- Ae.decodeFileStrict @Ae.Value shelleyGenesisSpecPath
  let spec' = patchShellyGenesisSpec spec
  Ae.encodeFile @Ae.Value shelleyGenesisSpecPath spec'

patchShellyGenesisSpec :: Ae.Value -> Ae.Value
patchShellyGenesisSpec (Ae.Object original) =
  Ae.Object $
    original

    -- Total amount of Lovelace in the system.
    & HM.insert "maxLovelaceSupply" (Ae.Number 1_000_000_000_000)

    -- The security parameter of the Ouroboros protocol.
    -- This is the maximum number of blocks that can be rolled back.
    -- The example uses 10, so we shall use 10.
    & HM.insert "securityParam" (Ae.Number 10)

    -- Number of nodes that need to agree to initiate protocol update.
    -- Set this to a low number as we have very few nodes.
    & HM.insert "updateQuorum" (Ae.Number 2)

    -- In our case Shelley is protocol version 2,
    -- as we first go through Byron (0) and Byron OBFT (1).
    & HM.insert "protocolVersion" (
        Ae.object
          [ "major" .= id @Int 2
          , "minor" .= id @Int 0 ]
    )
patchShellyGenesisSpec _ =
  error "Shelley genesis spec should be a JSON object"

generateShellyGenesisPhase3 :: FilePath -> IO ()
generateShellyGenesisPhase3 shelleyDirectory =
  callProcess
    "cardano-cli"
    [ "genesis", "create"
    , "--genesis-dir", shelleyDirectory
    , "--testnet-magic", show protocolMagic
    , "--gen-genesis-keys", "0" {- TODO: Pass number of BFT nodes. -}
    , "--gen-utxo-keys", "0" {- TODO: Pass number of UTXO keys. -} ]

--------------------------------------------------------------------------------
-- Configuration file

-- |
-- Write the configuration.yaml file.
-- We write JSON, but YAML is a superset of JSON.
writeConfiguration :: FilePath -> IO ()
writeConfiguration directory = do
  let configurationPath = directory </> "configuration.yaml"
  Ae.encodeFile @Ae.Value configurationPath configuration

-- |
-- Configuration for cardano-node.
configuration :: Ae.Value
configuration =
  Ae.object
    [ -- The Cardano protocol contains all eras.
      -- This is what we want because we will
      -- hardfork through all of them.
      "Protocol" .= id @String "Cardano"

    , -- These files are written by the other parts of this module.
      "ByronGenesisFile"   .= id @String "byron/genesis.json"
    , "ShelleyGenesisFile" .= id @String "shelley/genesis.json"
    , "AlonzoGenesisFile"  .= id @String "shelley/genesis.alonzo.json"

      -- This allows us to do special testnet-specific things.
    , "RequiresNetworkMagic" .= id @String "RequiresMagic"

      -- This number gets used by block producing nodes
      -- as part of the system for agreeing on protocol updates.
    , "LastKnownBlockVersion-Major" .= id @Int 5
    , "LastKnownBlockVersion-Minor" .= id @Int 0
    , "LastKnownBlockVersion-Alt"   .= id @Int 0

      -- Configure nodes to update to latest protocol immediately.
    , "TestShelleyHardForkAtEpoch" .= id @Int 0
    , "TestAllegraHardForkAtEpoch" .= id @Int 0
    , "TestMaryHardForkAtEpoch"    .= id @Int 0
    , "TestAlonzoHardForkAtEpoch"  .= id @Int 0
    , "TestEnableDevelopmentHardForkEras"     .= True
    , "TestEnableDevelopmentNetworkProtocols" .= True

      -- Logging configuration.
      -- This isn’t very interesting.
    , "TurnOnLogging"    .= True
    , "TurnOnLogMetrics" .= False
    , "minSeverity"      .= id @String "Info"
    , "TracingVerbosity" .= id @String "NormalVerbosity"
    , "setupBackends"    .= id @[String] ["KatipBK"]
    , "defaultBackends"  .= id @[String] ["KatipBK"]
    , "setupScribes"     .=
        [ Ae.object
            [ "scKind"   .= id @String "StdoutSK"
            , "scName"   .= id @String "stdout"
            , "scFormat" .= id @String "ScText" ] ]
    , "defaultScribes"   .= id @[[String]] [["StdoutSK", "stdout"]]
    , "options"          .= Ae.object [ ] ]

--------------------------------------------------------------------------------
-- Running cardano-node

-- |
-- Run an action with multiple instances of cardano-node.
-- One instance of cardano-node is started for each given port number.
withCardanoNodes :: FilePath -> [Word16] -> IO a -> IO a
withCardanoNodes _ [] action = action
withCardanoNodes directory (node : nodes) action =
  withCardanoNode directory node $
    withCardanoNodes directory nodes action

-- |
-- Run an action with an instance of cardano-node.
withCardanoNode :: FilePath -> Word16 -> IO a -> IO a
withCardanoNode directory port action =
  let

    -- Directory in which the node files are stored.
    -- Relative to the directory argument.
    nodeDirectory :: FilePath
    nodeDirectory = "node-" <> show port

    -- Start cardano-node with appropriate arguments.
    -- Remaining configuration is read from the files
    -- that we write in the other parts of this module.
    createProcess :: CreateProcess
    createProcess =
      P.proc
        "cardano-node"
        [ "run"
        , "--port", show port
        , "--config", "configuration.yaml"
        , "--topology", nodeDirectory </> "topology.json"
        , "--database-path", nodeDirectory </> "db"
        , "--socket-path", nodeDirectory </> "socket" ]

    -- Set some extra options for the process.
    createProcess' :: CreateProcess
    createProcess' =
      createProcess
        { P.cwd = Just directory }
  in

    withCreateProcess createProcess' $
      \_stdin _stdout _stderr _pid ->
        action

--------------------------------------------------------------------------------
-- Protocol magic

-- |
-- Identifies the Cardano network.
protocolMagic :: Int32
protocolMagic = 0xBEEF
