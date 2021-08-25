{-# LANGUAGE OverloadedStrings #-}

module Adatipd.Cardano.Testnet
  ( Testnet (..)
  , withTestnet
  ) where

import Control.Exception (bracket)
import Data.Aeson ((.=))
import Data.Foldable (for_)
import Data.Set (Set)
import Data.Word (Word16)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))
import System.IO.Temp (withSystemTempDirectory)

import qualified Data.Aeson as Ae (Value, encodeFile, object)
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
