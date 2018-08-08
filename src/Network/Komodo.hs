{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo where

import qualified Data.ByteString.Char8 as C8 (concat, pack)

import           Network.Haskoin.Block
import           Network.Haskoin.Constants


initKomodo :: IO ()
initKomodo = setNetwork komodo

komodo :: Network
komodo = Network
    { getNetworkName = "komodo"
    , getAddrPrefix = 60
    , getScriptPrefix = 85
    , getSecretPrefix = 188
    , getExtPubKeyPrefix = 0x0488b21e
    , getExtSecretPrefix = 0x0488ade4

    -- FROM HERE DOWN IS UNVERIFIED

    , getNetworkMagic = 0xf9beb4d9
    , getGenesisHeader =
        BlockHeader
            0x01
            "0000000000000000000000000000000000000000000000000000000000000000"
            "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
            1231006505
            0x1d00ffff
            2083236893
            -- Hash 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
    , getMaxBlockSize = 1000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent = "/hath:0.1.1"
    , getDefaultPort = 8333
    , getAllowMinDifficultyBlocks = False
    , getPowNoRetargetting = False
    , getPowLimit =
        0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
        ( 227931
        , "000000000000024b89b42a942fe0d9fea3bb44ab7bd1b19115dd6a759c0808b8" )
    , getBip65Height = 388381
    , getBip66Height = 363725
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints =
        [ ( 11111
          , "0000000069e244f73d78e8fd29ba2fd2ed618bd6fa2ee92559f542fdb26e7c1d" )
        , ( 33333
          , "000000002dd5588a74784eaa7ab0507a18ad16a236e7b1ce69f00d7ddfb5d0a6" )
        , ( 74000
          , "0000000000573993a3c9e41ce34471c079dcf5f52a0e824a81e7f953b8661a20" )
        , ( 105000
          , "00000000000291ce28027faea320c8d2b054b2e0fe44a773f3eefb151d6bdc97" )
        , ( 134444
          , "00000000000005b12ffd4cd315cd34ffd4a594f430ac814c91184a0d42d2b0fe" )
        , ( 168000
          , "000000000000099e61ea72015e79632f216fe6cb33d7899acb35b75c8303b763" )
        , ( 193000
          , "000000000000059f452a5f7340de6682a977387c17010ff6e6c3bd83ca8b1317" )
        , ( 210000
          , "000000000000048b95347e83192f69cf0366076336c639f9b7228e9ba171342e" )
        , ( 216116
          , "00000000000001b4f4b433e81ee46494af945cf96014816a4e2370f11b23df4e" )
        , ( 225430
          , "00000000000001c108384350f74090433e7fcf79a606b8e797f065b130575932" )
        , ( 250000
          , "000000000000003887df1f29024b06fc2200b55f8af8f35453d7be294df2d214" )
        , ( 279000
          , "0000000000000001ae8c72a0b0c301f67e3afca10e819efa9041e458e9bd7e40" )
        ]
    , getSeeds =
        [ "seed.mainnet.b-pay.net"        -- BitPay
        , "seed.ob1.io"                   -- OB1
        , "seed.blockchain.info"          -- Blockchain
        , "bitcoin.bloqseeds.net"         -- Bloq
        , "seed.bitcoin.sipa.be"          -- Pieter Wuille
        , "dnsseed.bluematt.me"           -- Matt Corallo
        , "dnsseed.bitcoin.dashjr.org"    -- Luke Dashjr
        , "seed.bitcoinstats.com"         -- Chris Decker
        , "seed.bitcoin.jonasschnelli.ch" -- Jonas Schnelli
        ]
    , getBip44Coin = 0
    }
