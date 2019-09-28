-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Poseidon.Postgis
-- Copyright   :  (c) 2019 Florian Grignon
-- License     :  BSD3
--
-- Maintainer  :  grignon.florian@gmail.com
-- Stability   :  experimental
--
-- This library provide a Simple and Extensible access to PostgreSQL.
--
-- Simple: Poseidon runs a SQL query and returns a set of custom datatype.
-- **It is not an ORM.**
--
-- Extensible: As a user of the library, you can map your custom PostgreSQL
-- type to your Haskell datatype easily, in a pluggable way (e.g. if you're
-- using postgis, you will be most likely interested by poseidon-postgis,
-- that maps GeoJSON WKT to GeospatialGeometry).
--
-----------------------------------------------------------------------------
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Poseidon.Postgis where

import GHC.Generics

import Data.Geospatial.Internal.Geometry

import Control.Monad
import Database.Poseidon.Internal
import Database.Poseidon

import Prelude (error)

import System.IO
import Text.Show
import Control.Applicative
import Data.Function
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid
import Data.Either
import Data.Maybe
import Data.Binary.Get
import Data.Wkb


newtype PGGeospatialGeometry = PGGeospatialGeometry GeospatialGeometry
  deriving (Generic, Show)

toGeospatialGeometry :: PGGeospatialGeometry -> GeospatialGeometry
toGeospatialGeometry pgGeospatialGeometry = case pgGeospatialGeometry of
                                              PGGeospatialGeometry value' -> value'

deserializeGeospatialGeometry :: BSL.ByteString -> PGGeospatialGeometry
deserializeGeospatialGeometry bs = do
  let eitherParsed = parseByteString bs
  case eitherParsed of
    Right location' -> PGGeospatialGeometry $ location'
    Left _ -> error "Impossible to decode location"

instance Deserialize PGGeospatialGeometry where
  deserialize res row col = do
    bs <- (fromMaybe mempty) <$> getBSValue res row col
    pure $ deserializeGeospatialGeometry bs

instance Deserialize (Maybe PGGeospatialGeometry) where
  deserialize res row col = do
    bs <- getBSValue res row col
    pure $ deserializeGeospatialGeometry <$> bs

-- Location
parseWkbByteString :: BSL.ByteString -> IO PGGeospatialGeometry
parseWkbByteString bs = do
  let eitherBS = parseByteString bs
  case eitherBS of
    Right wkb -> pure $ PGGeospatialGeometry wkb
    Left errorStr -> error errorStr

instance Deserialize [PGGeospatialGeometry] where
  deserialize res row col = do
    bs <- (fromMaybe mempty) <$> getBSValue res row col
    let pgArray = runGet getPGArray bs
    let words8  = fmap pgArrayDataData $ pgArrayData pgArray
    let bsList  = BS.pack <$> words8
    sequence $ (parseWkbByteString . BSL.fromStrict) <$> bsList
