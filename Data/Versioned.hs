{-|
Module      : Data.Versioned
Description : A class for versioned data.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Data.Versioned (

    Versioned(..)
  , migrateToLatest
  , getLatest

  ) where

import Data.Bijection
import Data.TypeNat.Nat
import Data.Migration

-- | A class describing datatypes which are versioned:
class Versioned d where
  -- | The latest version of this datatype.
  type LatestVersion d :: Nat
  -- | The history of versions of this datatype.
  data VersionHistory d :: Nat -> *
  -- | A bijection between the datatype and the version history indexed at
  --   the latest version.
  bijectionLatestVersion :: d :<->: ((VersionHistory d) (LatestVersion d))
  -- | A path of single-step migrations from Zero to the latest version.
  migrationPath :: MigrationPath (VersionHistory d) (LatestVersion d)

-- | Brings a @VersionHistory d n@ for some @d@ to the latest version, as
--   determined by the @Versioned d@ instance.
migrateToLatest
  :: forall d n m .
     ( Versioned d
     , LTE n (LatestVersion d)
     )
  => (VersionHistory d) n
  -> (VersionHistory d) (LatestVersion d)
migrateToLatest = migrate migrationPath

-- | Produce a @d@ from any @(VersionHistory d) n@, by migrating to the latest
--   version and using @bijectionLatestVersion@.
getLatest
  :: forall d n m .
     ( Versioned d
     , LTE n (LatestVersion d)
     )
  => (VersionHistory d) n
  -> d
getLatest = (biFrom bijectionLatestVersion) . migrateToLatest
