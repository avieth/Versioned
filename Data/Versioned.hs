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

  ) where

import Data.Bijection
import Data.TypeNat.Nat
import Data.Migration

-- | A class describing datatypes which are versioned:
--   - they indicate a latest version
--   - they give a history of versions, indexed by version number
--   - they give a bijection between the datatype and the version history when
--     indexed at the latest version
--   - they give a MigrationPath
class Versioned d where
  type LatestVersion d :: Nat
  data VersionHistory d :: Nat -> *
  bijectionLatestVersion :: d :<->: ((VersionHistory d) (LatestVersion d))
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
