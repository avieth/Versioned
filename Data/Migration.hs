{-|
Module      : Data.Migration
Description : Description of data migrations.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Migration (

    Migration(..)
  , MigrationPath(..)
  , migrate

  ) where

import Data.TypeNat.Nat
import Data.Proxy

-- | A Migration of a Nat-indexed datatype from one index to its successor.
data Migration :: (Nat -> *) -> Nat -> * where
  Migration :: (d n -> d (S n)) -> Migration d (S n)

-- | Carry out a Migration.
migrateOnce :: Migration d (S n) -> d n -> d (S n)
migrateOnce m x = case m of
    Migration f -> f x

-- | A path of migrations from Z to n on some Nat-indexed datatype.
data MigrationPath :: (Nat -> *) -> Nat -> * where
  TrivialMigrationPath :: MigrationPath d Z
  ConsMigrationPath :: Migration d (S n) -> MigrationPath d n -> MigrationPath d (S n)

-- | Carry out a migration from n to m on some Nat-indexed datatype, where
--   m need not be the successor of n
migrate :: forall d n m . (StrongLTE m m, LTE n m) => MigrationPath d m -> d n -> d m
migrate path = lteInduction proxy step
  where
    step :: forall k . LTE (S k) m => d k -> d (S k)
    step x = migrateOnce (getMigration path) x
    proxy :: Proxy m
    proxy = Proxy

-- | Extract the Migration from the top of a MigrationPath.
migration :: MigrationPath d (S n) -> Migration d (S n)
migration mg = case mg of
    ConsMigrationPath m _ -> m

-- | Extract a sub-MigrationPath for a particular Nat.
shrinkMigrationPath :: LTE (S k) m => MigrationPath d m -> MigrationPath d (S k)
shrinkMigrationPath = lteRecursion step
  where
    step :: forall d l . MigrationPath d (S l) -> MigrationPath d l
    step mp = case mp of
        ConsMigrationPath _ x -> x

-- | Get the Migration for a given sub-MigrationPath
getMigration :: LTE (S k) m => MigrationPath d m -> Migration d (S k)
getMigration = migration . shrinkMigrationPath
