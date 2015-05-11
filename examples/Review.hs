{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

import Data.Versioned
import Data.Migration
import Data.TypeNat.Nat
import Data.Bijection

-- Our 0th version.
--data Review = Good | Bad
--  deriving Show

-- Our first version.
--data Review = Good | Bad | Mediocre
--  deriving Show

-- Our second version.
--data Review = Good | Bad | Mediocre | Other String
--  deriving Show

data Review = Review String
  deriving Show

instance Versioned Review where

  -- Versions are identified by types of kind Nat.
  -- We indicate here that the latest version is S (S Z), for which Two is
  -- a type synonym (from the TypeNat package).
  type LatestVersion Review = Three

  -- The version history for Review corresponds to the revisions given above.
  data VersionHistory Review n where

    -- First, there was:
    --   data Review = Good | Bad
    -- which in GADT syntax is
    --   data Review where
    --     Good :: Review
    --     Bad :: Review
    -- We just embed that into this GADT, prefixing the names with Review0.
    -- We are careful to choose Zero as the nat index for these constructors.
    Review0Good :: VersionHistory Review Zero
    Review0Bad :: VersionHistory Review Zero

    -- Next, we decided to appeal to the indecisive user, with a bigger
    -- Review type:
    --   data Review = Good | Bad | Mediocre
    -- This is just like the 0th version: write it in GADT style
    --   data Review where
    --     Good : Review
    --     Bad :: Review
    --     Mediocre :: Review
    -- then prefix it and throw it into the VersionHistory.
    Review1Good :: VersionHistory Review One
    Review1Bad :: VersionHistory Review One
    Review1Mediocre :: VersionHistory Review One

    -- To give even more expressiveness to the user, we offer an Other
    -- review with arbitrary string:
    --   data Review = Good | Bad | Mediocre | Other String
    -- Now we have a constructor with associated data: a String. As always, we
    -- just write it in GADT syntax, prefix it, and splice it into the
    -- VersionHistory.
    Review2Good :: VersionHistory Review Two
    Review2Bad :: VersionHistory Review Two
    Review2Mediocre :: VersionHistory Review Two
    Review2Other :: String -> VersionHistory Review Two

    Review3 :: String -> VersionHistory Review Three

  -- Any Versioned t instance needs a bijection between the latest version
  -- in VersionHistory and t itself. Given the way in which we construct
  -- VersionHistory, this is completely mechanical. The type index isolates the
  -- set of constructors which correspond exactly to the constructors of t.
  bijectionLatestVersion = Bi forwards backwards

    where

      forwards :: Review -> (VersionHistory Review) (LatestVersion Review)
      forwards review = case review of
          Review str -> Review3 str

      backwards :: (VersionHistory Review) (LatestVersion Review) -> Review
      backwards version = case version of
          Review3 str -> Review str

  -- The migration path is a sequence of single-step migrations (from version
  -- n to version (S n)).
  migrationPath =
        ConsMigrationPath migrate23
      $ ConsMigrationPath migrate12
      $ ConsMigrationPath migrate01
        TrivialMigrationPath

migrate01 :: Migration (VersionHistory Review) One
migrate01 = Migration $ \versionZero -> case versionZero of
    Review0Good -> Review1Good
    Review0Bad -> Review1Bad

migrate12 :: Migration (VersionHistory Review) Two
migrate12 = Migration $ \versionOne -> case versionOne of
    Review1Good -> Review2Good
    Review1Bad -> Review2Bad
    Review1Mediocre -> Review2Mediocre

migrate23 :: Migration (VersionHistory Review) Three
migrate23 = Migration $ \versionTwo -> case versionTwo of
    Review2Good -> Review3 "Good"
    Review2Bad -> Review3 "Bad"
    Review2Mediocre -> Review3 "Mediocre"
    Review2Other str -> Review3 str
