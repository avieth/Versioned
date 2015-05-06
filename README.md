Versioned
=========

Unobtrusively version your Haskell datatypes to obtain migrations.

An instance `t` of the `Versioned` typeclass has an associated `Nat`-indexed
datatype, known as the `VersionHistory t`, which describes all revisions of
`t` such that there is a bijection between the `n`'th version of `t` and values
of the type `VersionHistory t n`. When earlier versions are obsoleted by later
ones, the earlier representations remain in the program, coded by the
`VersionHistory` of the given type, so that programs which must interact with
older versions (database interfacing programs, for instance) can do so safely.

Example
=======

Here's a 3-revision datatype and its `VersionHistory`. See `examples/Review.hs`
for the full `Versioned` instance.

```Haskell
-- Our 0th version. Would be deleted in real code.
--data Review = Good | Bad

-- Our first version. Would be deleted in real code.
--data Review = Good | Bad | Mediocre

-- Our second version.
data Review = Good | Bad | Mediocre | Other String

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
```
