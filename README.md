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
