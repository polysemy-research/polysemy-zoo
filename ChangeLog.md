# Changelog for polysemy-zoo

## 0.8.1.0 (2022-09-16)
- Add support for GHC 9.2
  ([#79](https://github.com/polysemy-research/polysemy-zoo/pull/79), thanks to @spacekitteh)

## 0.8.0.0 (2022-03-08)
### Breaking Changes
- Removed `Polysemy.IdempotentLowering`, previously deprecated in favor of `Polysemy.Final`.

### Other Changes
- Updated `asyncToIOFinalGlobal` to support `cancel`.

## 0.7.0.2 (2020-11-04)
- Add support for GHC 9.0
  ([#70](https://github.com/polysemy-research/polysemy-zoo/pull/70), thanks to @funketh)

## 0.7.0.1 (2020-10-06)
- As a stop-gap for [#65](https://github.com/polysemy-research/polysemy-zoo/issues/65), the library has been rewritten to no longer use `polysemy-plugin`. This should allow GHC 8.10 users to use the library.

## 0.7.0.0 (2020-02-14)
### Breaking Changes
- `Tagged` has been migrated to `polysemy` proper.
- `Polysemy.Alias` and `InterpreterOf` has been removed in favor of
    `InterpreterFor`, which is now part of `polysemy` proper
    (thanks to @bolt12).
- Removed `runKVStoreInRedis`, `runSetStoreInRedis`, and `Polysemy.Redis.Utils`
    due to lack of use.

### Other Changes
- Added `MonadThrow` and `MonadCatch` constraint absorbers which operate
    via `Error SomeException` (thanks to @adamConnerSax).
- Added `Polysemy.Input.Streaming`, which offers
    [streaming](https://hackage.haskell.org/package/streaming) interoperability.
- Added `Polysemy.Reader.Compact`, which is useful for `Reader` effects
    which provide a large structure (thanks to @spacekitteh).


## 0.6.0.1 (2019-09-12)

- Fixed the implementation of `atomicPut`

## 0.6.0.0 (2019-09-05)

- `Final` has been migrated to `polysemy` proper.
    Only `Polysemy.Final.MTL` and experimental features pertaining to
    `Final` remain in `polysemy-zoo`.
- Added `Fresh` effect.
- Added `Tagged` effect.
- Added `MonadCont` constraint absorber which operates via the `Cont` effect.
- Added `runContViaFresh`, which is still under development.
- Added `runReaderFixSem`
- Added `EndState` effect
- Added `RevState` effect
- Added `lowerFinal`
- Added `runFinalSem`
- Added `nonDetToFinal`
- Added `interpretFinalGlobal`
- Added `asyncToIOFinalGlobal`
- Added `resourceToIOFinalGlobal`

## 0.5.0.1 (2019-07-25)

- Fixed some bugs with haddock

## 0.5.0.0 (2019-07-24)

- Added Continuation effects (thanks to @KingoftheHomeless)
- Update to `polysemy-1.0.0.0`'s new names

## 0.4.0.1 (2019-07-10)

- Fixed an erroneous lower bound in the tests

## 0.4.0.0 (2019-07-10)

### Breaking Changes

- The semantics of `absorbWriter` are now aligned with the `MTL` implementation

### New Effects and Interpreters

- Added `SetStore` effect
- Added `Floodgate` effect
- Added `Final` effect, together with submodules of interpreters using it
    (thanks to @KingoftheHomeless)
- Added `lookupOrThrowKV`, `existsKV` and `modifyKV` actions to `KVStore`
- Added Redis interpretations of `SetStore` and `KVStore`

## 0.3.0.0 (2019-06-17)

- Removed `Polysemy.MTL`
- The machinery for MTL absorption is now monomorphized in
    `Polysemy.ConstraintAbsorber`. See the documentation there and in submodules
    for more information.

## 0.2.0.0 (2019-06-14)

- Removed `Polysemy.RandomFu`, which is moving to its own package
- Add explicit cabal bounds for dependencies of `polysemy-zoo`

## 0.1.2.1 (2019-06-12)

- Update the tests to run against `polysemy-0.4.0.0`

## 0.1.2.0 (2019-06-01)

- Added `Polysemy.MTL` for inter-op with MTL (thanks to @adamConnerSax)
- Moved `Polysemy.Random` from `polysemy`
- Added `Polysemy.RandomFu` (thanks to @adamConnerSax)
- Added `fixedNat` and `fixedNat'` to `Polysemy.IdempotentLowering` for working
    with higher-order effects.

## 0.1.1.0 (2019-05-22)

- Added `Polysemy.IdempotentLowering`
