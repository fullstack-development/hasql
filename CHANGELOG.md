# 1.5.0.2

- Add `exceptions` instances (`MonadThrow`, `MonadCatch`, `MonadMask`) to the
  `Session` monad.
- Introduce `MonadSession` typeclass with the `sql` and `statement` functions.

# 1.5

- Added column number to `RowError` (breaking change).
- Added `MonadReader Connection` instance for Session.
