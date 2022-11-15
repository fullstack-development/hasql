# 1.6.3

- Add `exceptions` instances (`MonadThrow`, `MonadCatch`, `MonadMask`) to the
  `Session` monad.
- Introduce `MonadSession` typeclass with the `sql` and `statement` functions.

# 1.6.2

- Added composite encoder
- Added `oid` and `name` encoders

# 1.6.1

- Added `jsonLazyBytes` and `jsonbLazyBytes`

# 1.6

- Added position to `ServerError` (breaking change).
- Disabled failure on empty query.

# 1.5

- Added column number to `RowError` (breaking change).
- Added `MonadReader Connection` instance for Session.
