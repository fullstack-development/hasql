module Hasql.Private.MonadSession where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Hasql.Private.Connection as Connection
import Hasql.Private.Errors
import Hasql.Private.Prelude
import qualified Hasql.Statement as Statement

-- |
-- Typeclass for monads that support executing PSQL statements
class (Monad m, Catch.MonadMask m) => MonadSession m where
  -- |
  -- Possibly a multi-statement query,
  -- which however cannot be parameterized or prepared,
  -- nor can any results of it be collected.
  sql :: ByteString -> m ()
  -- |
  -- Parameters and a specification of a parametric single-statement query to apply them to.
  statement :: params -> Statement.Statement params result -> m result
  -- |
  -- Obtain the current PostgreSQL `Connection.Connection`
  askConnection :: m Connection.Connection
  -- |
  -- Throw a `QueryError` in the monad
  --
  -- The default implementation throws it as an exception, thus it becomes possible to
  -- catch it with generic `Catch.catch`:
  --
  -- @
  --   e <- `Catch.try` $ `throwQueryError` exampleQueryError
  --   {- e === Left exampleQueryError -}
  -- @
  throwQueryError :: QueryError -> m x
  throwQueryError = Catch.throwM
  -- |
  -- Catch and handle a `QueryError`
  --
  -- Note that this does not catch other exceptions or underlying monad's early exit.
  -- For this, you have to use functions from `Catch.MonadCatch` or `Catch.MonadMask`
  catchQueryError :: m a -> (QueryError -> m a) -> m a
  catchQueryError = Catch.catch

instance (MonadSession m) => MonadSession (Except.ExceptT e m) where
  sql bs = lift (sql bs)
  statement ps s = lift (statement ps s)
  askConnection = lift askConnection
  throwQueryError err = lift (throwQueryError err)
  catchQueryError act han =
    Except.ExceptT $
      catchQueryError (Except.runExceptT act) (\e -> Except.runExceptT (han e))

instance (MonadSession m) => MonadSession (Reader.ReaderT r m) where
  sql bs = lift (sql bs)
  statement ps s = lift (statement ps s)
  askConnection = lift askConnection
  throwQueryError err = lift (throwQueryError err)
  catchQueryError act han =
    Reader.ReaderT $ \r ->
      catchQueryError (Reader.runReaderT act r) (\e -> Reader.runReaderT (han e) r)

instance (MonadSession m) => MonadSession (State.Lazy.StateT s m) where
  sql bs = lift (sql bs)
  statement ps s = lift (statement ps s)
  askConnection = lift askConnection
  throwQueryError err = lift (throwQueryError err)
  catchQueryError act han =
    State.Lazy.StateT $ \s ->
      catchQueryError (State.Lazy.runStateT act s) (\e -> State.Lazy.runStateT (han e) s)

instance (MonadSession m) => MonadSession (State.Strict.StateT s m) where
  sql bs = lift (sql bs)
  statement ps s = lift (statement ps s)
  askConnection = lift askConnection
  throwQueryError err = lift (throwQueryError err)
  catchQueryError act han =
    State.Strict.StateT $ \s ->
      catchQueryError (State.Strict.runStateT act s) (\e -> State.Strict.runStateT (han e) s)
