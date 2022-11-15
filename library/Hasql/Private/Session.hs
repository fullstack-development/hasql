module Hasql.Private.Session where

import qualified Control.Monad.Catch as Catch
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.Private.Connection as Connection
import qualified Hasql.Private.Decoders.Result as Decoders.Result
import qualified Hasql.Private.Decoders.Results as Decoders.Results
import qualified Hasql.Private.Encoders as Encoders
import qualified Hasql.Private.Encoders.Params as Encoders.Params
import Hasql.Private.Errors
import qualified Hasql.Private.IO as IO
import Hasql.Private.MonadSession
import Hasql.Private.Prelude
import qualified Hasql.Private.Settings as Settings
import qualified Hasql.Statement as Statement

-- |
-- A batch of actions to be executed in the context of a database connection.
newtype Session a
  = Session (ReaderT Connection.Connection IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Connection.Connection,
            Catch.MonadThrow, Catch.MonadCatch, Catch.MonadMask)

instance MonadError QueryError Session where
  throwError = Catch.throwM
  catchError = Catch.catch

instance MonadSession Session where
  sql = sessionSql
  statement = sessionStatement
  askConnection = ask

-- |
-- Smart constructor of the Session object
session :: (Connection.Connection -> IO (Either QueryError a)) -> Session a
session inner =
  Session $ ReaderT $ \conn ->
    inner conn >>= either Catch.throwM pure

-- |
-- Executes a bunch of commands on the provided connection, returns errors in `Left`
run :: Session a -> Connection.Connection -> IO (Either QueryError a)
run session connection =
  Catch.try $ runThrow session connection

-- |
-- Executes a bunch of commands on the provided connection, throws errors as exceptions
runThrow :: Session a -> Connection.Connection -> IO a
runThrow (Session impl) connection =
  runReaderT impl connection

sessionSql :: ByteString -> Session ()
sessionSql sql =
  session $ \(Connection.Connection pqConnectionRef integerDatetimes registry) ->
    fmap (mapLeft (QueryError sql [])) $
      withMVar pqConnectionRef $ \pqConnection -> do
        r1 <- IO.sendNonparametricStatement pqConnection sql
        r2 <- IO.getResults pqConnection integerDatetimes decoder
        return $ r1 *> r2
  where
    decoder =
      Decoders.Results.single Decoders.Result.noResult

sessionStatement :: params -> Statement.Statement params result -> Session result
sessionStatement input (Statement.Statement template (Encoders.Params paramsEncoder) decoder preparable) =
  session $ \(Connection.Connection pqConnectionRef integerDatetimes registry) ->
    fmap (mapLeft (QueryError template inputReps)) $
      withMVar pqConnectionRef $ \pqConnection -> do
        r1 <- IO.sendParametricStatement pqConnection integerDatetimes registry template paramsEncoder preparable input
        r2 <- IO.getResults pqConnection integerDatetimes (unsafeCoerce decoder)
        return $ r1 *> r2
  where
    inputReps =
      let Encoders.Params.Params (Op encoderOp) = paramsEncoder
          step (_, _, _, rendering) acc =
            rendering : acc
       in foldr step [] (encoderOp input)
