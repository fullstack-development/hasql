module Hasql.Loops.Sender where

import Hasql.Prelude
import qualified Hasql.Socket as A
import qualified Data.ByteString as B


{-# INLINABLE loop #-}
loop :: A.Socket -> IO ByteString -> (Text -> IO ()) -> IO ()
loop socket getNextChunk reportError =
  forever $ do
    bytes <- getNextChunk
    traceEventIO "START Sender/send"
    resultOfSending <- A.send socket bytes
    case resultOfSending of
      Right () -> return ()
      Left msg -> reportError msg
    traceEventIO "STOP Sender/send"
