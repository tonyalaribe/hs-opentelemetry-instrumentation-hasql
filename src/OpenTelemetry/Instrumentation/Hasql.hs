{- |
OpenTelemetry tracing instrumentation for @hasql@ + @hasql-pool@.

Every call to 'use' opens a Client span carrying OTel database semantic-convention
attributes (@db.system@, @db.namespace@, @server.address@, @server.port@, @db.user@).
'useStatement' additionally fills in @db.statement@ + @db.operation.name@ from the
'Statement' constructor. Hasql 'UsageError's are mapped onto the span status as
'Error' (with @db.response.status_code@ set from the PostgreSQL SQLSTATE when
available), and synchronous exceptions are recorded and rethrown.

Designed to mirror the API of @hs-opentelemetry-instrumentation-postgresql-simple@.
-}
module OpenTelemetry.Instrumentation.Hasql
  ( -- * Pool with cached span attributes
    TracedPool (..)
  , StaticAttrs (..)
  , emptyAttrs
  , acquire
  , acquireWithAttrs
  , acquireFromConnString
  , release

    -- * Running sessions (instrumented)
  , use
  , useStatement
  , useSession
  , hasqlSpan

    -- * Lower-level helpers
  , staticAttrsFromConnString
  , recordUsageError
  , extractOperationName
  , extractStatementSql

    -- * Re-exports
  , module Hasql.Pool
  , module Hasql.Session
  , module Hasql.Statement
  ) where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Stack (HasCallStack)

import Hasql.Errors
  ( IsError (..)
  , ServerError (..)
  , SessionError (..)
  , StatementError (..)
  , toDetailedText
  )
import Hasql.Pool (Pool, UsageError (..))
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as PoolConfig
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import Hasql.Statement (Statement (..))

import OpenTelemetry.Attributes (Attribute, ToAttribute (..), emptyAttributes)
import OpenTelemetry.Trace.Core
  ( InstrumentationLibrary (..)
  , Span
  , SpanArguments (..)
  , SpanKind (Client)
  , SpanStatus (Error)
  , Tracer
  , addAttribute
  , defaultSpanArguments
  , getGlobalTracerProvider
  , inSpan'
  , makeTracer
  , recordException
  , setStatus
  , tracerOptions
  )

-- ---------------------------------------------------------------------------
-- Tracer

instrumentation :: InstrumentationLibrary
instrumentation =
  InstrumentationLibrary
    { libraryName = "hs-opentelemetry-instrumentation-hasql"
    , libraryVersion = "0.1.0.0"
    , librarySchemaUrl = ""
    , libraryAttributes = emptyAttributes
    }

getTracer :: MonadIO m => m Tracer
getTracer = do
  tp <- getGlobalTracerProvider
  pure $ makeTracer tp instrumentation tracerOptions

-- ---------------------------------------------------------------------------
-- TracedPool

-- | Static per-connection attributes captured once at pool-acquire time and
-- attached to every span produced for that pool. Hasql hides the underlying
-- libpq handle, so we cache these up front rather than introspecting per call.
data StaticAttrs = StaticAttrs
  { saHost :: Maybe Text
  , saPort :: Maybe Int
  , saDbName :: Maybe Text
  , saUser :: Maybe Text
  }
  deriving stock (Show, Eq)

emptyAttrs :: StaticAttrs
emptyAttrs = StaticAttrs Nothing Nothing Nothing Nothing

-- | A 'Hasql.Pool.Pool' bundled with its static span attributes. The pre-rendered
-- attribute map is built once at acquire time so per-call overhead stays low.
data TracedPool = TracedPool
  { rawPool :: Pool
  , poolAttrs :: StaticAttrs
  , attrMap :: HashMap Text Attribute
  }

-- | Acquire a 'TracedPool' from a hasql-pool 'PoolConfig.Config'. Static
-- connection attributes default to empty; use 'acquireWithAttrs' or
-- 'acquireFromConnString' to populate them.
acquire :: MonadIO m => PoolConfig.Config -> m TracedPool
acquire = acquireWithAttrs emptyAttrs

acquireWithAttrs :: MonadIO m => StaticAttrs -> PoolConfig.Config -> m TracedPool
acquireWithAttrs attrs cfg = liftIO $ do
  p <- Pool.acquire cfg
  pure TracedPool {rawPool = p, poolAttrs = attrs, attrMap = renderStaticAttrs attrs}

-- | Acquire a 'TracedPool', parsing a libpq connection string (URI or
-- keyword/value form) for the static attributes. Unknown fields are dropped.
acquireFromConnString :: MonadIO m => PoolConfig.Config -> ByteString -> m TracedPool
acquireFromConnString cfg conn = acquireWithAttrs (staticAttrsFromConnString conn) cfg

release :: MonadIO m => TracedPool -> m ()
release tp = liftIO $ Pool.release (rawPool tp)

renderStaticAttrs :: StaticAttrs -> HashMap Text Attribute
renderStaticAttrs StaticAttrs {..} =
  H.fromList $
    ("db.system", toAttribute ("postgresql" :: Text))
      : catMaybes'
        [ (\h -> ("server.address", toAttribute h)) <$> saHost
        , (\h -> ("net.peer.name", toAttribute h)) <$> saHost
        , (\p -> ("server.port", toAttribute (fromIntegral p :: Int))) <$> saPort
        , (\p -> ("net.peer.port", toAttribute (fromIntegral p :: Int))) <$> saPort
        , (\d -> ("db.namespace", toAttribute d)) <$> saDbName
        , (\d -> ("db.name", toAttribute d)) <$> saDbName
        , (\u -> ("db.user", toAttribute u)) <$> saUser
        ]
  where
    catMaybes' = foldr (\x acc -> maybe acc (: acc) x) []

-- ---------------------------------------------------------------------------
-- Connection-string parsing

-- | Parse libpq-format connection info from either the URI form
-- (@postgresql:\/\/user:pw\@host:port\/db@) or keyword=value form
-- (@host=h port=5432 dbname=d user=u@). Unknown / missing fields stay 'Nothing'.
staticAttrsFromConnString :: ByteString -> StaticAttrs
staticAttrsFromConnString raw
  | "postgresql://" `BS8.isPrefixOf` raw || "postgres://" `BS8.isPrefixOf` raw =
      parseUri (TE.decodeUtf8Lenient raw)
  | otherwise =
      parseKv (TE.decodeUtf8Lenient raw)

parseKv :: Text -> StaticAttrs
parseKv t = foldr apply emptyAttrs (T.words t)
  where
    apply tok acc = case T.break (== '=') tok of
      (k, eqv)
        | T.length eqv > 1 ->
            let v = T.tail eqv
             in case k of
                  "host" -> acc {saHost = Just v}
                  "hostaddr" -> acc {saHost = Just v}
                  "port" -> acc {saPort = parseInt v}
                  "dbname" -> acc {saDbName = Just v}
                  "user" -> acc {saUser = Just v}
                  _ -> acc
      _ -> acc

parseUri :: Text -> StaticAttrs
parseUri t0 =
  -- strip scheme://
  let afterScheme = T.drop 2 (T.dropWhile (/= '/') (T.dropWhile (/= ':') t0))
      (auth, pathQ) = T.break (== '/') afterScheme
      (userInfo, hostPort) = case T.break (== '@') auth of
        (left, right)
          | T.null right -> (Nothing, left)
          | otherwise -> (Just left, T.drop 1 right)
      (host, port) = case T.break (== ':') hostPort of
        (h, p)
          | T.null p -> (h, Nothing)
          | otherwise -> (h, parseInt (T.drop 1 p))
      db = T.takeWhile (\c -> c /= '?' && c /= '/') (T.dropWhile (== '/') pathQ)
      user = (T.takeWhile (/= ':')) <$> userInfo
   in StaticAttrs
        { saHost = nonEmpty host
        , saPort = port
        , saDbName = nonEmpty db
        , saUser = user >>= nonEmpty
        }
  where
    nonEmpty x = if T.null x then Nothing else Just x

parseInt :: Text -> Maybe Int
parseInt t
  | T.null t || not (T.all isDigit t) = Nothing
  | otherwise = Just (T.foldl' (\acc c -> acc * 10 + fromEnum c - fromEnum '0') 0 t)

-- ---------------------------------------------------------------------------
-- Running sessions

-- | Run a 'Session' with tracing. One span per call, name @"hasql.session [db]"@,
-- 'Client' kind. SQL text is not populated (Sessions are opaque); use
-- 'useStatement' if you have a single 'Statement' so the SQL can be attached.
use :: MonadIO m => TracedPool -> Session a -> m (Either UsageError a)
use tp s = hasqlSpan tp (sessionSpanName tp) mempty (Pool.use (rawPool tp) s)

-- | Run a single 'Statement' as a 'Session', extracting SQL + operation name
-- for the span.
useStatement
  :: MonadIO m
  => TracedPool
  -> params
  -> Statement params a
  -> m (Either UsageError a)
useStatement tp params st@(Statement sqlBs _ _ _) =
  let sqlTxt = TE.decodeUtf8Lenient sqlBs
      op = extractOperationName sqlTxt
      name = case (op, saDbName (poolAttrs tp)) of
        (Just o, Just db) -> o <> " " <> db
        (Just o, Nothing) -> o
        (Nothing, Just db) -> "hasql.statement " <> db
        (Nothing, Nothing) -> "hasql.statement"
      extras =
        H.fromList $
          ("db.statement", toAttribute sqlTxt)
            : maybe [] (\o -> [("db.operation.name", toAttribute o)]) op
   in hasqlSpan tp name extras (Pool.use (rawPool tp) (Session.statement params st))

-- | Like 'use' but lets the caller supply a span name and extra attributes
-- (e.g. when running a 'Hasql.Session.sql' literal).
useSession
  :: MonadIO m
  => TracedPool
  -> Text
  -> HashMap Text Attribute
  -> Session a
  -> m (Either UsageError a)
useSession tp name extras s = hasqlSpan tp name extras (Pool.use (rawPool tp) s)

-- | Lowest-level helper: open a span around an arbitrary
-- @IO (Either UsageError a)@ using this pool's cached attributes. Records the
-- 'UsageError' on the span and also catches\/records\/rethrows any synchronous
-- exception.
hasqlSpan
  :: (HasCallStack, MonadIO m)
  => TracedPool
  -> Text
  -> HashMap Text Attribute
  -> IO (Either UsageError a)
  -> m (Either UsageError a)
hasqlSpan tp name extras action = liftIO $ do
  tracer <- getTracer
  let args = defaultSpanArguments {kind = Client, attributes = H.union extras (attrMap tp)}
  inSpan' tracer name args $ \sp -> do
    er <- try action
    case er of
      Left (e :: SomeException) -> do
        recordException sp mempty Nothing e
        setStatus sp (Error (T.pack (show e)))
        throwIO e
      Right (Left ue) -> recordUsageError sp ue >> pure (Left ue)
      Right (Right a) -> pure (Right a)

sessionSpanName :: TracedPool -> Text
sessionSpanName tp = case saDbName (poolAttrs tp) of
  Just db -> "hasql.session " <> db
  Nothing -> "hasql.session"

-- ---------------------------------------------------------------------------
-- Error mapping

-- | Map a hasql 'UsageError' onto a span: status, @error.type@, sqlstate when
-- available, and a descriptive message.
recordUsageError :: MonadIO m => Span -> UsageError -> m ()
recordUsageError sp = \case
  AcquisitionTimeoutUsageError -> do
    addAttribute sp "error.type" ("acquisition_timeout" :: Text)
    setStatus sp (Error "hasql: pool acquisition timeout")
  ConnectionUsageError ce -> do
    addAttribute sp "error.type" ("connection" :: Text)
    setStatus sp (Error ("hasql connection error: " <> toDetailedText ce))
  SessionUsageError se -> recordSessionError sp se

recordSessionError :: MonadIO m => Span -> SessionError -> m ()
recordSessionError sp = \case
  StatementSessionError _total _idx sql _params _prep stErr -> do
    addAttribute sp "db.statement" sql
    recordStatementError sp stErr
  ScriptSessionError sql se -> do
    addAttribute sp "db.statement" sql
    recordServerError sp se
  ConnectionSessionError reason -> do
    addAttribute sp "error.type" ("connection" :: Text)
    setStatus sp (Error ("hasql connection error: " <> reason))
  MissingTypesSessionError _ -> do
    addAttribute sp "error.type" ("missing_types" :: Text)
    setStatus sp (Error "hasql: missing types in database")
  DriverSessionError reason -> do
    addAttribute sp "error.type" ("driver" :: Text)
    setStatus sp (Error ("hasql driver error: " <> reason))

recordStatementError :: MonadIO m => Span -> StatementError -> m ()
recordStatementError sp = \case
  ServerStatementError se -> do
    addAttribute sp "error.type" ("result" :: Text)
    recordServerError sp se
  UnexpectedRowCountStatementError _ _ actual ->
    setStatus sp (Error ("unexpected amount of rows: " <> T.pack (show actual)))
  UnexpectedColumnCountStatementError expected actual ->
    setStatus sp (Error ("unexpected column count: expected " <> T.pack (show expected) <> ", got " <> T.pack (show actual)))
  UnexpectedColumnTypeStatementError col expOid actOid ->
    setStatus sp (Error ("unexpected column type at col=" <> T.pack (show col) <> ": expected oid " <> T.pack (show expOid) <> ", got " <> T.pack (show actOid)))
  RowStatementError row rowErr ->
    setStatus sp (Error ("row error at row=" <> T.pack (show row) <> " " <> T.pack (show rowErr)))
  UnexpectedResultStatementError t ->
    setStatus sp (Error ("unexpected result: " <> t))

recordServerError :: MonadIO m => Span -> ServerError -> m ()
recordServerError sp (ServerError code msg _detail _hint _pos) = do
  addAttribute sp "db.response.status_code" code
  addAttribute sp "error.type" code
  setStatus sp (Error (code <> ": " <> msg))

-- ---------------------------------------------------------------------------
-- SQL inspection helpers

-- | Pull the leading verb out of a SQL string (best-effort, ASCII only).
extractOperationName :: Text -> Maybe Text
extractOperationName t = case T.words (T.toUpper (T.take 64 t)) of
  (w : _) | not (T.null w) && T.all isAlphaUpper w -> Just w
  _ -> Nothing
  where
    isAlphaUpper c = c >= 'A' && c <= 'Z'

-- | Get the SQL bytes from a 'Statement'.
extractStatementSql :: Statement p r -> ByteString
extractStatementSql (Statement sqlBs _ _ _) = sqlBs
