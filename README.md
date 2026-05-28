# hs-opentelemetry-instrumentation-hasql

OpenTelemetry tracing instrumentation for the [`hasql`](https://hackage.haskell.org/package/hasql) Postgres client and [`hasql-pool`](https://hackage.haskell.org/package/hasql-pool).

Mirrors the API style of [`hs-opentelemetry-instrumentation-postgresql-simple`](https://github.com/iand675/hs-opentelemetry/tree/main/instrumentation/postgresql-simple) — every executed `Session` becomes a Client span with the OTel database semantic-convention attributes, and Hasql `UsageError`s are recorded onto the span as errors.

## Usage

```haskell
import qualified OpenTelemetry.Instrumentation.Hasql as Hasql
import qualified Hasql.Pool.Config as HPC

main :: IO ()
main = do
  pool <- Hasql.acquireFromConnString cfg "postgresql://app@db:5432/monoscope"
  result <- Hasql.use pool mySession
  ...
```

The span carries:

| Attribute | Value |
|-----------|-------|
| `db.system` | `"postgresql"` |
| `db.namespace` / `db.name` | dbname from the connection string |
| `db.statement` | SQL text (when known — i.e. when calling `useStatement` / `useSession` with a single `Statement`) |
| `db.operation.name` | leading SQL verb (`SELECT`, `INSERT`, …) |
| `server.address` / `net.peer.name` | host |
| `server.port` / `net.peer.port` | port |
| `db.user` | user |

On a `Left UsageError`:
- span status set to `Error` with a message describing the error variant,
- `db.response.status_code` set to the PostgreSQL `SQLSTATE` when available,
- `error.type` set to a short tag (`client` / `result` / `acquisition_timeout` / `connection`).

Synchronous exceptions thrown inside the Session are recorded and rethrown (matching `inSpan` semantics).

## Span granularity

`use` opens one span per call. The library does not transparently break a multi-statement `Session` into per-statement spans — `Hasql.Session` is opaque so there is no safe interception point. If you want per-statement spans, use `useStatement` (single `Hasql.Statement`) or wrap your own helpers with `hasqlSpan`.

## License

BSD-3-Clause.
