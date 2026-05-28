# Changelog

## 0.1.0.0 — 2026-05-28

- Initial release.
- `TracedPool` wrapping `Hasql.Pool.Pool` with cached span attributes.
- `use` / `useStatement` / `useSession` instrumented variants of `Hasql.Pool.use`.
- `hasqlSpan` helper for wrapping arbitrary Hasql IO actions in a Client span.
- Maps `UsageError` to `SpanStatus = Error` with attribute `db.response.status_code` from PostgreSQL `SQLSTATE` when available.
- OTel database semantic conventions: `db.system=postgresql`, `db.namespace`, `db.statement`, `db.operation.name`, `server.address`, `server.port`, `db.user`.
