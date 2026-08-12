# peekbankr 0.4.0

## Redivis backend

peekbankr now reads from the versioned peekbank dataset on Redivis
(https://redivis.com/datapages/datasets/peekbank) instead of the retired
hosted MySQL server. All `get_*` signatures are unchanged and results are
verified against fixtures captured from the MySQL backend (see
`tests/testthat/test-characterization.R`).

- `connect_to_peekbank()` returns a lightweight version handle instead of a
  DBI connection; `db_args`/`host`/`port`/`ssl`/`compress` are deprecated
  and ignored. Version resolution is discovered from the Redivis dataset
  itself ("current" = latest release; each version's `release_info` table
  names its Peekbank release), so nothing is hardcoded.
- `get_*` functions return local tibbles in all cases (previously they
  returned lazy remote tbls when a connection was supplied); existing
  `collect()` calls are harmless no-ops.
- `get_aoi_timepoints()` still transfers the run-length-encoded table and
  decodes locally.
- `get_sql_query()` now runs BigQuery Standard SQL (via Redivis) rather than
  MySQL/MariaDB SQL. Note that string comparison is case-sensitive (MySQL's
  default collation was case-insensitive).
- `list_peekbank_tables()` reflects the Redivis staging: the Django
  bookkeeping tables and the rebuildable `aoi_timepoints_indexed`
  intermediate are gone; a `release_info` table is added.
- Removed dependencies: DBI, dbplyr, RMariaDB. The `redivis` client is in
  Suggests (install from https://langcog.r-universe.dev).

## Bug fixes

- `unpack_aux_data()` no longer requires dplyr to be attached (bare
  `pull`/`ungroup`/`all_of`/`as_tibble` calls are now namespaced).
- In the retired backend, `get_aoi_timepoints()` disconnected a
  user-supplied connection after use, breaking subsequent queries on that
  connection; with version handles this can no longer occur.

## Tests

- Migrated the loose test scripts to testthat (edition 3): offline unit
  tests for argument validation, RLE decoding, `ds.resample_times`, and
  `unpack_aux_data`; network tests gated on `PEEKBANK_NETWORK_TESTS=true`;
  characterization tests against MySQL-era fixtures gated on
  `PEEKBANK_FIXTURES_DIR`.
