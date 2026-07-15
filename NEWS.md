# BigDataPE 0.2.0

## Changes

- BigDataPE is now a thin wrapper around the new generic 'apifetch' package:
  every `bdpe_*` function delegates to its `apifetch::af_*` counterpart with the
  Big Data PE service conventions (token sent verbatim in the `Authorization`
  header, `limit`/`offset` as HTTP headers, the `"Mensagem"` status column
  dropped when combining chunks). The public API is unchanged, and the
  environment-variable token names (`BigDataPE_<dataset>`) are identical, so
  existing code and stored tokens keep working.
- `Imports` is now just `apifetch`; the direct dependencies on `cli`, `dplyr`,
  `tibble`, and `httr2` are inherited transitively through `apifetch`.

# BigDataPE 0.1.0

## New Features

- Migrated all user-facing messages, warnings, and errors from base R (`message()`, `stop()`, `stopifnot()`) to the `cli` package (`cli::cli_abort()`, `cli::cli_alert_success()`, `cli::cli_alert_warning()`, `cli::cli_alert_info()`). This provides richer, more informative, and consistently formatted console output.
- Added progress feedback in `bdpe_fetch_chunks()` when `verbosity > 0`, showing the number of records fetched per chunk and the running total.
- Added a completion summary message in `bdpe_fetch_chunks()` reporting the total number of records retrieved.
- Improved input validation in `bdpe_fetch_data()` and `bdpe_fetch_chunks()` with descriptive `cli` error messages replacing `stopifnot()` calls.
- Numeric parameters (`limit`, `offset`, `total_limit`, `chunk_size`, `verbosity`) are now automatically coerced to integer when possible (e.g., `limit = 50` now works without requiring `limit = 50L`). An informative error is raised only when conversion is not possible (e.g., non-numeric or fractional values).
- Improved HTTP error handling in `bdpe_fetch_data()`: connection failures and HTTP errors (e.g., 503 Service Unavailable) now produce clear, user-friendly messages with guidance on network requirements and next steps, instead of raw `httr2` errors.

## Changes

- Added `cli` as a package dependency in `Imports`.
- Added Marcos Wasiliew and Carlos Amorim as package authors.
- Added vignette "Exploring Dengue Data with BigDataPE" demonstrating the full package workflow with the public dengue dataset.

# BigDataPE 0.0.96

## Bug Fixes

- Fixed an issue in `bdpe_fetch_chunks()` where the `offset` parameter was being formatted as a scientific double (e.g., `1e+05`), which caused the API to fail. The value is now properly formatted as an integer string, ensuring compatibility with the API which expects an exact integer format.



