# lcNEPSE

`lcNEPSE` is an independent R package for reproducible access to Nepalese
stock-market data as published by [ShareSansar](https://www.sharesansar.com/).
A scheduled GitHub Actions job scrapes the published tables, validates them,
and commits a dated archive with SHA-256 hashes to the `data-archive` branch of
this repository. The package downloads only hash-verified partitions, caches
them outside the installed package, standardizes the columns, and keeps the
source URL and archive vintage attached to every row.

It is not affiliated with or endorsed by ShareSansar or the Nepal Stock
Exchange (NEPSE).

## Installation

```r
pak::pak("koshishjlc72/lcNEPSE")
```

## Quick start

```r
library(lcNEPSE)

nepse_catalog()          # datasets, date coverage, last update
nepse_symbols()          # listed symbols and company names
nepse_search("hydro")    # find symbols by name

# One trading day, one symbol
nepse_prices("NABIL", start = "2026-10-01", end = "2026-10-30")

# Everything archived for a set of symbols
nepse_prices(c("NABIL", "NTC"), start = "2026-01-01")

# Sector sub-index levels
nepse_indices("Banking SubIndex", start = "2026-01-01")

# One row per trading day: turnover, volume, advancers, decliners
nepse_market(start = "2026-10-01")

# The latest session, and the calendar of archived sessions
nepse_latest()
nepse_dates()
```

The first request for a month downloads that monthly partition into the user
cache (`tools::R_user_dir("lcNEPSE", "cache")`). Later requests reuse the
content-addressed copy. `refresh = TRUE` re-reads the archive manifest.

## How a call reaches the data

No price data is bundled with the package. Each call reads the archive index
and then downloads only the partitions it needs, straight from the repository:

```text
nepse_prices("NABIL", start = "2019-01-01")
  -> GET .../data-archive/manifest.csv
       validated, cached for 15 minutes
  -> GET .../data-archive/data/prices/monthly/2019-01.csv
       SHA-256 compared with the manifest entry
  -> parsed, filtered to NABIL, returned with provenance columns
```

`nepse_archive_url()` prints the base URL in use. It can be redirected with
`options(lcNEPSE.mirror_url = ...)`, or pointed at a local copy of the branch
with `options(lcNEPSE.mirror_dir = ...)` for offline work. `refresh = TRUE`
re-reads `manifest.csv`, so a session published a minute earlier is picked up
without restarting R.

Because every file is verified against a published hash, a truncated or
substituted download fails loudly instead of entering the cache.

## What is archived

| Dataset ID | Contents | Frequency |
|---|---|---|
| `prices` | Open, high, low, close, LTP, VWAP, volume, turnover, transactions, change, range, 120-day and 180-day averages, 52-week high and low, and ShareSansar's `Conf.` column | Every trading day |
| `indices` | Open, high, low, close, turnover, point change, percent change, and 52-week range for each published sector sub-index | Latest session |
| `symbols` | Symbol, company name, first and last archived date | Current master |

The 120-day and 180-day averages are published on the current-session price
table. ShareSansar's date-filter view omits those two columns, so a
back-filled historical day stores them as missing rather than inventing
values. Sector indices are only published for the latest session, so they are
archived for that session only; `nepse_indices()` therefore covers fewer dates
than `nepse_prices()`.

Every price row also carries `source_url`, `retrieved_at`, `source_sha256`,
`archive_path`, `archive_updated_at`, and `parser_version`.

`nepse_prices(long = TRUE)` returns one row per symbol, date, and field, which
is convenient for plotting and for joining across heterogeneous measures.

## How the archive works

```text
manifest.csv                          index with SHA-256 and date coverage
data/prices/daily/YYYY/YYYY-MM-DD.csv  immutable snapshot per trading day
data/prices/monthly/YYYY-MM.csv        monthly partition read by the package
data/indices/daily|monthly/...         the same layout for sub-indices
data/symbols.csv                       symbol master
```

The Actions job runs after the close on NEPSE trading days. It:

1. scrapes the ShareSansar price table and sub-index table;
2. rejects the run if the expected columns moved, a row is misaligned, or the
   snapshot has too few symbols to be the whole market;
3. writes the daily file, rebuilds the monthly partition, and updates the
   symbol master;
4. verifies the whole archive through the package's public API with an empty
   cache (`data-raw/verify-archive.R`);
5. commits only if every check passed.

If ShareSansar revises a session after it was archived, the previous content is
kept as `YYYY-MM-DD@<hash8>.csv` and only one version is marked `current`.

Historical sessions can be back-filled through the same workflow. ShareSansar's
date filter is an AJAX endpoint that returns the table without the 120-day and
180-day columns and without sub-indices, so those fields are recorded as
missing for back-filled days. Each day is validated and appended to the same
monthly partition.

## Live mode

The archive is the normal route, but the current page can be read directly:

```r
nepse_prices(source = "live")
nepse_prices(source = "live", start = "2026-10-29", end = "2026-10-29")
```

Live mode forwards ShareSansar's table for that day and does not use the
archive. Historical backfill through the site's date filter is available
through the same scraper; `data-raw/backfill-archive.R` uses it to populate the
archive.

## Reliability and limitations

- Downloaded partitions are checked against the SHA-256 published in
  `manifest.csv`; a mismatch is an error, never a silent fallback.
- Manifest validation rejects unknown datasets, unsafe paths, parser-version
  drift, missing dates, and duplicate `current` versions.
- The package never rewrites an archived partition. The cache is
  content-addressed, so a revised partition lands beside the old one.
- ShareSansar's `Conf.` column is preserved exactly as published. The portal
  documents it as "Stock Confidence"; this package reproduces the number and
  does not interpret it.
- Sector labels for individual symbols are not published in the price table;
  `symbols` therefore does not invent a `sector` column.
- Scraping depends on ShareSansar's HTML. A layout change fails the workflow
  loudly and leaves the last verified archive in place. The live page and the
  dated AJAX view are validated against separate column contracts, so a change
  in either fails instead of silently dropping columns.
- Units are preserved: prices in NPR per share, volume in shares as published,
  turnover in NPR, transactions as a count, index levels in points.

## Data source and attribution

Data are published by ShareSansar. Cite ShareSansar as the source and retain
`source_url`, `retrieved_at`, and `source_sha256` in reproducible outputs. The
package code is MIT licensed; the data remain subject to the publisher's terms.

## Development

```r
# Offline test suite (no network)
testthat::test_local()

# Archive one session locally, then verify it end to end
Rscript data-raw/update-archive.R archive
Rscript data-raw/verify-archive.R archive
```

The `data-archive` branch is written only by
`.github/workflows/update-archive.yml`. Manual backfill:

```text
Actions -> Update ShareSansar data archive -> Run workflow
  backfill_from: 2026-01-01
  backfill_to:   2026-03-31
```
