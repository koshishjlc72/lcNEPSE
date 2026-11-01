# lcNEPSE data archive

This branch is the machine-readable archive behind the
[lcNEPSE](https://github.com/koshishjlc72/lcNEPSE) R package. It is written by
a scheduled GitHub Actions job and should not be edited by hand.

## Layout

```text
manifest.csv                     index of every file, with SHA-256 and date coverage
data/prices/daily/YYYY/YYYY-MM-DD.csv      one immutable snapshot per trading day
data/prices/monthly/YYYY-MM.csv            monthly partition assembled from the daily files
data/indices/daily/YYYY/YYYY-MM-DD.csv     one snapshot of the published sub-indices
data/indices/monthly/YYYY-MM.csv           monthly sub-index partition
data/symbols.csv                           symbol and company-name master
```

`data/prices/monthly/*` and `data/indices/monthly/*` are the files the R
package reads; the daily files are the provenance layer. If ShareSansar
revises a day after it was archived, the previous content is kept as
`YYYY-MM-DD@<hash8>.csv` and the manifest marks only one version `current`.

## Manifest columns

| Column | Meaning |
|---|---|
| `dataset_id` | `prices`, `indices`, or `symbols` |
| `partition` | `YYYY-MM` for monthly files, `YYYY-MM-DD` for daily files, `current` for the symbol master |
| `path` | path relative to this branch |
| `first_date`, `last_date` | first and last trading date in the file |
| `rows` | number of data rows |
| `sha256` | SHA-256 of the file exactly as published |
| `updated_at` | UTC time the file was written |
| `source_url` | ShareSansar page the rows came from |
| `parser_version` | parser contract the file satisfies |
| `current` | `true` for the version the package reads |

## Guarantees

- A file is published only after the whole run passes schema, type, duplicate,
  and hash verification (see `data-raw/verify-archive.R`).
- Every row keeps `source_url` and `retrieved_at`; every partition keeps a
  SHA-256 in the manifest.
- Nothing is downloaded from this branch except over HTTPS from
  `raw.githubusercontent.com`, and every download is checked against the
  manifest hash.

## Source and attribution

Data are published by [ShareSansar](https://www.sharesansar.com/). They remain
subject to that publisher's terms. This archive is an independent
reproducibility layer and is not affiliated with or endorsed by ShareSansar or
the Nepal Stock Exchange.
