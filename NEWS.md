# lcNEPSE 0.1.0

- First release.
- `nepse_prices()`, `nepse_indices()`, `nepse_symbols()`, `nepse_market()`,
  `nepse_dates()`, and `nepse_latest()` read hash-verified monthly partitions
  from the `data-archive` branch.
- `nepse_catalog()`, `nepse_search()`, `nepse_metadata()`, `nepse_read_raw()`,
  and `nepse_update()` describe and inspect the archive.
- `nepse_index()` exposes the validated manifest; `nepse_cache_info()` and
  `nepse_clear_cache()` manage the runtime cache.
- `nepse_scrape_prices()`, `nepse_scrape_indices()`, and
  `nepse_scrape_index_history()` read ShareSansar directly. Historical share
  prices go through the site's AJAX date filter, which
  omits the 120-day and 180-day columns; those fields are recorded as missing
  for back-filled days rather than invented.
- The index archive now includes all published NEPSE, Sensitive, Float,
  Sensitive Float, and sector/sub-index history. Each series keeps its actual
  source coverage; unavailable historical 52-week fields remain missing.
- The scheduled workflow archives each trading day, preserves revised days as
  versioned files, and publishes only after the archive passes a full
  read-through with an empty cache.
