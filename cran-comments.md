## R CMD check results

0 errors | 0 warnings | 0 notes

Checked with R 4.5.2 on Windows 11, and on Ubuntu and macOS via GitHub Actions.

On a machine without the `qpdf` binary, `--as-cran` adds the standard
environmental warning about PDF size checks. `qpdf` is present on CRAN and on
the GitHub Actions runners, where the check is clean.

## Submission

This is a first submission.

## Notes for reviewers

- `lcNEPSE` is a client for a data archive published on the `data-archive`
  branch of <https://github.com/koshishjlc72/lcNEPSE>. Nothing is downloaded at
  install time. Network access happens only when a user calls a data function,
  and every request is a plain HTTPS `GET` of a file whose SHA-256 is published
  in the archive `manifest.csv`; a mismatch is an error.
- Tests and the vignette run fully offline. The scraping pipeline lives in
  `data-raw/`, is excluded from the built package, and its test skips when that
  directory is absent.
- The data are published by ShareSansar (Nepal). The package is independent and
  is not affiliated with or endorsed by ShareSansar or the Nepal Stock
  Exchange; this is stated in the Description, the README, and `LICENSE.md`.
- `Conf.` is reproduced exactly as published; the publisher does not define it,
  so the package does not interpret it.
- Historical sessions fetched through the site's date filter do not carry the
  120-day and 180-day reference columns; they are stored as missing rather than
  imputed.
