# CRAN submission instructions — lcNEPSE

`lcNEPSE` already implements demand-driven GitHub data access. The CRAN package
contains client code; ShareSansar price and index snapshots stay on the separate
`data-archive` branch. No downloads occur at package installation or load time.

## 1. Build and verify

1. Commit the new workflow to the repository's `main` branch. On that push,
   **Actions > Prepare CRAN submission** runs automatically. You may also
   choose **Run workflow** on the Actions page.
2. Confirm that **both** `release-tarball` and `r-devel` pass. They build
   the source archive and check with `R CMD check --as-cran --no-manual`.
3. Confirm the existing **R-CMD-check** workflow succeeds on Windows, Ubuntu,
   and macOS for the *same commit*.
4. Download the `lcNEPSE-cran-source` artifact from the successful Actions run.
   Extract its outer `.zip` and locate the **`lcNEPSE_<version>.tar.gz`** source
   archive. Do not submit the GitHub artifact `.zip` itself.

The release workflow is intended to prepare an archive for human submission.
It intentionally does **not** submit to CRAN automatically.

## 2. CRAN review checklist

Read the [CRAN repository policy](https://CRAN.R-project.org/web/packages/policies.html)
and the [submission checklist](https://CRAN.R-project.org/web/packages/submission_checklist.html).
Before submitting, ensure the package Description, contact details, license,
examples, documentation, and provider attribution are accurate, and verify that
there are no check ERRORs, WARNINGs, or NOTEs requiring explanation. Confirm
that the use of data complies with ShareSansar's applicable terms. Update
`cran-comments.md` with the actual results from the release being submitted.

A local check is also possible if R is installed:

```sh
R CMD build .
R CMD check --as-cran --no-manual lcNEPSE_<version>.tar.gz
```

## 3. Submit and accept

As maintainer, open <https://CRAN.R-project.org/submit.html> and upload the
`.tar.gz` file, then **confirm the email** sent to the maintainer address in
`DESCRIPTION`. CRAN reviews the submission and may ask for changes.

Only **after acceptance** can others install directly from CRAN:

```r
install.packages("lcNEPSE")
library(lcNEPSE)
nepse_prices("NABIL", start = "2026-01-01", refresh = TRUE)
```

Until then, install the package from GitHub:

```r
install.packages("remotes")
remotes::install_github("koshishjlc72/lcNEPSE")
library(lcNEPSE)
```

## 4. Live GitHub archive access

```r
nepse_catalog()  # download manifest on demand
nepse_symbols()  # download symbol master on demand
nepse_prices("NABIL", start = "2025-01-01", end = "2025-12-31")
nepse_indices(start = "2026-10-01")
nepse_latest(refresh = TRUE)  # bypass 15-minute manifest cache
nepse_cache_info()
```

Each required monthly archive partition comes directly from
`https://raw.githubusercontent.com/koshishjlc72/lcNEPSE/data-archive/`,
with SHA-256 verification before use. Repeated queries reuse cached partitions.
Only an explicit data request contacts GitHub; the offline test suite does not.
