<#
.SYNOPSIS
  Backfill every historical ShareSansar trading session into the lcNEPSE
  GitHub archive.

.DESCRIPTION
  Dispatches the "Update ShareSansar data archive" workflow once per year.
  Each run scrapes ShareSansar, validates the rows, rebuilds the monthly
  partitions, verifies the whole archive, and commits to the `data-archive`
  branch. Nothing is pushed unless that year passes verification.

  Safe to re-run: a session that is already archived is detected by content
  hash and is not duplicated. Weekend and holiday dates are skipped by the
  workflow itself.

  Requires the GitHub CLI (gh), authenticated for the repository owner:
      gh auth login
      gh auth status

.EXAMPLE
  pwsh -NoProfile -ExecutionPolicy Bypass -File backfill-all.ps1

.EXAMPLE
  # try data older than 2014, or rebuild a single year
  pwsh -NoProfile -ExecutionPolicy Bypass -File backfill-all.ps1 -FromYear 2010 -ToYear 2013
#>
param(
  [int]$FromYear = 2014,
  [int]$ToYear = (Get-Date).Year,
  [string]$Repo = "koshishjlc72/lcNEPSE",
  [string]$Workflow = "update-archive.yml"
)

$ErrorActionPreference = "Stop"

# --- Preconditions ---------------------------------------------------------
if (-not (Get-Command gh -ErrorAction SilentlyContinue)) {
  throw "GitHub CLI (gh) is not installed: https://cli.github.com/"
}
gh auth status *> $null
if ($LASTEXITCODE -ne 0) {
  throw "gh is not authenticated. Run: gh auth login"
}

$logDir = Join-Path $env:TEMP "lcNEPSE-backfill"
New-Item -ItemType Directory -Force -Path $logDir | Out-Null
$logFile = Join-Path $logDir ("backfill-" + (Get-Date -Format "yyyyMMdd-HHmmss") + ".log")

function Log([string]$Message) {
  $line = "[{0}] {1}" -f (Get-Date -Format "HH:mm:ss"), $Message
  Write-Host $line
  Add-Content -Path $logFile -Value $line
}

function Get-LatestRunId {
  gh run list --repo $Repo --workflow $Workflow --limit 1 --json databaseId --jq '.[0].databaseId'
}

function Wait-Run([string]$Id) {
  while ($true) {
    $run = gh run view $Id --repo $Repo --json status,conclusion | ConvertFrom-Json
    if ($run.status -eq "completed") { return $run.conclusion }
    Start-Sleep -Seconds 20
  }
}

# --- Backfill --------------------------------------------------------------
Log "Repository : $Repo"
Log "Range      : $FromYear .. $ToYear"
Log "Log file   : $logFile"

$results = @()
for ($year = $FromYear; $year -le $ToYear; $year++) {
  $from = "{0}-01-01" -f $year
  $to = if ($year -eq (Get-Date).Year) {
    (Get-Date).ToString("yyyy-MM-dd")
  } else {
    "{0}-12-31" -f $year
  }
  Log "=== $from .. $to ==="

  $previous = Get-LatestRunId

  $dispatchOk = $false
  for ($attempt = 1; $attempt -le 3; $attempt++) {
    gh workflow run $Workflow --repo $Repo -f "backfill_from=$from" -f "backfill_to=$to"
    if ($LASTEXITCODE -eq 0) { $dispatchOk = $true; break }
    Log "dispatch failed (attempt $attempt); retrying"
    Start-Sleep -Seconds (30 * $attempt)
  }
  if (-not $dispatchOk) {
    $results += [pscustomobject]@{ Year = $year; Run = ""; Result = "dispatch failed" }
    Log "$year => dispatch failed"
    continue
  }

  # Pick the run that was just queued (a new id, not the previous one).
  $id = $null
  for ($i = 0; $i -lt 30; $i++) {
    Start-Sleep -Seconds 5
    $candidate = Get-LatestRunId
    if ($candidate -and $candidate -ne $previous) { $id = $candidate; break }
  }
  if (-not $id) {
    $results += [pscustomobject]@{ Year = $year; Run = ""; Result = "run not found" }
    Log "$year => run not found (open the Actions tab to check)"
    continue
  }

  Log "run $id started; waiting (about 10-12 minutes per year)"
  $conclusion = Wait-Run $id
  Log "$year => $conclusion"
  $results += [pscustomobject]@{ Year = $year; Run = $id; Result = $conclusion }
}

# --- Summary ---------------------------------------------------------------
Log "Finished. Log file: $logFile"
$results | Format-Table -AutoSize | Out-String | Write-Host

$failed = @($results | Where-Object { $_.Result -ne "success" })
if ($failed.Count -gt 0) {
  Write-Host "These years did not publish:" -ForegroundColor Yellow
  $failed | Format-Table -AutoSize | Out-String | Write-Host
  Write-Host "Re-run the same command for just those years, e.g.:" -ForegroundColor Yellow
  Write-Host ("  pwsh -NoProfile -ExecutionPolicy Bypass -File {0} -FromYear {1} -ToYear {1}" -f $PSCommandPath, $failed[0].Year)
}
