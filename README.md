# Load and recovery monitoring

**[Open the app](https://connect.posit.cloud/michael-davies-personal/content/01a1595f-e7f0-b63f-d2cf-a1ced13e3f21)** · [Portfolio write-up](https://michaeldavies64.github.io/projects/products/load-monitoring/)

A self-serve Shiny app that answers one question quickly: is this player's load where it should be for this point in the match week? Every session and recovery score is compared with a normal range learned from the player's own history for that kind of day, so unusual sessions, and weeks with several of them, stand out.

Built in R on the [CFC Insights Vizathon 2025](https://github.com/Chelsea-Fc-Performance-Insights/Competition) data: simulated data for one player, from August 2022 to April 2025.

> The app is hosted on Posit Connect Cloud. After a quiet spell it takes about 10 seconds to wake up; the welcome page shows while it does.

## What's in the app

| Page | What it answers |
|---|---|
| Overview | How many metrics are outside their normal range today, the latest session and the last 28 days of load |
| Profile | Who the player is, the current status of every metric, and what each type of day typically looks like |
| Longitudinal | Load over any period: daily, against the adaptive range, or as rolling 7- and 28-day load, with sessions outside their range marked |
| Session check | One session against its normal ranges, against the last 10 sessions of the same type, and the recovery scores that followed |
| Glossary | What every metric means and how the app uses it |

## How it's built

```
raw CSVs ──> staging ──> features ──> monitoring ──> Shiny app
             (DuckDB schemas; every transformation in SQL or R, rebuilt from the raw files)
```

| Stage | Code | Write-up |
|---|---|---|
| Download and read the raw files, with explicit types | `utils/download_data.R`, `utils/read_data.R` | |
| Data quality checks | `utils/quality_checks.R` | [Data quality report](https://michaeldavies64.github.io/load-recovery-monitoring/data_quality.html) |
| Staging: the files as received, plus reference tables | `code/load_staging.R` | |
| Features: match-week labels, full-match reference, per-minute and heart-rate band metrics | `queries/*.sql`, `code/build_features.R` | [Feature engineering report](https://michaeldavies64.github.io/load-recovery-monitoring/feature_engineering.html) |
| Monitoring: adaptive ranges for every metric and day type | `utils/adaptive_ranges.R`, `code/build_monitoring.R` | [Adaptive ranges report](https://michaeldavies64.github.io/load-recovery-monitoring/adaptive_ranges.html) |
| App | `app/` (`ui`, `server`, `data_visualisation`, `data_management`) | |

The reports are rendered from `docs/*.qmd` and served from the `docs` folder with GitHub Pages.

The adaptive ranges follow Roshan et al. (2021), *A comparison of methods to generate adaptive reference ranges in longitudinal monitoring*, PLOS ONE, adapted for a single player.

## Run it locally

```r
renv::restore()
source("scr/01_download_data.R")      # raw files from the competition repository
source("scr/02_load_staging.R")
source("scr/03_build_features.R")
source("scr/04_build_monitoring.R")
shiny::runApp("app")
```

The app also builds the database itself if it is missing, which is how the hosted version starts.

## Data

The raw Vizathon files are downloaded from Chelsea FC Performance Insights' public competition repository when the pipeline runs; they are not stored or re-hosted here. The player and all values are simulated.
