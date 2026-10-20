# Step 4: build the monitoring tables (adaptive ranges) in data/load_recovery.duckdb
# Run from the project root after step 3: Rscript scr/04_build_monitoring.R
suppressPackageStartupMessages(library(dplyr))
for (f in c(list.files(here::here("utils"), pattern = "\\.R$", full.names = TRUE), here::here("code", "build_monitoring.R"))) {
  source(f, encoding = "UTF-8")
}

counts <- with_db(build_monitoring)
message(paste0(counts$table, ": ", format(counts$rows, big.mark = ","), " rows", collapse = "\n"))
