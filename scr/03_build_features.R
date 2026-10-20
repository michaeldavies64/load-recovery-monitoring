# Step 3: build the features tables in data/load_recovery.duckdb from the staging tables
# Run from the project root: Rscript scr/03_build_features.R
for (f in c(list.files(here::here("utils"), pattern = "\\.R$", full.names = TRUE), here::here("code", "build_features.R"))) {
  source(f, encoding = "UTF-8")
}

counts <- with_db(build_features)
message(paste0(counts$table, ": ", format(counts$rows, big.mark = ","), " rows", collapse = "\n"))
