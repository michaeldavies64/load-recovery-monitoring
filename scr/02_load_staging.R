# Step 2: load the raw files into the staging schema of data/load_recovery.duckdb
# Run from the project root: Rscript scr/02_load_staging.R
for (f in c(list.files("utils", pattern = "\\.R$", full.names = TRUE), "code/load_staging.R")) {
  source(f, encoding = "UTF-8")
}

counts <- with_db(load_staging)
message(paste0(counts$table, ": ", format(counts$rows, big.mark = ","), " rows", collapse = "\n"))
