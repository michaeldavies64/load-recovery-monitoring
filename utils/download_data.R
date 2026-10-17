# Downloads the CFC Insights Vizathon 2025 data from Chelsea's public repository.
# The raw files are not stored in this repository; run scr/01_download_data.R to fetch them.

vizathon_files <- c(
  gps        = "CFC GPS Data.csv",
  capability = "CFC Physical Capability Data_.csv",
  recovery   = "CFC Recovery status Data.csv",
  priorities = "CFC Individual Priority Areas.csv"
)

vizathon_base_url <- "https://raw.githubusercontent.com/Chelsea-Fc-Performance-Insights/Competition/main/DATA/"

# Returns a tibble with one row per file and what happened to it:
# "downloaded", "already present" or "failed" (with the error message).
# Each file is downloaded to a temporary file first and only moved into place once
# it has arrived in full, so a failed download never leaves a partial file behind.
download_vizathon_data <- function(dest = "input", overwrite = FALSE) {
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)

  results <- lapply(names(vizathon_files), function(key) {
    f <- vizathon_files[[key]]
    path <- file.path(dest, f)
    if (file.exists(path) && !overwrite) {
      return(data.frame(file = key, status = "already present", detail = NA_character_))
    }

    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    url <- paste0(vizathon_base_url, utils::URLencode(f))
    outcome <- tryCatch({
      suppressWarnings(utils::download.file(url, tmp, mode = "wb", quiet = TRUE))
      if (file.size(tmp) == 0) stop("downloaded file is empty")
      if (!file.copy(tmp, path, overwrite = TRUE)) stop("could not write to ", path)
      NULL
    }, error = function(e) conditionMessage(e))

    if (is.null(outcome)) {
      data.frame(file = key, status = "downloaded", detail = NA_character_)
    } else {
      data.frame(file = key, status = "failed", detail = outcome)
    }
  })

  tibble::as_tibble(do.call(rbind, results))
}
