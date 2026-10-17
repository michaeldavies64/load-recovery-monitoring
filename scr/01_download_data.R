# Step 1: download the raw Vizathon files into input/
# Run from the project root: Rscript scr/01_download_data.R
source("utils/download_data.R")

result <- download_vizathon_data(dest = "input")

for (status in c("downloaded", "already present")) {
  hit <- result$file[result$status == status]
  if (length(hit)) message(sub("^(.)", "\\U\\1", status, perl = TRUE), ": ", paste(hit, collapse = ", "))
}

failed <- result[result$status == "failed", ]
if (nrow(failed)) {
  stop("Failed to download: ",
       paste0(failed$file, " (", failed$detail, ")", collapse = "; "),
       call. = FALSE)
}
