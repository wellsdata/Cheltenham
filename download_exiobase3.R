# Download EXIOBASE 3 from Zenodo
# Equivalent to pymrio.download_exiobase3()
#
# system: "pxp" (product x product) or "ixi" (industry x industry)
# years:  numeric vector e.g. c(2011, 2012), or NULL for all available years
# doi:    pin to a specific Zenodo version (recommended for reproducibility)
#
# Suggested citation: Stadler et al. (2018) https://doi.org/10.1111/jiec.12715

library(httr)
library(jsonlite)

# --- Configuration -----------------------------------------------------------

exio3_folder <- "~/mrios/EXIO3"   # change to your preferred storage path
system       <- "pxp"              # "pxp" or "ixi"
years        <- c(2011, 2012)      # set to NULL to download all years
doi          <- "10.5281/zenodo.5589597"  # latest as of 2023; pin for reproducibility

# --- Functions ---------------------------------------------------------------

# Resolve a Zenodo DOI to its record metadata (files list, etc.)
get_zenodo_record <- function(doi) {
  record_id <- sub("10.5281/zenodo.", "", doi, fixed = TRUE)
  url       <- paste0("https://zenodo.org/api/records/", record_id)
  resp      <- GET(url, add_headers(Accept = "application/json"))
  stop_for_status(resp)
  content(resp, as = "parsed", type = "application/json")
}

# Download EXIOBASE 3 files matching system/years from a given DOI
download_exiobase3 <- function(storage_folder,
                               system = "pxp",
                               years  = NULL,
                               doi    = "10.5281/zenodo.5589597") {

  dir.create(storage_folder, recursive = TRUE, showWarnings = FALSE)

  message("Fetching record metadata for DOI: ", doi)
  record <- get_zenodo_record(doi)

  # Build a data frame of available files
  files <- record$files
  file_df <- data.frame(
    filename = vapply(files, `[[`, character(1), "key"),
    url      = vapply(files, function(f) f$links$self, character(1)),
    size_mb  = vapply(files, function(f) f$size / 1e6, numeric(1)),
    stringsAsFactors = FALSE
  )

  # Filter to the requested system (pxp or ixi)
  pattern <- paste0("^IOT_\\d{4}_", system, "\\.zip$")
  file_df <- file_df[grepl(pattern, file_df$filename), ]

  # Filter to requested years (if specified)
  if (!is.null(years)) {
    year_pattern <- paste(years, collapse = "|")
    file_df <- file_df[grepl(year_pattern, file_df$filename), ]
  }

  if (nrow(file_df) == 0) {
    stop("No files matched system='", system, "' and years=",
         paste(years, collapse = ", "))
  }

  message(nrow(file_df), " file(s) to download:")
  message(paste0("  ", file_df$filename, " (", round(file_df$size_mb, 0), " MB)",
                 collapse = "\n"))

  # Download each file
  log_entries <- vector("list", nrow(file_df))

  for (i in seq_len(nrow(file_df))) {
    dest <- file.path(storage_folder, file_df$filename[i])

    if (file.exists(dest)) {
      message("\nSkipping (already exists): ", file_df$filename[i])
    } else {
      message("\nDownloading: ", file_df$filename[i],
              " (", round(file_df$size_mb[i], 0), " MB) ...")
      download.file(file_df$url[i], destfile = dest, mode = "wb")
      message("  Saved to: ", dest)
    }

    log_entries[[i]] <- list(
      timestamp = format(Sys.time(), "%Y%m%d %H:%M:%S"),
      action    = "FILEIO",
      message   = paste0("Downloaded ", file_df$url[i], " to ", file_df$filename[i])
    )
  }

  # Save download log (mirrors pymrio's download_log.json)
  log <- list(
    description = "Download log of EXIOBASE3",
    mrio_name   = "EXIO3",
    system      = system,
    version     = doi,
    r_version   = R.version$version.string,
    history     = log_entries
  )

  log_path <- file.path(storage_folder, "download_log.json")
  write_json(log, log_path, pretty = TRUE, auto_unbox = TRUE)
  message("\nDownload complete. Log saved to: ", log_path)

  invisible(log)
}

# --- Run ---------------------------------------------------------------------

download_log <- download_exiobase3(
  storage_folder = exio3_folder,
  system         = system,
  years          = years,
  doi            = doi
)

# Print log summary (mirrors print(exio_downloadlog) in Python)
cat("\nDescription:", download_log$description, "\n")
cat("MRIO Name:  ", download_log$mrio_name, "\n")
cat("System:     ", download_log$system, "\n")
cat("Version:    ", download_log$version, "\n")
cat("History:\n")
for (entry in download_log$history) {
  cat(" ", entry$timestamp, "-", entry$action, "- ", entry$message, "\n")
}
