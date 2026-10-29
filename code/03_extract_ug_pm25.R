# Extract fixed 2013-2023 annual PM2.5 values for UGR and publish only
# the seven explicitly approved analysis fields. Coordinates and annual values
# are retained exclusively in private data.

required_packages <- c("readxl", "writexl")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)
)]
if (length(missing_packages) > 0L) {
  stop("Missing R package(s): ", paste(missing_packages, collapse = ", "))
}

command_args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", command_args, value = TRUE)
script_dir <- if (length(script_arg) == 1L) {
  dirname(normalizePath(sub("^--file=", "", script_arg), winslash = "/"))
} else {
  normalizePath(getwd(), winslash = "/")
}
repo_root <- normalizePath(file.path(script_dir, ".."), winslash = "/")

project <- "UG"
project_label <- "UGR"
analysis_date <- "2026-10-26"
years <- 2013:2023
private_root <- "D:/Postdoc/Paper_brainhealth_environment/private data"
private_dir <- file.path(private_root, "intermediate", paste0(project, "_", analysis_date))
input_path <- file.path(private_dir, paste0(project, "_adi_private_", analysis_date, ".xlsx"))
raster_dir <- file.path(private_root, "reference", "pm-download", "Annual", "Annual")
private_qc_path <- file.path(private_dir, paste0(project, "_pm25_private_qc_2013_2023_", analysis_date, ".csv"))
private_exclusion_path <- file.path(private_dir, paste0(project, "_pm25_exclusions_2013_2023_", analysis_date, ".csv"))
public_dir <- file.path(repo_root, "data", "derived")
public_output <- file.path(public_dir, paste0(project, "_analysis_data_", analysis_date, ".xlsx"))
public_summary <- file.path(public_dir, paste0(project, "_pipeline_summary_", analysis_date, ".csv"))
approved_columns <- c(
  "rf1_id", "sub_age", "gender", "ecog_score", "mspss_sum",
  "adi_natrank", "pm25_mean_2013_2023"
)

if (!file.exists(input_path)) stop("Required private ADI workbook not found: ", input_path)
if (!dir.exists(raster_dir)) stop("PM2.5 raster directory not found: ", raster_dir)
dir.create(public_dir, recursive = TRUE, showWarnings = FALSE)

assert_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) stop(label, " is missing: ", paste(missing, collapse = ", "))
}

find_raster_archives <- function(directory, target_years) {
  paths <- list.files(directory, pattern = "\\.asc\\.zip$", full.names = TRUE, ignore.case = TRUE)
  names_only <- basename(paths)
  valid <- grepl("\\.([0-9]{4})001-[0-9]{7}\\.asc\\.zip$", names_only)
  archive_year <- suppressWarnings(as.integer(ifelse(
    valid,
    sub(".*\\.([0-9]{4})001-[0-9]{7}\\.asc\\.zip$", "\\1", names_only),
    NA_character_
  )))
  selected <- paths[archive_year %in% target_years]
  selected_year <- archive_year[archive_year %in% target_years]
  counts <- table(factor(selected_year, levels = target_years))
  if (any(counts != 1L)) {
    stop("Expected exactly one PM2.5 archive for year(s): ",
         paste(target_years[counts != 1L], collapse = ", "))
  }
  result <- selected[match(target_years, selected_year)]
  names(result) <- as.character(target_years)
  result
}

parse_ascii_header <- function(lines, label) {
  if (length(lines) != 6L) stop("Invalid ASCII raster header: ", label)
  pieces <- strsplit(trimws(lines), "[[:space:]]+")
  keys <- tolower(vapply(pieces, `[[`, character(1), 1L))
  values <- suppressWarnings(as.numeric(vapply(
    pieces, function(x) if (length(x) >= 2L) x[[2L]] else NA_character_, character(1)
  )))
  names(values) <- keys
  required <- c("ncols", "nrows", "cellsize", "nodata_value")
  if (any(!required %in% names(values))) stop("Incomplete ASCII header: ", label)
  x_first_center <- if ("xllcenter" %in% names(values)) values[["xllcenter"]] else
    values[["xllcorner"]] + values[["cellsize"]] / 2
  y_first_center <- if ("yllcenter" %in% names(values)) values[["yllcenter"]] else
    values[["yllcorner"]] + values[["cellsize"]] / 2
  list(
    ncols = as.integer(values[["ncols"]]), nrows = as.integer(values[["nrows"]]),
    x_first_center = x_first_center, y_first_center = y_first_center,
    cellsize = values[["cellsize"]], nodata = values[["nodata_value"]]
  )
}

grid_signature <- function(header) paste(
  header$ncols, header$nrows, format(header$x_first_center, digits = 15),
  format(header$y_first_center, digits = 15), format(header$cellsize, digits = 15),
  sep = "|"
)

skip_ascii_rows <- function(connection, count, label) {
  remaining <- count
  while (remaining > 0L) {
    chunk <- min(25L, remaining)
    if (length(readLines(connection, n = chunk, warn = FALSE)) != chunk) {
      stop("Unexpected raster end: ", label)
    }
    remaining <- remaining - chunk
  }
}

extract_one_archive <- function(path, longitude, latitude, reference_grid = NULL) {
  listing <- utils::unzip(path, list = TRUE)
  asc_name <- listing$Name[grepl("\\.asc$", listing$Name, ignore.case = TRUE)]
  prj_name <- listing$Name[grepl("\\.prj$", listing$Name, ignore.case = TRUE)]
  if (length(asc_name) != 1L || length(prj_name) != 1L) {
    stop("Expected one ASC and one PRJ in ", basename(path))
  }
  prj_connection <- unz(path, prj_name, open = "rt")
  prj <- paste(readLines(prj_connection, warn = FALSE), collapse = " ")
  close(prj_connection)
  if (!grepl("WGS84", prj, ignore.case = TRUE) || !grepl("GEOGRAPHIC", prj, ignore.case = TRUE)) {
    stop("Unexpected raster projection: ", basename(path))
  }

  connection <- unz(path, asc_name, open = "rt")
  on.exit(close(connection), add = TRUE)
  header <- parse_ascii_header(readLines(connection, n = 6L, warn = FALSE), basename(path))
  if (!is.null(reference_grid) && grid_signature(header) != grid_signature(reference_grid)) {
    stop("PM2.5 raster grids differ: ", basename(path))
  }
  x_left <- header$x_first_center - header$cellsize / 2
  y_bottom <- header$y_first_center - header$cellsize / 2
  column_index <- floor((longitude - x_left) / header$cellsize) + 1L
  bottom_row <- floor((latitude - y_bottom) / header$cellsize) + 1L
  file_row <- header$nrows - bottom_row + 1L
  outside <- is.na(column_index) | is.na(file_row) | column_index < 1L |
    column_index > header$ncols | file_row < 1L | file_row > header$nrows
  if (any(outside)) stop("Participant coordinate falls outside ", basename(path))

  output <- rep(NA_real_, length(longitude))
  current_row <- 0L
  for (target_row in sort(unique(file_row))) {
    skip_ascii_rows(connection, target_row - current_row - 1L, basename(path))
    raster_line <- readLines(connection, n = 1L, warn = FALSE)
    row_values <- scan(text = raster_line, what = double(), quiet = TRUE, nmax = header$ncols)
    if (length(row_values) != header$ncols) stop("Malformed raster row in ", basename(path))
    participant_index <- which(file_row == target_row)
    output[participant_index] <- row_values[column_index[participant_index]]
    current_row <- target_row
  }
  output[is.finite(output) & abs(output - header$nodata) < 1e-8] <- NA_real_
  list(values = output, header = header)
}

data <- as.data.frame(readxl::read_excel(input_path, sheet = "Data"))
assert_columns(
  data,
  c(approved_columns[approved_columns != "pm25_mean_2013_2023"],
    "pm25_longitude", "pm25_latitude", "pm25_coordinate_source"),
  "Private ADI workbook"
)
if (anyDuplicated(data$rf1_id)) stop("Private ADI workbook contains duplicate rf1_id values")
longitude <- suppressWarnings(as.numeric(data$pm25_longitude))
latitude <- suppressWarnings(as.numeric(data$pm25_latitude))
if (anyNA(longitude) || anyNA(latitude) || any(abs(longitude) > 180) || any(abs(latitude) > 90)) {
  stop("Private ADI workbook contains missing or invalid coordinates")
}

archives <- find_raster_archives(raster_dir, years)
pm25 <- matrix(NA_real_, nrow = nrow(data), ncol = length(years),
               dimnames = list(NULL, paste0("pm25_", years)))
reference_grid <- NULL
for (year in years) {
  message("Extracting ", year, " PM2.5")
  extracted <- extract_one_archive(
    archives[[as.character(year)]], longitude, latitude, reference_grid
  )
  if (is.null(reference_grid)) reference_grid <- extracted$header
  pm25[, paste0("pm25_", year)] <- extracted$values
}

year_count <- rowSums(!is.na(pm25))
complete <- year_count == length(years)
mean_pm25 <- rep(NA_real_, nrow(data))
mean_pm25[complete] <- round(rowMeans(pm25[complete, , drop = FALSE]), 4)
data$pm25_mean_2013_2023 <- mean_pm25

private_qc <- data.frame(
  rf1_id = data$rf1_id, longitude = longitude, latitude = latitude,
  coordinate_source = data$pm25_coordinate_source, pm25,
  pm25_mean_2013_2023 = mean_pm25, pm25_years_available = year_count,
  pm25_complete_2013_2023 = complete, stringsAsFactors = FALSE,
  check.names = FALSE
)
exclusions <- data.frame(
  rf1_id = data$rf1_id[!complete],
  pm25_years_available = year_count[!complete],
  exclusion_reason = ifelse(year_count[!complete] == 0L, "missing_all_years", "missing_one_or_more_years"),
  stringsAsFactors = FALSE
)
public_data <- data[complete, approved_columns, drop = FALSE]

# Hard privacy assertion: no unapproved field can be written to the repository.
if (!identical(names(public_data), approved_columns)) stop("Public column allowlist check failed")
for (forbidden in c("address", "zip", "dob", "latitude", "longitude", "gisjoin", "coordinate")) {
  if (any(grepl(forbidden, names(public_data), ignore.case = TRUE))) {
    stop("Forbidden public column detected: ", forbidden)
  }
}
if (any(!complete.cases(public_data))) stop("Public analysis data contain missing approved fields")

utils::write.csv(private_qc, private_qc_path, row.names = FALSE, na = "")
utils::write.csv(exclusions, private_exclusion_path, row.names = FALSE, na = "")
writexl::write_xlsx(list(Data = public_data), public_output)
utils::write.csv(data.frame(
  project = project, analysis_date = analysis_date,
  participants_entering_pm25 = nrow(data),
  participants_published = nrow(public_data),
  participants_excluded_incomplete_pm25 = nrow(exclusions),
  approved_columns = paste(approved_columns, collapse = ";"),
  stringsAsFactors = FALSE
), public_summary, row.names = FALSE, na = "")

cat(project_label, "PM2.5 extraction complete\n")
cat("Published participants:", nrow(public_data), "\n")
cat("Public workbook:", normalizePath(public_output, winslash = "/"), "\n")
cat("Private coordinate/QC audit:", normalizePath(private_qc_path, winslash = "/"), "\n")
