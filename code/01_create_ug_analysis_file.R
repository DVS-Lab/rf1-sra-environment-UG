# Build the fixed 2026-10-26 UGR analysis cohort.
# All participant-level outputs from this stage are private because address and
# ZIP are still required for environmental linkage.

required_packages <- c("writexl")
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

project <- "UG"
project_label <- "UGR"
analysis_date <- "2026-10-26"
expected_roster_n <- 145L
private_root <- "D:/Postdoc/Paper_brainhealth_environment/private data"
private_dir <- file.path(private_root, "intermediate", paste0(project, "_", analysis_date))
screener_path <- file.path(
  private_root, "raw",
  "IslandScreener-JacobSophieKatieData_DATA_2026-10-26_1247.csv"
)
roster_path <- file.path(private_root, "inputs", "subjects_ugr_2026-10-26.txt")
output_path <- file.path(private_dir, paste0(project, "_cohort_private_", analysis_date, ".xlsx"))
summary_path <- file.path(private_dir, paste0(project, "_cohort_exclusion_counts_", analysis_date, ".csv"))
details_path <- file.path(private_dir, paste0(project, "_cohort_exclusion_details_", analysis_date, ".csv"))
upload_path <- file.path(private_dir, paste0(project, "_geocodio_upload_", analysis_date, ".xlsx"))

for (path in c(screener_path, roster_path)) {
  if (!file.exists(path)) stop("Required private input not found: ", path)
}
dir.create(private_dir, recursive = TRUE, showWarnings = FALSE)

read_all_character <- function(path) {
  data <- utils::read.csv(
    path, colClasses = "character", check.names = FALSE,
    na.strings = c("", "NA", "N/A"), strip.white = FALSE
  )
  for (column in names(data)) {
    data[[column]] <- trimws(data[[column]])
    data[[column]][data[[column]] == ""] <- NA_character_
  }
  data
}

assert_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(label, " is missing column(s): ", paste(missing, collapse = ", "))
  }
}

is_blank <- function(x) is.na(x) | trimws(as.character(x)) == ""

as_numeric_checked <- function(values, column) {
  converted <- suppressWarnings(as.numeric(values))
  invalid <- !is.na(values) & is.na(converted)
  if (any(invalid)) stop("Nonnumeric value in ", column)
  converted
}

parse_date <- function(values) {
  parsed <- rep(as.Date(NA), length(values))
  for (format_string in c("%m/%d/%Y", "%Y-%m-%d")) {
    use <- is.na(parsed) & !is.na(values)
    parsed[use] <- as.Date(values[use], format = format_string)
  }
  parsed
}

completed_age <- function(dob, reference_date) {
  age <- as.integer(format(reference_date, "%Y")) - as.integer(format(dob, "%Y"))
  age - as.integer(format(reference_date, "%m%d") < format(dob, "%m%d"))
}

read_roster <- function(path) {
  values <- trimws(readLines(path, warn = FALSE))
  values <- sub("^sub-", "", values[values != "" & !startsWith(values, "#")])
  if (length(values) != expected_roster_n || anyDuplicated(values) ||
      any(!grepl("^[0-9]+$", values))) {
    stop("Validated ", project_label, " roster must contain exactly ",
         expected_roster_n, " unique numeric IDs")
  }
  values
}

data <- read_all_character(screener_path)
assert_columns(
  data,
  c("rf1_id", "ecog_score", "mspss_sum", "address", "zip_code",
    "dob", "screener_date", "gender"),
  "Fixed screener export"
)
data$.source_row <- seq_len(nrow(data))
data$rf1_id_source <- trimws(data$rf1_id)
data$rf1_id <- sub("_duplicate$", "", data$rf1_id_source, ignore.case = TRUE)
data$.duplicate_suffix <- grepl("_duplicate$", data$rf1_id_source, ignore.case = TRUE)

valid_id <- !is_blank(data$rf1_id_source) & grepl("^[0-9]+$", data$rf1_id)
invalid_id_n <- sum(!valid_id)
data <- data[valid_id, , drop = FALSE]
roster <- read_roster(roster_path)
missing_screener <- setdiff(roster, unique(data$rf1_id))
data <- data[data$rf1_id %in% roster, , drop = FALSE]

dob <- parse_date(data$dob)
screener_date <- parse_date(data$screener_date)
data$sub_age <- completed_age(dob, screener_date)
data$sub_age[data$sub_age < 0 | data$sub_age > 120] <- NA_integer_
data$ecog_score <- as_numeric_checked(data$ecog_score, "ecog_score")
data$mspss_sum <- as_numeric_checked(data$mspss_sum, "mspss_sum")

flags <- list(
  missing_sub_age_analysis_variable = is.na(data$sub_age),
  missing_ecog_score = is.na(data$ecog_score),
  missing_mspss_sum = is.na(data$mspss_sum),
  missing_address = is_blank(data$address),
  missing_zip_code = is_blank(data$zip_code),
  missing_gender = is_blank(data$gender)
)
active <- rep(TRUE, nrow(data))
summary_rows <- list()
detail_rows <- list()
for (index in seq_along(flags)) {
  flag <- active & flags[[index]]
  before_ids <- unique(data$rf1_id[active])
  after <- active & !flags[[index]]
  after_ids <- unique(data$rf1_id[after])
  removed_ids <- setdiff(before_ids, after_ids)
  summary_rows[[index]] <- data.frame(
    project = project, step = index + 1L, criterion = names(flags)[index],
    rows_excluded = sum(flag), participants_excluded = length(removed_ids),
    participants_remaining = length(after_ids), stringsAsFactors = FALSE
  )
  if (any(flag)) {
    detail_rows[[length(detail_rows) + 1L]] <- data.frame(
      project = project, step = index + 1L, criterion = names(flags)[index],
      source_row = data$.source_row[flag], rf1_id = data$rf1_id[flag],
      participant_excluded = data$rf1_id[flag] %in% removed_ids,
      stringsAsFactors = FALSE
    )
  }
  active <- after
}
data <- data[active, , drop = FALSE]

# Prefer the unsuffixed REDCap row and then the first source row.
ordered <- order(data$rf1_id, data$.duplicate_suffix, data$.source_row)
drop_duplicate <- rep(FALSE, nrow(data))
drop_duplicate[ordered[duplicated(data$rf1_id[ordered])]] <- TRUE
if (any(drop_duplicate)) {
  detail_rows[[length(detail_rows) + 1L]] <- data.frame(
    project = project, step = length(flags) + 2L,
    criterion = "nonpreferred_eligible_duplicate_rf1_row",
    source_row = data$.source_row[drop_duplicate], rf1_id = data$rf1_id[drop_duplicate],
    participant_excluded = FALSE, stringsAsFactors = FALSE
  )
}
data <- data[!drop_duplicate, , drop = FALSE]
data <- data[order(data$.source_row), , drop = FALSE]
if (anyDuplicated(data$rf1_id)) stop("Final private cohort contains duplicate rf1_id values")

private_columns <- c(
  "rf1_id", "sub_age", "gender", "ecog_score", "mspss_sum", "address", "zip_code"
)
private_data <- data[private_columns]
geocodio_upload <- data.frame(
  source_row = seq.int(2L, nrow(private_data) + 1L),
  street = private_data$address,
  postal_code = private_data$zip_code,
  country = "US",
  input_review = NA_character_,
  stringsAsFactors = FALSE
)

summary <- do.call(rbind, c(list(data.frame(
  project = project, step = 1L, criterion = "imaging_pool_id_without_valid_screener_record",
  rows_excluded = length(missing_screener), participants_excluded = length(missing_screener),
  participants_remaining = length(unique(data$rf1_id)), stringsAsFactors = FALSE
)), summary_rows, list(data.frame(
  project = project, step = length(flags) + 2L,
  criterion = "nonpreferred_eligible_duplicate_rf1_row",
  rows_excluded = sum(drop_duplicate), participants_excluded = 0L,
  participants_remaining = nrow(private_data), stringsAsFactors = FALSE
))))
details <- if (length(detail_rows) == 0L) {
  data.frame(project = character(), step = integer(), criterion = character(),
             source_row = integer(), rf1_id = character(),
             participant_excluded = logical())
} else do.call(rbind, detail_rows)

writexl::write_xlsx(list(Data = private_data), output_path)
writexl::write_xlsx(list(Data = geocodio_upload), upload_path)
utils::write.csv(summary, summary_path, row.names = FALSE, na = "")
utils::write.csv(details, details_path, row.names = FALSE, na = "")

cat(project_label, "private cohort complete\n")
cat("Invalid/blank source IDs:", invalid_id_n, "\n")
cat("Validated roster IDs missing a valid screener record:", length(missing_screener), "\n")
cat("Private cohort participants:", nrow(private_data), "\n")
cat("Private workbook:", normalizePath(output_path, winslash = "/"), "\n")
