# Link the fixed UGR cohort to 2024 Neighborhood Atlas ADI.
# Address, ZIP, ZIP9, GISJOIN, and coordinates remain in private data only.

required_packages <- c("readxl", "writexl", "sf")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)
)]
if (length(missing_packages) > 0L) {
  stop("Missing R package(s): ", paste(missing_packages, collapse = ", "))
}

project <- "UG"
project_label <- "UGR"
analysis_date <- "2026-10-26"
private_root <- "D:/Postdoc/Paper_brainhealth_environment/private data"
private_dir <- file.path(private_root, "intermediate", paste0(project, "_", analysis_date))
input_path <- file.path(private_dir, paste0(project, "_cohort_private_", analysis_date, ".xlsx"))
geocodio_path <- file.path(private_root, "inputs", "geocodio_download_118_cohort_2026-10-26.csv")
review_path <- file.path(private_root, "inputs", "adi_regeocode_review_2026-10-16.csv")
adi_root <- file.path(private_root, "reference", "adi-download")
output_path <- file.path(private_dir, paste0(project, "_adi_private_", analysis_date, ".xlsx"))
exclusion_path <- file.path(private_dir, paste0(project, "_adi_exclusions_", analysis_date, ".csv"))
summary_path <- file.path(private_dir, paste0(project, "_adi_summary_", analysis_date, ".csv"))

adi_paths <- c(
  PA = file.path(adi_root, "adi-download_Pennsylvania", "PA_2024_ADI_9Digit_Zip_v4_0_1.csv"),
  NJ = file.path(adi_root, "adi-download_NewJersey", "NJ_2024_ADI_9Digit_Zip_v4_0_1.csv"),
  DE = file.path(adi_root, "adi-download_Delaware", "DE_2024_ADI_9Digit_Zip_v4_0_1.csv"),
  MD = file.path(adi_root, "adi-download_Maryland", "MD_2024_ADI_9Digit_Zip_v4_0_1.csv"),
  VA = file.path(adi_root, "adi-download_Virginia", "VA_2024_ADI_9Digit_Zip_v4_0_1.csv")
)
for (path in c(input_path, geocodio_path, review_path, adi_paths)) {
  if (!file.exists(path)) stop("Required private input not found: ", path)
}

read_csv_character <- function(path) {
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
  if (length(missing) > 0L) stop(label, " is missing: ", paste(missing, collapse = ", "))
}

digits_only <- function(x) gsub("[^0-9]", "", ifelse(is.na(x), "", x))
normalize_address <- function(x) tolower(gsub("[^[:alnum:]]", "", ifelse(is.na(x), "", x)))
normalize_zip5 <- function(x) {
  value <- digits_only(x)
  ifelse(nchar(value) >= 5L, substr(value, 1L, 5L), NA_character_)
}
normalize_zip9 <- function(x) {
  value <- digits_only(x)
  ifelse(nchar(value) == 9L, value, NA_character_)
}
safe_numeric <- function(x) suppressWarnings(as.numeric(x))

collapse_unambiguous <- function(data, key_columns, value_columns) {
  selected <- unique(data[c(key_columns, value_columns)])
  if (nrow(selected) == 0L) return(selected)
  key <- do.call(paste, c(selected[key_columns], sep = "|"))
  selected[!key %in% names(which(table(key) > 1L)), , drop = FALSE]
}

stream_adi_matches <- function(zip9_targets = character(), gisjoin_targets = character()) {
  zip9_targets <- unique(zip9_targets[!is.na(zip9_targets)])
  gisjoin_targets <- unique(gisjoin_targets[!is.na(gisjoin_targets)])
  required <- c("TYPE", "STATE", "BENE_ZIP_CD", "GISJOIN", "ADI_NATRANK", "ADI_STATERNK")
  collected <- list()
  output_index <- 0L
  for (state in names(adi_paths)) {
    connection <- file(adi_paths[[state]], open = "rt", encoding = "UTF-8")
    header <- readLines(connection, n = 1L, warn = FALSE)
    repeat {
      lines <- readLines(connection, n = 10000L, warn = FALSE)
      if (length(lines) == 0L) break
      block <- utils::read.csv(
        text = paste(c(header, lines), collapse = "\n"),
        colClasses = "character", check.names = FALSE,
        na.strings = c("", "NA", "N/A"), strip.white = FALSE
      )
      assert_columns(block, required, paste(state, "ADI crosswalk"))
      zip9 <- normalize_zip9(block$BENE_ZIP_CD)
      gisjoin <- trimws(ifelse(is.na(block$GISJOIN), "", block$GISJOIN))
      keep <- trimws(ifelse(is.na(block$TYPE), "", block$TYPE)) == "" &
        (zip9 %in% zip9_targets | gisjoin %in% gisjoin_targets)
      if (any(keep)) {
        output_index <- output_index + 1L
        selected <- block[keep, c("STATE", "BENE_ZIP_CD", "GISJOIN", "ADI_NATRANK", "ADI_STATERNK")]
        selected$STATE <- toupper(trimws(selected$STATE))
        selected$BENE_ZIP_CD <- normalize_zip9(selected$BENE_ZIP_CD)
        selected$GISJOIN <- trimws(selected$GISJOIN)
        selected$ADI_NATRANK <- safe_numeric(selected$ADI_NATRANK)
        selected$ADI_STATERNK <- safe_numeric(selected$ADI_STATERNK)
        collected[[output_index]] <- selected
      }
    }
    close(connection)
  }
  if (length(collected) == 0L) {
    return(data.frame(STATE = character(), BENE_ZIP_CD = character(), GISJOIN = character(),
                      ADI_NATRANK = numeric(), ADI_STATERNK = numeric()))
  }
  do.call(rbind, collected)
}

state_fips <- c(PA = "42", NJ = "34", DE = "10", MD = "24", VA = "51")
tiger_root <- file.path(adi_root, "tigerline_2024_bg")
load_tiger <- function(state) {
  fips <- unname(state_fips[[state]])
  state_dir <- file.path(tiger_root, state)
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
  shape <- file.path(state_dir, paste0("tl_2024_", fips, "_bg.shp"))
  if (!file.exists(shape)) {
    archive <- file.path(state_dir, paste0("tl_2024_", fips, "_bg.zip"))
    if (!file.exists(archive)) {
      url <- paste0("https://www2.census.gov/geo/tiger/TIGER2024/BG/tl_2024_", fips, "_bg.zip")
      utils::download.file(url, archive, mode = "wb")
    }
    utils::unzip(archive, exdir = state_dir)
  }
  polygons <- suppressMessages(sf::st_read(shape, quiet = TRUE))
  geoid_column <- intersect(c("GEOID", "GEOID20"), names(polygons))
  if (length(geoid_column) == 0L) stop("TIGER file lacks GEOID: ", shape)
  polygons$GEOID <- as.character(polygons[[geoid_column[[1L]]]])
  sf::st_transform(polygons["GEOID"], 4326)
}

geoid_to_gisjoin <- function(geoid) {
  ifelse(
    is.na(geoid) | nchar(geoid) != 12L, NA_character_,
    paste0("G", substr(geoid, 1, 2), "0", substr(geoid, 3, 5), "0",
           substr(geoid, 6, 11), substr(geoid, 12, 12))
  )
}

data <- as.data.frame(readxl::read_excel(input_path, sheet = "Data"))
assert_columns(data, c("rf1_id", "sub_age", "gender", "ecog_score", "mspss_sum", "address", "zip_code"),
               "Private cohort")
if (anyDuplicated(data$rf1_id)) stop("Private cohort contains duplicate rf1_id values")

geocodio <- read_csv_character(geocodio_path)
assert_columns(
  geocodio,
  c("street", "postal_code", "Geocodio Latitude", "Geocodio Longitude",
    "Geocodio Accuracy Score", "Geocodio Accuracy Type", "Geocodio State / Province",
    "Geocodio Postal Code", "ZIP 9 Range Start", "ZIP 9 Range End (or blank if no range)",
    "Exact Match?"),
  "Fixed Geocodio return"
)
geocodio$address_key <- paste(normalize_address(geocodio$street), normalize_zip5(geocodio$postal_code), sep = "|")
if (anyDuplicated(geocodio$address_key)) stop("Geocodio return has duplicate submitted address + ZIP5 keys")
if (any(!is.na(geocodio$`ZIP 9 Range End (or blank if no range)`))) {
  stop("Geocodio returned a ZIP9 range; manual review is required")
}
geocodio$zip9 <- normalize_zip9(geocodio$`ZIP 9 Range Start`)
geocodio$state <- toupper(geocodio$`Geocodio State / Province`)
geocodio$longitude <- safe_numeric(geocodio$`Geocodio Longitude`)
geocodio$latitude <- safe_numeric(geocodio$`Geocodio Latitude`)

data$address_key <- paste(normalize_address(data$address), normalize_zip5(data$zip_code), sep = "|")
geocodio_index <- match(data$address_key, geocodio$address_key)
if (anyNA(geocodio_index)) stop("At least one cohort address + ZIP5 is absent from the fixed Geocodio return")

review <- read_csv_character(review_path)
assert_columns(
  review,
  c("rf1_id", "source_address", "source_zip5", "census_longitude", "census_latitude",
    "gisjoin", "adi_natrank", "adi_staternk", "decision", "decision_reason"),
  "Private ADI review"
)
review_index <- match(as.character(data$rf1_id), review$rf1_id)
reviewed <- !is.na(review_index)
if (any(reviewed)) {
  idx <- review_index[reviewed]
  if (any(normalize_address(data$address[reviewed]) != normalize_address(review$source_address[idx])) ||
      any(normalize_zip5(data$zip_code[reviewed]) != normalize_zip5(review$source_zip5[idx]))) {
    stop("Current private cohort disagrees with the fixed ADI review source fields")
  }
}

gc <- geocodio[geocodio_index, , drop = FALSE]
adi_zip_rows <- stream_adi_matches(zip9_targets = gc$zip9)
adi_zip <- collapse_unambiguous(
  adi_zip_rows, c("STATE", "BENE_ZIP_CD"), c("GISJOIN", "ADI_NATRANK", "ADI_STATERNK")
)
adi_zip$key <- paste(adi_zip$STATE, adi_zip$BENE_ZIP_CD, sep = "|")
direct_index <- match(paste(gc$state, gc$zip9, sep = "|"), adi_zip$key)
direct_match <- !is.na(direct_index)

coordinate_gisjoin <- rep(NA_character_, nrow(data))
coordinate_candidate <- !direct_match &
  tolower(ifelse(is.na(gc$`Geocodio Accuracy Type`), "", gc$`Geocodio Accuracy Type`)) == "rooftop" &
  !is.na(gc$longitude) & !is.na(gc$latitude) & gc$state %in% names(state_fips)
for (state in intersect(unique(gc$state[coordinate_candidate]), names(state_fips))) {
  row_index <- which(coordinate_candidate & gc$state == state)
  points <- sf::st_as_sf(
    data.frame(row_index = row_index, longitude = gc$longitude[row_index], latitude = gc$latitude[row_index]),
    coords = c("longitude", "latitude"), crs = 4326, remove = FALSE
  )
  joined <- suppressWarnings(sf::st_join(points, load_tiger(state), join = sf::st_within))
  if (nrow(joined) != length(row_index)) stop("Unexpected TIGER one-to-many join")
  coordinate_gisjoin[row_index] <- geoid_to_gisjoin(joined$GEOID)
}

adi_block_rows <- stream_adi_matches(gisjoin_targets = coordinate_gisjoin)
adi_block <- collapse_unambiguous(
  adi_block_rows, "GISJOIN", c("STATE", "ADI_NATRANK", "ADI_STATERNK")
)
block_index <- match(coordinate_gisjoin, adi_block$GISJOIN)
block_match <- !direct_match & !is.na(block_index)

data$adi_natrank <- NA_real_
data$adi_staternk <- NA_real_
data$adi_state <- NA_character_
data$adi_gisjoin <- NA_character_
data$adi_match_status <- NA_character_
if (any(direct_match)) {
  idx <- direct_index[direct_match]
  data$adi_natrank[direct_match] <- adi_zip$ADI_NATRANK[idx]
  data$adi_staternk[direct_match] <- adi_zip$ADI_STATERNK[idx]
  data$adi_state[direct_match] <- adi_zip$STATE[idx]
  data$adi_gisjoin[direct_match] <- adi_zip$GISJOIN[idx]
  data$adi_match_status[direct_match] <- ifelse(
    tolower(gc$`Exact Match?`[direct_match]) == "yes", "matched_zip9_exact", "matched_zip9_not_exact"
  )
}
if (any(block_match)) {
  idx <- block_index[block_match]
  data$adi_natrank[block_match] <- adi_block$ADI_NATRANK[idx]
  data$adi_staternk[block_match] <- adi_block$ADI_STATERNK[idx]
  data$adi_state[block_match] <- adi_block$STATE[idx]
  data$adi_gisjoin[block_match] <- adi_block$GISJOIN[idx]
  data$adi_match_status[block_match] <- "matched_rooftop_coordinate"
}

linked <- !is.na(data$adi_natrank) & !is.na(data$adi_staternk)
accepted_review <- reviewed
accepted_review[reviewed] <- review$decision[review_index[reviewed]] == "accept_census_regeocode"
review_match <- !linked & accepted_review
if (any(review_match)) {
  idx <- review_index[review_match]
  data$adi_natrank[review_match] <- safe_numeric(review$adi_natrank[idx])
  data$adi_staternk[review_match] <- safe_numeric(review$adi_staternk[idx])
  data$adi_state[review_match] <- "PA"
  data$adi_gisjoin[review_match] <- review$gisjoin[idx]
  data$adi_match_status[review_match] <- "matched_census_regeocode"
}

data$pm25_longitude <- gc$longitude
data$pm25_latitude <- gc$latitude
data$pm25_coordinate_source <- "Geocodio"
if (any(review_match)) {
  idx <- review_index[review_match]
  data$pm25_longitude[review_match] <- safe_numeric(review$census_longitude[idx])
  data$pm25_latitude[review_match] <- safe_numeric(review$census_latitude[idx])
  data$pm25_coordinate_source[review_match] <- "US Census Geocoder reviewed address"
}

linked <- !is.na(data$adi_natrank) & !is.na(data$adi_staternk)
exclusions <- data.frame(
  rf1_id = data$rf1_id[!linked],
  reason = ifelse(reviewed[!linked], review$decision_reason[review_index[!linked]],
                  "no_numeric_adi_linkage"),
  stringsAsFactors = FALSE
)
private_output <- data[linked, setdiff(names(data), "address_key"), drop = FALSE]
if (anyNA(private_output$pm25_longitude) || anyNA(private_output$pm25_latitude)) {
  stop("An ADI-linked participant lacks PM2.5 extraction coordinates")
}

writexl::write_xlsx(list(Data = private_output), output_path)
utils::write.csv(exclusions, exclusion_path, row.names = FALSE, na = "")
utils::write.csv(data.frame(
  project = project, participants_entering = nrow(data),
  participants_linked = nrow(private_output), participants_excluded = nrow(exclusions)
), summary_path, row.names = FALSE, na = "")

cat(project_label, "ADI linkage complete\n")
cat("Participants entering:", nrow(data), "\n")
cat("Participants linked:", nrow(private_output), "\n")
cat("Private ADI workbook:", normalizePath(output_path, winslash = "/"), "\n")
