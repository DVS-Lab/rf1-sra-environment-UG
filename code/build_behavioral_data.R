#!/usr/bin/env Rscript

# Build the analysis-ready Ultimatum Game task data for the final RF1 sample.
#
# INPUTS
# ------
#   1. Canonical BIDS *_task-ugr_run-*_events.tsv files from rf1-sra-linux2.
#      These files contain task timing, offers, endowments, choices, and response
#      times for each participant, session, and run.
#   2. Model-3 condition pmod EV files from rf1-sra-ugr. Their amplitudes encode
#      offer fairness after demeaning within each run, sociality, and endowment
#      cell. Every responded trial is checked against its matching EV row.
#   3. L3-act-subjects.txt beside this script. It contains one five-digit study
#      ID per line.
#   4. L3-act-selected-inputs.tsv beside this script. Its validated handoff rows
#      define the exact session and run(s) retained for every listed participant.
#      Only those participant/session/run combinations enter any output or model.
#
# OUTPUTS
# -------
# All four files are written beside this script and contain only the exact
# participant/session/run combinations in L3-act-selected-inputs.tsv:
#
#   Allcompiled_behavior.csv  One row per task trial, including missed trials.
#   slope_df.csv              One row per participant with fairness-sensitivity
#                             slopes from three binomial mixed-effects models.
#   behavioral_data.csv       Responded trials, derived behavioral variables,
#                             and the participant-level slopes.
#   study_df.xlsx             Responded trials in long format, restricted to the
#                             study ID and task columns listed in the configuration.
#
# MODEL
# -----
# Acceptance is modeled from the offered proportion of the endowment with a
# participant-specific intercept and fairness slope. The model is fitted once
# using all responded trials, once for social trials, and once for nonsocial
# trials. A participant's reported slope is the population fairness coefficient
# plus that participant's random fairness coefficient.
#
# RUN
# ---
# Change into this code directory and run exactly:
#
#   Rscript build_behavioral_data.R
#
# All input and output paths are defined in the configuration block below. The
# script has no command-line options.
#
# SOFTWARE
# --------
# R >= 4.1 is recommended. Required packages are readr, dplyr, tidyr, purrr,
# stringr, tibble, lme4, and writexl.


# -----------------------------------------------------------------------------
# 1. File locations and package setup
# -----------------------------------------------------------------------------

script_directory <- function() {
  full_arguments <- commandArgs(trailingOnly = FALSE)
  script_argument <- grep("^--file=", full_arguments, value = TRUE)

  if (length(script_argument) == 0) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }

  script_path <- sub("^--file=", "", script_argument[[1]])
  normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
}

required_packages <- c(
  "readr", "dplyr", "tidyr", "purrr", "stringr", "tibble", "lme4", "writexl"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing R package(s): ", paste(missing_packages, collapse = ", "), ".\n",
    "Install them with: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
    call. = FALSE
  )
}

code_dir <- script_directory()

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
# These defaults match the Linux analysis machine. BIDS_ROOT and UGR_EV_ROOT may
# be set in the environment when the same inputs are mounted elsewhere.
BIDS_ROOT <- Sys.getenv(
  "BIDS_ROOT",
  unset = "/ZPOOL/data/projects/rf1-sra-linux2/bids"
)
EV_ROOT <- Sys.getenv(
  "UGR_EV_ROOT",
  unset = "/ZPOOL/data/projects/rf1-sra-ugr/derivatives/fsl/EVfiles"
)
SUBJECT_LIST_PATH <- file.path(code_dir, "L3-act-subjects.txt")
SELECTED_INPUTS_PATH <- file.path(code_dir, "L3-act-selected-inputs.tsv")

# Outputs are written beside this script.
OUTPUT_DIRECTORY <- code_dir

# study_df.xlsx is controlled by a whitelist so changes to an upstream table
# cannot add unrequested columns to the workbook.
STUDY_EXPORT_COLUMNS <- c(
  "sub_id",
  "nTrial",
  "session",
  "run",
  "cue_Onset",
  "event_duration",
  "endowment_label",
  "condition",
  "fairness",
  "accept_reject",
  "rt",
  "R_Option",
  "L_Option",
  "Endowment"
)

# -----------------------------------------------------------------------------
# 2. Small validation and file-writing helpers
# -----------------------------------------------------------------------------

require_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      label, " is missing required column(s): ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

# Treat participant IDs as text. Remove only a possible numeric-import ".0"
# artifact while preserving leading zeros.
normalize_id <- function(value) {
  value <- trimws(as.character(value))
  value[value %in% c("", "NA", "NaN", "n/a")] <- NA_character_
  sub("\\.0$", "", value)
}

read_final_subject_ids <- function(path) {
  if (!file.exists(path)) {
    stop("Final subject list does not exist: ", path, call. = FALSE)
  }

  lines <- trimws(readLines(path, warn = FALSE))
  if (length(lines) == 0 || any(lines == "")) {
    stop(
      "The final subject list must contain one study ID on every line and no blank lines.",
      call. = FALSE
    )
  }

  subject_ids <- normalize_id(lines)
  invalid_ids <- subject_ids[is.na(subject_ids) | !grepl("^[0-9]{5}$", subject_ids)]
  if (length(invalid_ids) > 0) {
    stop(
      "The final subject list contains an invalid study ID: ", invalid_ids[[1]],
      ". Every ID must contain exactly five digits.",
      call. = FALSE
    )
  }

  duplicated_ids <- unique(subject_ids[duplicated(subject_ids)])
  if (length(duplicated_ids) > 0) {
    stop(
      "The final subject list contains duplicate ID(s): ",
      paste(duplicated_ids, collapse = ", "),
      call. = FALSE
    )
  }

  subject_ids
}

require_exact_subject_set <- function(data, subject_ids, label) {
  observed_ids <- sort(unique(normalize_id(data$sub_id)))
  expected_ids <- sort(subject_ids)
  missing_ids <- setdiff(expected_ids, observed_ids)
  unexpected_ids <- setdiff(observed_ids, expected_ids)

  if (length(missing_ids) > 0 || length(unexpected_ids) > 0) {
    details <- c(
      if (length(missing_ids) > 0) {
        paste0("missing: ", paste(missing_ids, collapse = ", "))
      },
      if (length(unexpected_ids) > 0) {
        paste0("unexpected: ", paste(unexpected_ids, collapse = ", "))
      }
    )
    stop(label, " has the wrong participant set (", paste(details, collapse = "; "), ").",
         call. = FALSE)
  }
}

clean_text <- function(value) {
  value <- trimws(as.character(value))
  value[value %in% c("", "NA", "NaN", "n/a")] <- NA_character_
  value
}

as_required_number <- function(value, label) {
  number <- suppressWarnings(as.numeric(value))
  if (length(number) != 1 || is.na(number) || !is.finite(number)) {
    stop(label, " must contain one finite numeric value.", call. = FALSE)
  }
  number
}

write_csv_safely <- function(data, path) {
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  temporary_path <- tempfile(
    pattern = paste0(".", basename(path), "."),
    tmpdir = directory,
    fileext = ".tmp"
  )
  on.exit(unlink(temporary_path), add = TRUE)

  readr::write_csv(data, temporary_path, na = "")

  # file.rename() cannot replace an existing target consistently across operating
  # systems. The exact target has already been resolved by the caller.
  if (file.exists(path) && !file.remove(path)) {
    stop("Could not replace existing output: ", path, call. = FALSE)
  }
  if (!file.rename(temporary_path, path)) {
    stop("Could not move completed output into place: ", path, call. = FALSE)
  }
}

write_xlsx_safely <- function(data, path) {
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  temporary_path <- tempfile(
    pattern = paste0(".", basename(path), "."),
    tmpdir = directory,
    fileext = ".xlsx"
  )
  on.exit(unlink(temporary_path), add = TRUE)

  # A single plainly named sheet preserves the long/tabular structure expected
  # by R, Python, and spreadsheet users. No formulas or hidden columns are added.
  writexl::write_xlsx(list(study_df = data), path = temporary_path)

  if (file.exists(path) && !file.remove(path)) {
    stop("Could not replace existing output: ", path, call. = FALSE)
  }
  if (!file.rename(temporary_path, path)) {
    stop("Could not move completed output into place: ", path, call. = FALSE)
  }
}

# -----------------------------------------------------------------------------
# 3. Discover and read canonical BIDS UGR event files
# -----------------------------------------------------------------------------

normalize_session <- function(value) {
  number <- suppressWarnings(as.integer(sub("^ses-", "", value)))
  if (is.na(number)) stop("Invalid session label: ", value, call. = FALSE)
  sprintf("%02d", number)
}

read_selected_run_manifest <- function(path, subject_ids) {
  if (!file.exists(path)) {
    stop("Selected-input manifest does not exist: ", path, call. = FALSE)
  }

  selected <- readr::read_tsv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )
  required <- c(
    "subject", "session", "analysis", "input_runs", "level", "contrast",
    "selection"
  )
  require_columns(selected, required, basename(path))

  selected <- selected |>
    dplyr::mutate(
      sub_id = normalize_id(.data$subject),
      session = vapply(.data$session, normalize_session, character(1)),
      analysis = tolower(clean_text(.data$analysis)),
      input_runs = clean_text(.data$input_runs),
      level = clean_text(.data$level),
      contrast = suppressWarnings(as.integer(.data$contrast)),
      selection = clean_text(.data$selection)
    )

  invalid_ids <- unique(
    selected$sub_id[is.na(selected$sub_id) | !grepl("^[0-9]{5}$", selected$sub_id)]
  )
  if (length(invalid_ids) > 0) {
    stop("Selected-input manifest contains an invalid participant ID.", call. = FALSE)
  }
  if (any(is.na(selected$analysis)) || any(selected$analysis != "ugr")) {
    stop("Selected-input manifest contains a non-UGR analysis row.", call. = FALSE)
  }
  if (any(is.na(selected$selection)) ||
      any(selected$selection != "selected_coverage95_preserved_decisions")) {
    stop("Selected-input manifest contains an unvalidated selection row.", call. = FALSE)
  }
  if (any(is.na(selected$contrast)) ||
      any(!(selected$contrast %in% seq_len(17)))) {
    stop("Selected-input manifest must contain only contrasts 1 through 17.", call. = FALSE)
  }

  consistency <- selected |>
    dplyr::group_by(.data$sub_id) |>
    dplyr::summarise(
      row_count = dplyr::n(),
      session_count = dplyr::n_distinct(.data$session),
      input_runs_count = dplyr::n_distinct(.data$input_runs),
      level_count = dplyr::n_distinct(.data$level),
      contrast_count = dplyr::n_distinct(.data$contrast),
      .groups = "drop"
    )
  inconsistent <- consistency |>
    dplyr::filter(
      .data$row_count != 17 |
        .data$session_count != 1 |
        .data$input_runs_count != 1 |
        .data$level_count != 1 |
        .data$contrast_count != 17
    )
  if (nrow(inconsistent) > 0) {
    stop(
      "Each participant in the selected-input manifest must have one consistent ",
      "session/run/level selection and exactly one row for each contrast 1 through 17.",
      call. = FALSE
    )
  }

  manifest <- selected |>
    dplyr::distinct(.data$sub_id, .data$session, .data$input_runs, .data$level) |>
    dplyr::arrange(.data$sub_id)

  require_exact_subject_set(
    manifest,
    subject_ids,
    "L3-act-selected-inputs.tsv"
  )

  allowed_runs <- c("1", "2", "1+2")
  if (any(is.na(manifest$input_runs)) ||
      any(!(manifest$input_runs %in% allowed_runs))) {
    stop("Selected input_runs must be one of: 1, 2, or 1+2.", call. = FALSE)
  }
  expected_level <- ifelse(manifest$input_runs == "1+2", "L2", "L1")
  if (any(is.na(manifest$level)) || any(manifest$level != expected_level)) {
    stop("Selected input_runs and level disagree in the selected-input manifest.",
         call. = FALSE)
  }

  expanded <- purrr::pmap_dfr(
    manifest,
    function(sub_id, session, input_runs, level) {
      tibble::tibble(
        sub_id = sub_id,
        session = session,
        run = as.integer(strsplit(input_runs, "\\+")[[1]])
      )
    }
  ) |>
    dplyr::arrange(.data$sub_id, .data$session, .data$run)

  duplicates <- expanded |>
    dplyr::count(.data$sub_id, .data$session, .data$run) |>
    dplyr::filter(.data$n != 1)
  if (nrow(duplicates) > 0 || any(is.na(expanded$run))) {
    stop("Selected-input manifest expands to invalid or duplicate runs.", call. = FALSE)
  }

  expanded
}

discover_event_files <- function(bids_root, sessions) {
  if (!dir.exists(bids_root)) {
    stop("BIDS root does not exist: ", bids_root, call. = FALSE)
  }

  paths <- list.files(
    bids_root,
    pattern = "_task-ugr_run-[^_]+_events\\.tsv$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(paths) == 0) {
    stop(
      "No canonical *_task-ugr_run-*_events.tsv files found under ", bids_root,
      ". Raw task logs are intentionally unsupported.",
      call. = FALSE
    )
  }

  filename_pattern <- paste0(
    "^sub-([^_]+)_ses-([^_]+)_task-ugr_run-([^_]+)_events\\.tsv$"
  )
  parts <- stringr::str_match(basename(paths), filename_pattern)
  recognized <- !is.na(parts[, 1])
  paths <- paths[recognized]
  parts <- parts[recognized, , drop = FALSE]

  discovered <- tibble::tibble(
    path = paths,
    sub_id = parts[, 2],
    session = vapply(parts[, 3], normalize_session, character(1)),
    run = suppressWarnings(as.integer(parts[, 4]))
  ) |>
    dplyr::filter(.data$session %in% sessions) |>
    dplyr::arrange(.data$sub_id, .data$session, .data$run)

  if (nrow(discovered) == 0) {
    stop("No canonical event files matched the requested sessions.", call. = FALSE)
  }
  if (any(is.na(discovered$run))) {
    stop("At least one canonical event filename has a nonnumeric run label.", call. = FALSE)
  }

  duplicates <- discovered |>
    dplyr::count(.data$sub_id, .data$session, .data$run) |>
    dplyr::filter(.data$n > 1)
  if (nrow(duplicates) > 0) {
    stop(
      "Duplicate canonical event files exist for ", nrow(duplicates),
      " participant/session/run combination(s).",
      call. = FALSE
    )
  }

  discovered
}

single_trial_row <- function(trial, trial_type, trial_label) {
  rows <- trial[clean_text(trial$trial_type) == trial_type, , drop = FALSE]
  if (nrow(rows) != 1) {
    stop(
      trial_label, " must contain exactly one '", trial_type,
      "' event; found ", nrow(rows), ".",
      call. = FALSE
    )
  }
  rows
}

constant_trial_value <- function(trial, column, trial_label) {
  values <- unique(clean_text(trial[[column]]))
  values <- values[!is.na(values)]
  if (length(values) > 1) {
    stop(
      trial_label, " has ", length(values), " distinct nonmissing values for ",
      column, ".",
      call. = FALSE
    )
  }
  if (length(values) == 0) NA_character_ else values[[1]]
}

read_one_event_file <- function(path, sub_id, session, run) {
  events <- readr::read_tsv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "n/a", "NA", "na", "--"),
    show_col_types = FALSE,
    progress = FALSE
  )

  required <- c(
    "onset", "duration", "trial_type", "response_time", "trial_id", "phase",
    "sociality", "endowment", "offer", "decision", "response",
    "left_option", "right_option"
  )
  require_columns(events, required, basename(path))

  if (nrow(events) == 0 || any(is.na(clean_text(events$trial_id)))) {
    stop(basename(path), " is empty or contains a missing trial_id.", call. = FALSE)
  }

  trials <- split(events, clean_text(events$trial_id))

  compiled <- purrr::imap_dfr(trials, function(trial, trial_id) {
    trial_label <- paste0(basename(path), " trial ", trial_id)
    metadata_columns <- c(
      "sociality", "endowment", "offer", "decision", "response",
      "left_option", "right_option"
    )
    metadata <- stats::setNames(
      lapply(metadata_columns, function(column) {
        constant_trial_value(trial, column, trial_label)
      }),
      metadata_columns
    )

    partner <- single_trial_row(trial, "partner_cue", trial_label)
    partner_onset <- as_required_number(partner$onset, paste(trial_label, "partner onset"))

    missed <- trial[clean_text(trial$trial_type) == "missed_decision", , drop = FALSE]
    feedback <- trial[clean_text(trial$trial_type) == "choice_feedback", , drop = FALSE]
    if ((nrow(missed) == 1) == (nrow(feedback) == 1) || nrow(missed) > 1 || nrow(feedback) > 1) {
      stop(
        trial_label,
        " must contain exactly one terminal state: missed_decision or choice_feedback.",
        call. = FALSE
      )
    }

    sociality <- tolower(metadata$sociality)
    if (!(sociality %in% c("social", "nonsocial"))) {
      stop(trial_label, " has invalid sociality: ", sociality, call. = FALSE)
    }

    endowment <- as_required_number(metadata$endowment, paste(trial_label, "endowment"))
    if (!(endowment %in% c(16, 32))) {
      stop(trial_label, " has an unexpected endowment: ", endowment, call. = FALSE)
    }

    left_option <- as_required_number(metadata$left_option, paste(trial_label, "left option"))
    right_option <- as_required_number(metadata$right_option, paste(trial_label, "right option"))

    if (nrow(missed) == 1) {
      terminal <- missed
      accept_reject <- "Miss"
      response_time <- NA_real_
      offer <- NA_real_
    } else {
      terminal <- feedback
      decision <- tolower(metadata$decision)
      if (!(decision %in% c("accept", "reject"))) {
        stop(trial_label, " has invalid decision: ", decision, call. = FALSE)
      }
      accept_reject <- tools::toTitleCase(decision)
      offer <- as_required_number(metadata$offer, paste(trial_label, "offer"))
      decision_row <- single_trial_row(trial, "decision", trial_label)
      response_time <- as_required_number(
        decision_row$response_time,
        paste(trial_label, "response time")
      )
    }

    terminal_end <-
      as_required_number(terminal$onset, paste(trial_label, "terminal onset")) +
      as_required_number(terminal$duration, paste(trial_label, "terminal duration"))
    broad_epoch_duration <- terminal_end - partner_onset

    if (partner_onset < 0 || broad_epoch_duration <= 0) {
      stop(trial_label, " has invalid broad-epoch timing.", call. = FALSE)
    }

    tibble::tibble(
      session = paste0("ses-", session),
      run = run,
      cue_Onset = partner_onset,
      event_duration = broad_epoch_duration,
      endowment_label = ifelse(endowment == 32, "high", "low"),
      condition = sociality,
      fairness = NA_real_,
      accept_reject = accept_reject,
      rt = response_time,
      R_Option = right_option,
      L_Option = left_option,
      Endowment = endowment,
      sub_id = sub_id,
      .offer = offer
    )
  }) |>
    dplyr::arrange(.data$cue_Onset) |>
    dplyr::mutate(nTrial = dplyr::row_number(), .before = 1)

  compiled
}


# -----------------------------------------------------------------------------
# 4. Check every responded trial against the authoritative model-3 EV pmods
# -----------------------------------------------------------------------------

locate_ev_file <- function(ev_root, sub_id, session, run, condition_label) {
  relative_path <- file.path(
    paste0("sub-", sub_id),
    paste0("ses-", session),
    "ugr",
    "model-3",
    paste0("run-", run, "_", condition_label, "_pmod.txt")
  )

  candidates <- c(
    file.path(ev_root, relative_path),
    file.path(ev_root, "EVfiles", relative_path),
    file.path(ev_root, "derivatives", "fsl", "EVfiles", relative_path)
  )
  matches <- unique(candidates[file.exists(candidates)])

  if (length(matches) != 1) {
    stop(
      "Expected exactly one model-3 pmod EV for sub-", sub_id,
      " ses-", session, " run-", run, " ", condition_label,
      "; found ", length(matches), ".",
      call. = FALSE
    )
  }
  matches[[1]]
}

read_pmod_ev <- function(path) {
  ev <- readr::read_table(
    path,
    col_names = c("onset", "duration", "amplitude"),
    col_types = readr::cols(
      onset = readr::col_double(),
      duration = readr::col_double(),
      amplitude = readr::col_double()
    ),
    show_col_types = FALSE,
    progress = FALSE
  )

  if (ncol(ev) != 3 || any(!is.finite(ev$onset)) || any(!is.finite(ev$amplitude))) {
    stop("Malformed three-column pmod EV: ", path, call. = FALSE)
  }
  ev
}

add_and_validate_fairness <- function(trials, ev_root, sub_id, session, run) {
  specifications <- tibble::tribble(
    ~condition,   ~endowment, ~label,
    "social",            32, "social_high",
    "social",            16, "social_low",
    "nonsocial",         32, "nonsocial_high",
    "nonsocial",         16, "nonsocial_low"
  )

  for (row_number in seq_len(nrow(specifications))) {
    specification <- specifications[row_number, ]
    trial_rows <- which(
      trials$accept_reject != "Miss" &
        trials$condition == specification$condition &
        trials$Endowment == specification$endowment
    )

    # An incomplete but internally coherent run may have no trials in one cell.
    # In that case rf1-sra-ugr has no corresponding pmod to validate.
    if (length(trial_rows) == 0) next

    ev_path <- locate_ev_file(
      ev_root, sub_id, session, run, specification$label
    )
    ev <- read_pmod_ev(ev_path) |>
      dplyr::arrange(.data$onset)

    ordered_trial_rows <- trial_rows[order(trials$cue_Onset[trial_rows])]
    if (nrow(ev) != length(ordered_trial_rows)) {
      stop(
        ev_path, " contains ", nrow(ev), " rows, but canonical events define ",
        length(ordered_trial_rows), " matching responded trials.",
        call. = FALSE
      )
    }

    onset_difference <- abs(ev$onset - trials$cue_Onset[ordered_trial_rows])
    if (any(onset_difference > 1e-4)) {
      stop(ev_path, " has an onset that does not match canonical events.", call. = FALSE)
    }

    # rf1-sra-ugr model 3 defines this pmod as offer minus the mean offer within
    # the run's sociality-by-endowment cell. The validated EV amplitude becomes
    # the Allcompiled_behavior.csv column named "fairness".
    expected_amplitude <-
      trials$.offer[ordered_trial_rows] - mean(trials$.offer[ordered_trial_rows])
    if (any(abs(ev$amplitude - expected_amplitude) > 1e-5)) {
      stop(
        ev_path,
        " disagrees with the condition-demeaned offers in canonical events.",
        call. = FALSE
      )
    }

    trials$fairness[ordered_trial_rows] <- ev$amplitude
  }

  trials
}

build_allcompiled <- function(event_files, ev_root) {
  compiled <- purrr::pmap_dfr(event_files, function(path, sub_id, session, run) {
    trials <- read_one_event_file(path, sub_id, session, run)
    add_and_validate_fairness(trials, ev_root, sub_id, session, run)
  }) |>
    dplyr::select(-".offer") |>
    dplyr::arrange(.data$sub_id, .data$session, .data$run, .data$nTrial) |>
    dplyr::select(dplyr::all_of(c(
      "nTrial", "session", "run", "cue_Onset", "event_duration",
      "endowment_label", "condition", "fairness", "accept_reject", "rt",
      "R_Option", "L_Option", "Endowment", "sub_id"
    )))

  if (nrow(compiled) == 0 || dplyr::n_distinct(compiled$sub_id) == 0) {
    stop("Canonical behavioral compilation produced no trials.", call. = FALSE)
  }
  compiled
}


# -----------------------------------------------------------------------------
# 5. Prepare the behavioral variables used by the slope models and R Markdown
# -----------------------------------------------------------------------------

prepare_behavioral_analysis_data <- function(compiled) {
  required <- c(
    "sub_id", "accept_reject", "condition", "R_Option", "L_Option", "Endowment"
  )
  require_columns(compiled, required, "Compiled behavioral data")

  # The compiled table has already been restricted to the handoff-selected runs.
  # Missed trials remain in Allcompiled_behavior.csv for transparent accounting,
  # but they have no binary choice outcome and therefore cannot enter the models.
  study <- compiled |>
    dplyr::mutate(sub_id = normalize_id(.data$sub_id)) |>
    dplyr::filter(.data$accept_reject != "Miss") |>
    dplyr::mutate(
      offer = .data$R_Option + .data$L_Option,
      beh_fairness = .data$offer / .data$Endowment,
      accept = ifelse(.data$accept_reject == "Accept", 1, 0),
      # Map every task offer to one of the four experimental fairness levels.
      beh_fairness_perc = dplyr::case_when(
        dplyr::between(.data$beh_fairness, 0.060, 0.0625) ~ "5%",
        dplyr::between(.data$beh_fairness, 0.09375, 0.125) ~ "10%",
        abs(.data$beh_fairness - 0.25) < 1e-6 ~ "25%",
        abs(.data$beh_fairness - 0.50) < 1e-6 ~ "50%",
        TRUE ~ NA_character_
      ),
      beh_fairness_num = dplyr::case_when(
        .data$beh_fairness_perc == "5%" ~ 0.05,
        .data$beh_fairness_perc == "10%" ~ 0.10,
        .data$beh_fairness_perc == "25%" ~ 0.25,
        .data$beh_fairness_perc == "50%" ~ 0.50,
        TRUE ~ NA_real_
      )
    )

  if (nrow(study) == 0) {
    stop("No responded trials remain for behavioral modeling.", call. = FALSE)
  }
  if (any(is.na(study$beh_fairness_num))) {
    stop(
      "At least one responded trial has an offer/endowment ratio outside the four ",
      "defined fairness levels.",
      call. = FALSE
    )
  }

  study
}


# -----------------------------------------------------------------------------
# 6. Estimate participant-specific fairness-sensitivity slopes
# -----------------------------------------------------------------------------

estimate_slopes <- function(data, output_column) {
  model_data <- data |>
    dplyr::select(dplyr::all_of(c("sub_id", "beh_fairness_num", "accept"))) |>
    tidyr::drop_na() |>
    dplyr::mutate(sub_id = factor(.data$sub_id))

  if (dplyr::n_distinct(model_data$sub_id) < 2 ||
      dplyr::n_distinct(model_data$beh_fairness_num) < 2) {
    stop("Insufficient participants or fairness variation for ", output_column, call. = FALSE)
  }

  # The random intercept allows baseline acceptance to vary by participant; the
  # random fairness coefficient allows fairness sensitivity to vary as well.
  model <- lme4::glmer(
    accept ~ beh_fairness_num + (1 + beh_fairness_num | sub_id),
    data = model_data,
    family = stats::binomial()
  )

  random_effects <- lme4::ranef(model)$sub_id
  fixed_slope <- lme4::fixef(model)[["beh_fairness_num"]]
  participant_slopes <- fixed_slope + random_effects[, "beh_fairness_num"]

  if (lme4::isSingular(model, tol = 1e-4)) {
    warning(output_column, " mixed model has a singular random-effects fit.", call. = FALSE)
  }

  result <- tibble::tibble(
    sub_id = rownames(random_effects),
    slope = as.numeric(participant_slopes)
  )
  names(result)[names(result) == "slope"] <- output_column
  result
}

build_slope_table <- function(study) {
  slope_all <- estimate_slopes(study, "slope_all")
  slope_social <- estimate_slopes(
    dplyr::filter(study, .data$condition == "social"),
    "slope_social"
  )
  slope_nonsocial <- estimate_slopes(
    dplyr::filter(study, .data$condition == "nonsocial"),
    "slope_nonsocial"
  )

  purrr::reduce(
    list(slope_all, slope_social, slope_nonsocial),
    dplyr::full_join,
    by = "sub_id"
  ) |>
    dplyr::mutate(diff_slope = .data$slope_social - .data$slope_nonsocial) |>
    dplyr::arrange(.data$sub_id)
}


# -----------------------------------------------------------------------------
# 7. Run the complete workflow and write the four requested outputs
# -----------------------------------------------------------------------------

main <- function() {
  final_subject_ids <- read_final_subject_ids(SUBJECT_LIST_PATH)
  message(
    "Final subject list: ", normalizePath(SUBJECT_LIST_PATH, winslash = "/"),
    " (", length(final_subject_ids), " unique study IDs)"
  )

  selected_runs <- read_selected_run_manifest(
    SELECTED_INPUTS_PATH,
    final_subject_ids
  )
  requested_sessions <- sort(unique(selected_runs$session))
  message(
    "Selected-run manifest: ",
    normalizePath(SELECTED_INPUTS_PATH, winslash = "/"),
    " (", nrow(selected_runs), " participant/session/run combinations)"
  )

  discovered_event_files <- discover_event_files(BIDS_ROOT, requested_sessions)
  missing_selected_runs <- dplyr::anti_join(
    selected_runs,
    discovered_event_files,
    by = c("sub_id", "session", "run")
  )
  if (nrow(missing_selected_runs) > 0) {
    labels <- paste0(
      "sub-", missing_selected_runs$sub_id,
      "/ses-", missing_selected_runs$session,
      "/run-", missing_selected_runs$run
    )
    stop(
      "No canonical UGR event file was found for selected combination(s): ",
      paste(labels, collapse = ", "),
      call. = FALSE
    )
  }

  event_files <- dplyr::inner_join(
    discovered_event_files,
    selected_runs,
    by = c("sub_id", "session", "run")
  ) |>
    dplyr::arrange(.data$sub_id, .data$session, .data$run)
  if (nrow(event_files) != nrow(selected_runs)) {
    stop("Selected run manifest and canonical event files are not one-to-one.",
         call. = FALSE)
  }
  message(
    "Canonical behavior source: ", normalizePath(BIDS_ROOT, winslash = "/"),
    " (", nrow(event_files), " selected files for the final sample)"
  )
  message("Model-3 EV source: ", normalizePath(EV_ROOT, winslash = "/"))

  allcompiled <- build_allcompiled(event_files, EV_ROOT)
  require_exact_subject_set(
    allcompiled,
    final_subject_ids,
    "Allcompiled_behavior.csv"
  )
  message(
    "Compiled ", nrow(allcompiled), " trials from ",
    dplyr::n_distinct(allcompiled$sub_id), " participants."
  )

  analysis_data <- prepare_behavioral_analysis_data(allcompiled)
  require_exact_subject_set(
    analysis_data,
    final_subject_ids,
    "Behavioral model data"
  )
  message(
    "Analysis table before slopes: ", nrow(analysis_data), " responded trials from ",
    dplyr::n_distinct(analysis_data$sub_id), " participants."
  )

  slope_df <- build_slope_table(analysis_data)
  require_exact_subject_set(slope_df, final_subject_ids, "slope_df.csv")
  if (any(is.na(slope_df))) {
    stop(
      "At least one final-list participant lacks a slope from one or more models.",
      call. = FALSE
    )
  }
  behavioral_data <- analysis_data |>
    dplyr::left_join(slope_df, by = "sub_id", relationship = "many-to-one")

  # sub_id is the only participant-level column in this task-only workbook.
  study_df <- analysis_data |>
    dplyr::select(dplyr::all_of(STUDY_EXPORT_COLUMNS))

  if (!identical(names(study_df), STUDY_EXPORT_COLUMNS)) {
    stop("Internal error: study_df.xlsx column whitelist changed unexpectedly.", call. = FALSE)
  }

  output_directory <- normalizePath(
    OUTPUT_DIRECTORY,
    winslash = "/",
    mustWork = FALSE
  )
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

  allcompiled_path <- file.path(output_directory, "Allcompiled_behavior.csv")
  slope_path <- file.path(output_directory, "slope_df.csv")
  behavioral_path <- file.path(output_directory, "behavioral_data.csv")
  study_path <- file.path(output_directory, "study_df.xlsx")

  write_csv_safely(allcompiled, allcompiled_path)
  write_csv_safely(slope_df, slope_path)
  write_csv_safely(behavioral_data, behavioral_path)
  write_xlsx_safely(study_df, study_path)

  message("Wrote ", normalizePath(allcompiled_path, winslash = "/"))
  message("Wrote ", normalizePath(slope_path, winslash = "/"))
  message("Wrote ", normalizePath(behavioral_path, winslash = "/"))
  message("Wrote ", normalizePath(study_path, winslash = "/"))
}

if (sys.nframe() == 0) {
  main()
}

