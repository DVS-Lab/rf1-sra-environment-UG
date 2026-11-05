# Johnson-Neyman plot for the ADI by MSPSS interaction predicting the mean
# contrast-12 z statistic extracted from the adixmspss cluster mask.
#
# Run this script from any directory:
#   Rscript code/plot_adi_mspss_brain_johnson_neyman.R
#
# The script uses L3-act-selected-inputs.tsv to choose the validated L2, run-1,
# or run-2 extraction for each participant. The model includes the nuisance
# covariates from the original adixmspss L3 imaging model. The figure is
# written beside this script as:
#   adi_mspss_brain_activity_johnson_neyman.svg
#
# Important: the extraction mask was defined from the same ADI by MSPSS group
# result. This plot is therefore a descriptive visualization of that result,
# not an independent inferential test of the interaction.


# -----------------------------------------------------------------------------
# 1. Packages and file locations
# -----------------------------------------------------------------------------

required_packages <- c("readr", "readxl", "dplyr", "tibble", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following R packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

script_argument <- grep(
  "^--file=",
  commandArgs(trailingOnly = FALSE),
  value = TRUE
)

script_directory <- if (length(script_argument) == 1) {
  dirname(normalizePath(
    sub("^--file=", "", script_argument),
    winslash = "/",
    mustWork = TRUE
  ))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

subject_file <- file.path(script_directory, "L3-act-subjects.txt")
selected_inputs_file <- file.path(
  script_directory,
  "L3-act-selected-inputs.tsv"
)
l3_covariate_file <- file.path(
  script_directory,
  "L3-act-covariates.xlsx"
)
analysis_covariate_file <- file.path(
  script_directory,
  "..",
  "data",
  "derived",
  "UG_analysis_data_2026-11-04.xlsx"
)
extraction_directory <- file.path(
  script_directory,
  "..",
  "derivatives",
  "extractions",
  "adixmspss_cope12_ugr_meants"
)
output_file <- file.path(
  script_directory,
  "adi_mspss_brain_activity_johnson_neyman.svg"
)

input_paths <- c(
  subject_file,
  selected_inputs_file,
  l3_covariate_file,
  analysis_covariate_file,
  extraction_directory
)
missing_input_paths <- input_paths[!file.exists(input_paths)]

if (length(missing_input_paths) > 0) {
  stop(
    "The following required inputs were not found:\n  ",
    paste(missing_input_paths, collapse = "\n  "),
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# 2. Read the validated brain extraction for each participant
# -----------------------------------------------------------------------------

subjects <- scan(subject_file, what = character(), quiet = TRUE)

if (length(subjects) == 0 || anyDuplicated(subjects) > 0) {
  stop(
    "L3-act-subjects.txt must contain unique participant IDs.",
    call. = FALSE
  )
}

selected_inputs <- readr::read_tsv(
  selected_inputs_file,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

required_selected_columns <- c(
  "subject",
  "input_runs",
  "level",
  "contrast"
)
missing_selected_columns <- setdiff(
  required_selected_columns,
  names(selected_inputs)
)

if (length(missing_selected_columns) > 0) {
  stop(
    "L3-act-selected-inputs.tsv is missing: ",
    paste(missing_selected_columns, collapse = ", "),
    call. = FALSE
  )
}

selected_contrast <- selected_inputs |>
  dplyr::filter(contrast == "12")

if (nrow(selected_contrast) != length(subjects) ||
    anyDuplicated(selected_contrast$subject) > 0 ||
    !identical(selected_contrast$subject, subjects)) {
  stop(
    "Contrast-12 selected inputs do not align one-to-one with the subject ",
    "list.",
    call. = FALSE
  )
}

extraction_tag <- dplyr::case_when(
  selected_contrast$level == "L2" &
    selected_contrast$input_runs == "1+2" ~ "L2",
  selected_contrast$level == "L1" &
    selected_contrast$input_runs %in% c("1", "2") ~
    paste0("run-", selected_contrast$input_runs),
  TRUE ~ NA_character_
)

if (anyNA(extraction_tag)) {
  stop(
    "The selected-input manifest contains an unsupported level/run choice.",
    call. = FALSE
  )
}

extraction_files <- file.path(
  extraction_directory,
  paste0(
    "sub-",
    selected_contrast$subject,
    "_",
    extraction_tag,
    "_adixmspss_meants.txt"
  )
)
missing_extraction_files <- extraction_files[!file.exists(extraction_files)]

if (length(missing_extraction_files) > 0) {
  stop(
    "The following selected extraction files were not found:\n  ",
    paste(missing_extraction_files, collapse = "\n  "),
    call. = FALSE
  )
}

read_scalar_meants <- function(path) {
  values <- scan(path, what = numeric(), quiet = TRUE)

  if (length(values) != 1 || !is.finite(values)) {
    stop("Expected one finite mean value in: ", path, call. = FALSE)
  }

  values
}

brain_data <- tibble::tibble(
  sub_id = selected_contrast$subject,
  input_level = selected_contrast$level,
  input_runs = selected_contrast$input_runs,
  brain_activity = vapply(
    extraction_files,
    read_scalar_meants,
    numeric(1)
  )
)


# -----------------------------------------------------------------------------
# 3. Read covariates and recreate the imaging-model adjustment set
# -----------------------------------------------------------------------------

l3_covariates <- readxl::read_excel(
  l3_covariate_file,
  sheet = "covariates"
) |>
  dplyr::mutate(sub_id = as.character(sub_id))

analysis_covariates <- readxl::read_excel(
  analysis_covariate_file,
  sheet = "Data",
  col_types = c("text", "numeric", "text", rep("numeric", 4))
) |>
  dplyr::transmute(
    sub_id = as.character(rf1_id),
    sub_age,
    mspss_sum,
    adi_natrank
  )

required_l3_columns <- c(
  "sub_id",
  "software_version_syngo_mr_e11_mc",
  "software_version_syngo_mr_xa30_mc",
  "tsnr",
  "fd_mean",
  "adi_natrank_mc",
  "mspss_sum_mc",
  "sub_age_mc",
  "male"
)
missing_l3_columns <- setdiff(required_l3_columns, names(l3_covariates))

if (length(missing_l3_columns) > 0) {
  stop(
    "L3-act-covariates.xlsx is missing: ",
    paste(missing_l3_columns, collapse = ", "),
    call. = FALSE
  )
}

if (anyDuplicated(l3_covariates$sub_id) > 0 ||
    anyDuplicated(analysis_covariates$sub_id) > 0) {
  stop("A covariate input contains duplicate participant IDs.", call. = FALSE)
}

if (!setequal(subjects, l3_covariates$sub_id) ||
    !setequal(subjects, analysis_covariates$sub_id)) {
  stop(
    "Participant IDs do not match across the subject and covariate inputs.",
    call. = FALSE
  )
}

analysis_data <- brain_data |>
  dplyr::left_join(
    analysis_covariates,
    by = "sub_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    l3_covariates |>
      dplyr::select(dplyr::all_of(required_l3_columns)),
    by = "sub_id",
    relationship = "one-to-one"
  ) |>
  dplyr::rename(
    software_e11_mc = software_version_syngo_mr_e11_mc,
    software_xa30_mc = software_version_syngo_mr_xa30_mc
  )

numeric_columns <- setdiff(
  names(analysis_data),
  c("sub_id", "input_level", "input_runs")
)

if (any(!is.finite(as.matrix(analysis_data[numeric_columns])))) {
  stop("The assembled analysis data contain missing/non-finite values.", call. = FALSE)
}

center_tolerance <- 1e-6

if (max(abs(
  analysis_data$adi_natrank - mean(analysis_data$adi_natrank) -
    analysis_data$adi_natrank_mc
)) > center_tolerance ||
    max(abs(
      analysis_data$mspss_sum - mean(analysis_data$mspss_sum) -
        analysis_data$mspss_sum_mc
    )) > center_tolerance ||
    max(abs(
      analysis_data$sub_age - mean(analysis_data$sub_age) -
        analysis_data$sub_age_mc
    )) > center_tolerance) {
  stop("Raw and mean-centered covariates do not reconcile.", call. = FALSE)
}

standardize <- function(values, label) {
  standard_deviation <- stats::sd(values)

  if (!is.finite(standard_deviation) || standard_deviation <= 0) {
    stop(label, " has no usable variation.", call. = FALSE)
  }

  (values - mean(values)) / standard_deviation
}

analysis_data <- analysis_data |>
  dplyr::mutate(
    adi_z = standardize(adi_natrank, "ADI"),
    mspss_z = standardize(mspss_sum, "MSPSS"),
    age_z = standardize(sub_age, "Age"),
    tsnr_z = standardize(tsnr, "tSNR"),
    fd_mean_z = standardize(fd_mean, "Mean framewise displacement")
  )

brain_model <- stats::lm(
  brain_activity ~ adi_z * mspss_z +
    fd_mean_z + tsnr_z + software_e11_mc + software_xa30_mc + male + age_z,
  data = analysis_data
)


# -----------------------------------------------------------------------------
# 4. Calculate the Johnson-Neyman conditional effect
# -----------------------------------------------------------------------------

# At a standardized MSPSS value m, the conditional effect of a one-SD increase
# in ADI on the extracted mean z statistic is:
#   beta_ADI + m * beta_ADIxMSPSS
model_coefficients <- stats::coef(brain_model)
model_covariance <- stats::vcov(brain_model)

adi_term <- "adi_z"
interaction_term <- "adi_z:mspss_z"

if (!all(c(adi_term, interaction_term) %in% names(model_coefficients))) {
  stop(
    "The ADI main effect or ADI by MSPSS interaction was not found in the ",
    "fitted model.",
    call. = FALSE
  )
}

beta_adi <- model_coefficients[adi_term]
beta_interaction <- model_coefficients[interaction_term]
variance_adi <- model_covariance[adi_term, adi_term]
variance_interaction <- model_covariance[
  interaction_term,
  interaction_term
]
covariance_terms <- model_covariance[adi_term, interaction_term]
critical_value <- stats::qt(0.975, df = stats::df.residual(brain_model))

observed_mspss_range <- range(analysis_data$mspss_z)
mspss_grid_z <- seq(
  observed_mspss_range[1],
  observed_mspss_range[2],
  length.out = 1000
)

conditional_effect <- beta_adi + beta_interaction * mspss_grid_z
conditional_standard_error <- sqrt(
  variance_adi +
    2 * mspss_grid_z * covariance_terms +
    mspss_grid_z^2 * variance_interaction
)
confidence_interval_low <-
  conditional_effect - critical_value * conditional_standard_error
confidence_interval_high <-
  conditional_effect + critical_value * conditional_standard_error

mspss_mean <- mean(analysis_data$mspss_sum)
mspss_standard_deviation <- stats::sd(analysis_data$mspss_sum)

plot_data <- tibble::tibble(
  mspss_z = mspss_grid_z,
  mspss_raw = mspss_mean + mspss_grid_z * mspss_standard_deviation,
  conditional_effect = conditional_effect,
  ci_low = confidence_interval_low,
  ci_high = confidence_interval_high,
  significance = dplyr::if_else(
    ci_low > 0 | ci_high < 0,
    "Significant",
    "Not significant"
  )
) |>
  dplyr::mutate(
    segment = cumsum(
      significance != dplyr::lag(significance, default = significance[1])
    )
  )

quadratic_a <-
  beta_interaction^2 - critical_value^2 * variance_interaction
quadratic_b <-
  2 * beta_adi * beta_interaction -
  2 * critical_value^2 * covariance_terms
quadratic_c <- beta_adi^2 - critical_value^2 * variance_adi
root_tolerance <- sqrt(.Machine$double.eps)

if (abs(quadratic_a) < root_tolerance) {
  johnson_neyman_roots_z <- if (abs(quadratic_b) < root_tolerance) {
    numeric()
  } else {
    -quadratic_c / quadratic_b
  }
} else {
  discriminant <- quadratic_b^2 - 4 * quadratic_a * quadratic_c
  johnson_neyman_roots_z <- if (discriminant < 0) {
    numeric()
  } else {
    sort(c(
      (-quadratic_b - sqrt(discriminant)) / (2 * quadratic_a),
      (-quadratic_b + sqrt(discriminant)) / (2 * quadratic_a)
    ))
  }
}

observed_johnson_neyman_roots_z <- johnson_neyman_roots_z[
  johnson_neyman_roots_z >= observed_mspss_range[1] &
    johnson_neyman_roots_z <= observed_mspss_range[2]
]
observed_johnson_neyman_roots_raw <-
  mspss_mean +
  observed_johnson_neyman_roots_z * mspss_standard_deviation


# -----------------------------------------------------------------------------
# 5. Create and save the lab-styled Johnson-Neyman plot
# -----------------------------------------------------------------------------

johnson_neyman_plot <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = mspss_raw, y = conditional_effect)
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = ci_low,
      ymax = ci_high,
      fill = significance,
      group = segment
    ),
    alpha = 0.22,
    color = NA
  ) +
  ggplot2::geom_line(
    ggplot2::aes(color = significance, group = segment),
    linewidth = 1.4
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray25",
    linewidth = 0.7
  ) +
  ggplot2::geom_vline(
    xintercept = observed_johnson_neyman_roots_raw,
    linetype = "dotted",
    color = "gray25",
    linewidth = 0.7
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Significant" = "#28E2E5",
      "Not significant" = "gray55"
    ),
    breaks = c("Significant", "Not significant"),
    name = NULL
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Significant" = "#28E2E5",
      "Not significant" = "gray75"
    ),
    breaks = c("Significant", "Not significant"),
    name = NULL
  ) +
  ggplot2::labs(
    x = "Perceived Social Support (MSPSS)",
    y = paste0(
      "Slope of ADI Predicting rTPJ Activation\n",
      "Tracking Social Fairness (z)"
    ),
    title = "Johnson-Neyman Plot"
  ) +
  ggplot2::theme_classic(base_size = 18) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = 24,
      hjust = 0.5,
      face = "bold"
    ),
    axis.title = ggplot2::element_text(size = 26),
    axis.text = ggplot2::element_text(size = 22, color = "black"),
    axis.line = ggplot2::element_line(linewidth = 0.6),
    axis.ticks = ggplot2::element_line(linewidth = 0.6),
    legend.position = "top",
    legend.text = ggplot2::element_text(size = 18),
    panel.grid = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = output_file,
  plot = johnson_neyman_plot,
  device = grDevices::svg,
  width = 12,
  height = 10,
  units = "in",
  bg = "white"
)

interaction_p_value <- summary(brain_model)$coefficients[
  interaction_term,
  "Pr(>|t|)"
]
selection_counts <- table(extraction_tag)

message("Wrote ", normalizePath(output_file, winslash = "/"))
message(
  "Selected brain inputs: ",
  paste(
    paste0(names(selection_counts), "=", as.integer(selection_counts)),
    collapse = ", "
  )
)
message(
  "Model interaction: b = ",
  format(round(beta_interaction, 3), nsmall = 3),
  ", p = ",
  format.pval(interaction_p_value, digits = 3)
)

if (length(observed_johnson_neyman_roots_raw) == 0) {
  message("No Johnson-Neyman boundary falls within the observed MSPSS range.")
} else {
  message(
    "Observed-range Johnson-Neyman bound",
    if (length(observed_johnson_neyman_roots_raw) == 1) "" else "s",
    ": ",
    paste(
      format(
        round(observed_johnson_neyman_roots_raw, 3),
        nsmall = 3
      ),
      collapse = ", "
    )
  )
}
