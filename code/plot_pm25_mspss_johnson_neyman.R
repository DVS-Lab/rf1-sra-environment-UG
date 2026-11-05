# Johnson-Neyman plot for the PM2.5 by MSPSS interaction predicting acceptance
# on social trials in the rf1-sra-environment-UG project.
#
# Run this script from the repository's code directory:
#   Rscript plot_pm25_mspss_johnson_neyman.R
#
# The model matches the age- and gender-adjusted social-trial GLMM in
# analyze_behavioral_data.R. The figure is written to this code directory as:
#   pm25_mspss_social_acceptance_johnson_neyman.svg
#
# The plot follows the lab style used in
# Codex/examples/behavioral analysis v3.Rmd: a classic theme, bold centered
# title, large axis text, a white background, and cyan for the
# statistically significant Johnson-Neyman region.


# -----------------------------------------------------------------------------
# 1. Packages and file locations
# -----------------------------------------------------------------------------

required_packages <- c(
  "readr",
  "readxl",
  "dplyr",
  "tibble",
  "ggplot2",
  "lme4"
)

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
  dirname(
    normalizePath(
      sub("^--file=", "", script_argument),
      winslash = "/",
      mustWork = TRUE
    )
  )
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

behavior_file <- file.path(script_directory, "behavioral_data.csv")
covariate_file <- file.path(
  script_directory,
  "..",
  "data",
  "derived",
  "UG_analysis_data_2026-11-04.xlsx"
)
output_file <- file.path(
  script_directory,
  "pm25_mspss_social_acceptance_johnson_neyman.svg"
)

input_files <- c(behavior_file, covariate_file)
missing_input_files <- input_files[!file.exists(input_files)]

if (length(missing_input_files) > 0) {
  stop(
    "The following required input files were not found:\n  ",
    paste(missing_input_files, collapse = "\n  "),
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# 2. Read and validate the analysis data
# -----------------------------------------------------------------------------

behavior <- readr::read_csv(
  behavior_file,
  col_types = readr::cols(
    sub_id = readr::col_character(),
    .default = readr::col_guess()
  ),
  show_col_types = FALSE
)

covariates <- readxl::read_excel(
  covariate_file,
  sheet = "Data",
  col_types = c("text", "numeric", "text", rep("numeric", 4))
) |>
  dplyr::rename(sub_id = rf1_id)

required_behavior_columns <- c(
  "sub_id",
  "condition",
  "accept",
  "beh_fairness_num"
)
required_covariate_columns <- c(
  "sub_id",
  "sub_age",
  "gender",
  "mspss_sum",
  "pm25_mean_2013_2023"
)

missing_behavior_columns <- setdiff(
  required_behavior_columns,
  names(behavior)
)
missing_covariate_columns <- setdiff(
  required_covariate_columns,
  names(covariates)
)

if (length(missing_behavior_columns) > 0) {
  stop(
    "behavioral_data.csv is missing: ",
    paste(missing_behavior_columns, collapse = ", "),
    call. = FALSE
  )
}

if (length(missing_covariate_columns) > 0) {
  stop(
    "The covariate workbook is missing: ",
    paste(missing_covariate_columns, collapse = ", "),
    call. = FALSE
  )
}

if (anyDuplicated(covariates$sub_id) > 0) {
  stop("The covariate workbook contains duplicate participant IDs.", call. = FALSE)
}

if (any(!behavior$accept %in% c(0, 1)) || anyNA(behavior$accept)) {
  stop("The accept column must contain only complete 0/1 responses.", call. = FALSE)
}

if (any(!behavior$condition %in% c("social", "nonsocial"))) {
  stop("Unexpected condition values were found in behavioral_data.csv.", call. = FALSE)
}


# -----------------------------------------------------------------------------
# 3. Recreate the adjusted social-trial GLMM
# -----------------------------------------------------------------------------

participant_data <- covariates |>
  dplyr::mutate(
    gender_code = trimws(as.character(gender)),
    gender_female = dplyr::case_when(
      gender_code == "0" ~ 0,
      gender_code == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    age_z = as.numeric(scale(sub_age)),
    pm25_z = as.numeric(scale(pm25_mean_2013_2023)),
    mspss_z = as.numeric(scale(mspss_sum)),
    gender_female_c = gender_female - mean(gender_female)
  )

if (anyNA(participant_data$gender_female)) {
  stop(
    "Gender must use the documented binary source codes 0 (male) and 1 ",
    "(female).",
    call. = FALSE
  )
}

if (!setequal(unique(behavior$sub_id), participant_data$sub_id)) {
  stop(
    "Participant IDs do not match between the behavioral and covariate data.",
    call. = FALSE
  )
}

# Match the fairness centering used in analyze_behavioral_data.R. One unit of
# fairness_10_c represents a 10-percentage-point increase in offer fairness.
mean_fairness_10 <- mean(behavior$beh_fairness_num * 10)

social_trial_data <- behavior |>
  dplyr::filter(condition == "social") |>
  dplyr::mutate(
    fairness_10_c = beh_fairness_num * 10 - mean_fairness_10
  ) |>
  dplyr::left_join(
    participant_data |>
      dplyr::select(
        sub_id,
        age_z,
        pm25_z,
        mspss_z,
        gender_female_c
      ),
    by = "sub_id",
    relationship = "many-to-one"
  )

adjusted_social_model <- lme4::glmer(
  accept ~ fairness_10_c + pm25_z * mspss_z + age_z + gender_female_c +
    (1 + fairness_10_c || sub_id),
  data = social_trial_data,
  family = stats::binomial,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

if (lme4::isSingular(adjusted_social_model, tol = 1e-4)) {
  warning("The adjusted social-trial GLMM is singular.", call. = FALSE)
}

convergence_messages <- adjusted_social_model@optinfo$conv$lme4$messages
if (!is.null(convergence_messages)) {
  warning(
    "The adjusted social-trial GLMM reported: ",
    paste(convergence_messages, collapse = " | "),
    call. = FALSE
  )
}


# -----------------------------------------------------------------------------
# 4. Calculate the Johnson-Neyman conditional effect
# -----------------------------------------------------------------------------

# At a given standardized MSPSS value m, the conditional fixed effect of PM2.5
# on the log odds of accepting a social offer is:
#   beta_PM2.5 + m * beta_PM2.5xMSPSS
# Its variance is calculated from the matching elements of the fixed-effect
# variance-covariance matrix. The Johnson-Neyman bounds occur where the 95%
# confidence interval for this conditional effect touches zero.
fixed_effects <- lme4::fixef(adjusted_social_model)
fixed_covariance <- as.matrix(stats::vcov(adjusted_social_model))

pm25_term <- "pm25_z"
interaction_term <- "pm25_z:mspss_z"

if (!all(c(pm25_term, interaction_term) %in% names(fixed_effects))) {
  stop(
    "The PM2.5 main effect or PM2.5 by MSPSS interaction was not found in ",
    "the fitted model.",
    call. = FALSE
  )
}

beta_pm25 <- fixed_effects[pm25_term]
beta_interaction <- fixed_effects[interaction_term]
variance_pm25 <- fixed_covariance[pm25_term, pm25_term]
variance_interaction <- fixed_covariance[interaction_term, interaction_term]
covariance_terms <- fixed_covariance[pm25_term, interaction_term]
critical_value <- stats::qnorm(0.975)

observed_mspss_range <- range(participant_data$mspss_z)
mspss_grid_z <- seq(
  observed_mspss_range[1],
  observed_mspss_range[2],
  length.out = 1000
)

conditional_effect <- beta_pm25 + beta_interaction * mspss_grid_z
conditional_standard_error <- sqrt(
  variance_pm25 +
    2 * mspss_grid_z * covariance_terms +
    mspss_grid_z^2 * variance_interaction
)
confidence_interval_low <-
  conditional_effect - critical_value * conditional_standard_error
confidence_interval_high <-
  conditional_effect + critical_value * conditional_standard_error

mspss_mean <- mean(participant_data$mspss_sum)
mspss_standard_deviation <- stats::sd(participant_data$mspss_sum)

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

# Solve the Johnson-Neyman quadratic exactly. Roots outside the observed MSPSS
# range are not drawn because they would represent extrapolation.
quadratic_a <-
  beta_interaction^2 - critical_value^2 * variance_interaction
quadratic_b <-
  2 * beta_pm25 * beta_interaction -
  2 * critical_value^2 * covariance_terms
quadratic_c <- beta_pm25^2 - critical_value^2 * variance_pm25

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
    sort(
      c(
        (-quadratic_b - sqrt(discriminant)) / (2 * quadratic_a),
        (-quadratic_b + sqrt(discriminant)) / (2 * quadratic_a)
      )
    )
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
  ggplot2::geom_rug(
    data = participant_data,
    ggplot2::aes(x = mspss_sum),
    inherit.aes = FALSE,
    sides = "b",
    color = "gray35",
    alpha = 0.35,
    linewidth = 0.45
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
    y = paste(
      "Conditional Effect of PM2.5 on Social Acceptance",
      "(Log Odds)"
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

message("Wrote ", normalizePath(output_file, winslash = "/"))
message(
  "Model interaction: b = ",
  format(round(beta_interaction, 3), nsmall = 3),
  ", p = ",
  format.pval(
    summary(adjusted_social_model)$coefficients[
      interaction_term,
      "Pr(>|z|)"
    ],
    digits = 3
  )
)

if (length(observed_johnson_neyman_roots_raw) == 0) {
  message("No Johnson-Neyman boundary falls within the observed MSPSS range.")
} else {
  message(
    "Observed-range Johnson-Neyman bound", 
    if (length(observed_johnson_neyman_roots_raw) == 1) "" else "s",
    ": ",
    paste(
      format(round(observed_johnson_neyman_roots_raw, 3), nsmall = 3),
      collapse = ", "
    )
  )
}
