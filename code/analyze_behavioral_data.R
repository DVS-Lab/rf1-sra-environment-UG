# Behavioral analyses for the rf1-sra-environment-UG project
#
# Run this script from the repository's code directory:
#   Rscript analyze_behavioral_data.R
#
# The script reads participant-level covariates from the derived analysis
# workbook and behavioral outcomes from build_behavioral_data.R. It writes
# aggregate statistical results and manuscript-ready figures to the external
# results-UG directory beside the repository. Keeping these generated results
# outside the repository prevents them from being committed to GitHub. It does
# not write participant-level covariate data.


# -----------------------------------------------------------------------------
# 1. Packages and file locations
# -----------------------------------------------------------------------------

required_packages <- c(
  "readr",
  "readxl",
  "dplyr",
  "tibble",
  "ggplot2",
  "scales",
  "lme4",
  "afex"
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

behavior_file <- "behavioral_data.csv"
slope_file <- "slope_df.csv"
subject_list_file <- "L3-act-subjects.txt"
covariate_file <- file.path(
  "..", "data", "derived", "UG_analysis_data_2026-11-04.xlsx"
)
results_directory <- file.path("..", "..", "results-UG")

input_files <- c(
  behavior_file,
  slope_file,
  subject_list_file,
  covariate_file
)

missing_input_files <- input_files[!file.exists(input_files)]

if (length(missing_input_files) > 0) {
  stop(
    "The following input files were not found. Run this script from the code ",
    "directory and confirm that the inputs exist:\n  ",
    paste(missing_input_files, collapse = "\n  "),
    call. = FALSE
  )
}

dir.create(results_directory, showWarnings = FALSE, recursive = TRUE)


# -----------------------------------------------------------------------------
# 2. Read and validate the analysis inputs
# -----------------------------------------------------------------------------

behavior <- readr::read_csv(
  behavior_file,
  col_types = readr::cols(
    sub_id = readr::col_character(),
    .default = readr::col_guess()
  ),
  show_col_types = FALSE
)

slopes <- readr::read_csv(
  slope_file,
  col_types = readr::cols(
    sub_id = readr::col_character(),
    .default = readr::col_double()
  ),
  show_col_types = FALSE
)

retained_subjects <- readr::read_lines(subject_list_file)
retained_subjects <- trimws(retained_subjects)
retained_subjects <- retained_subjects[nzchar(retained_subjects)]

covariates <- readxl::read_excel(
  covariate_file,
  sheet = "Data",
  # gender is retained as text, although it is not used in these analyses.
  col_types = c("text", "numeric", "text", rep("numeric", 4))
) |>
  dplyr::rename(sub_id = rf1_id)

required_behavior_columns <- c(
  "sub_id", "condition", "accept", "beh_fairness_num"
)
required_slope_columns <- c(
  "sub_id", "slope_all", "slope_social", "slope_nonsocial", "diff_slope"
)
required_covariate_columns <- c(
  "sub_id", "sub_age", "ecog_score", "mspss_sum", "adi_natrank",
  "pm25_mean_2013_2023"
)

missing_behavior_columns <- setdiff(required_behavior_columns, names(behavior))
missing_slope_columns <- setdiff(required_slope_columns, names(slopes))
missing_covariate_columns <- setdiff(required_covariate_columns, names(covariates))

if (length(missing_behavior_columns) > 0) {
  stop(
    "behavioral_data.csv is missing: ",
    paste(missing_behavior_columns, collapse = ", "),
    call. = FALSE
  )
}

if (length(missing_slope_columns) > 0) {
  stop(
    "slope_df.csv is missing: ",
    paste(missing_slope_columns, collapse = ", "),
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

if (anyDuplicated(retained_subjects) > 0) {
  stop("The retained subject list contains duplicate IDs.", call. = FALSE)
}

if (anyDuplicated(slopes$sub_id) > 0 || anyDuplicated(covariates$sub_id) > 0) {
  stop("Slope or covariate data contain duplicate participant IDs.", call. = FALSE)
}

behavior_subjects <- sort(unique(behavior$sub_id))
slope_subjects <- sort(slopes$sub_id)
covariate_subjects <- sort(covariates$sub_id)
retained_subjects_sorted <- sort(retained_subjects)

if (!identical(behavior_subjects, retained_subjects_sorted)) {
  stop(
    "Participant IDs in behavioral_data.csv do not exactly match ",
    "L3-act-subjects.txt.",
    call. = FALSE
  )
}

if (!identical(slope_subjects, retained_subjects_sorted)) {
  stop(
    "Participant IDs in slope_df.csv do not exactly match ",
    "L3-act-subjects.txt.",
    call. = FALSE
  )
}

if (!identical(covariate_subjects, retained_subjects_sorted)) {
  stop(
    "Participant IDs in the covariate workbook do not exactly match ",
    "L3-act-subjects.txt.",
    call. = FALSE
  )
}

if (any(!behavior$condition %in% c("social", "nonsocial"))) {
  stop("Unexpected condition values were found in behavioral_data.csv.", call. = FALSE)
}

if (any(!behavior$accept %in% c(0, 1)) || anyNA(behavior$accept)) {
  stop("The accept column must contain only complete 0/1 responses.", call. = FALSE)
}

if (anyNA(behavior[, required_behavior_columns])) {
  stop("Required behavioral variables contain missing values.", call. = FALSE)
}

if (anyNA(slopes[, required_slope_columns])) {
  stop("Required fairness-slope variables contain missing values.", call. = FALSE)
}

if (anyNA(covariates[, required_covariate_columns])) {
  stop("Required individual-difference variables contain missing values.", call. = FALSE)
}


# -----------------------------------------------------------------------------
# 3. Construct participant-level and trial-level analysis data
# -----------------------------------------------------------------------------

participant_acceptance <- behavior |>
  dplyr::group_by(sub_id) |>
  dplyr::summarise(
    percent_accept = 100 * mean(accept),
    percent_accept_social = 100 * mean(accept[condition == "social"]),
    percent_accept_nonsocial = 100 * mean(accept[condition == "nonsocial"]),
    percent_accept_diff = percent_accept_social - percent_accept_nonsocial,
    .groups = "drop"
  )

participant_data <- participant_acceptance |>
  dplyr::left_join(slopes, by = "sub_id", relationship = "one-to-one") |>
  dplyr::left_join(covariates, by = "sub_id", relationship = "one-to-one") |>
  dplyr::mutate(
    # Standardized predictors make effects comparable and reduce scaling issues.
    age_z = as.numeric(scale(sub_age)),
    ecog_z = as.numeric(scale(ecog_score)),
    adi_z = as.numeric(scale(adi_natrank)),
    pm25_z = as.numeric(scale(pm25_mean_2013_2023)),
    mspss_z = as.numeric(scale(mspss_sum))
  )

standardized_predictors <- c("age_z", "ecog_z", "adi_z", "pm25_z", "mspss_z")

if (any(!is.finite(as.matrix(participant_data[, standardized_predictors])))) {
  stop(
    "At least one individual-difference variable could not be standardized.",
    call. = FALSE
  )
}

# Fairness is expressed in 10-percentage-point units and centered at its grand
# mean. Therefore, its coefficient is the change associated with a 10-point
# increase in offer fairness, and the partner coefficient is evaluated at the
# average observed fairness level.
mean_fairness_10 <- mean(behavior$beh_fairness_num * 10)

trial_data <- behavior |>
  dplyr::mutate(
    partner_social = ifelse(condition == "social", 1, 0),
    fairness_10_c = beh_fairness_num * 10 - mean_fairness_10
  ) |>
  dplyr::left_join(
    participant_data |>
      dplyr::select(sub_id, dplyr::all_of(standardized_predictors)),
    by = "sub_id",
    relationship = "many-to-one"
  )


# -----------------------------------------------------------------------------
# 4. Acceptance rates by offer fairness and partner type
# -----------------------------------------------------------------------------

acceptance_cells <- behavior |>
  dplyr::mutate(
    fairness_level = factor(
      beh_fairness_num * 100,
      levels = c(5, 10, 25, 50),
      labels = c("5%", "10%", "25%", "50%")
    ),
    partner_type = factor(
      condition,
      levels = c("social", "nonsocial"),
      labels = c("Social", "Nonsocial")
    )
  ) |>
  dplyr::group_by(sub_id, fairness_level, partner_type) |>
  dplyr::summarise(
    acceptance_rate = 100 * mean(accept),
    .groups = "drop"
  )

expected_cell_count <- length(retained_subjects) * 4 * 2

if (nrow(acceptance_cells) != expected_cell_count) {
  stop(
    "At least one participant lacks a fairness-by-partner acceptance cell.",
    call. = FALSE
  )
}

acceptance_plot_data <- acceptance_cells |>
  dplyr::group_by(fairness_level, partner_type) |>
  dplyr::summarise(
    mean_acceptance_rate = mean(acceptance_rate),
    sd_acceptance_rate = stats::sd(acceptance_rate),
    n_participants = dplyr::n(),
    se_acceptance_rate = sd_acceptance_rate / sqrt(n_participants),
    .groups = "drop"
  )

readr::write_csv(
  acceptance_plot_data,
  file.path(results_directory, "acceptance_by_fairness_partner.csv")
)

# This repeated-measures ANOVA treats fairness categorically and tests the
# omnibus effects of fairness, partner type, and their interaction. Greenhouse-
# Geisser correction is applied to effects involving the four-level fairness
# factor. Partial eta squared is reported by afex.
acceptance_anova <- afex::aov_ez(
  id = "sub_id",
  dv = "acceptance_rate",
  data = acceptance_cells,
  within = c("fairness_level", "partner_type"),
  anova_table = list(correction = "GG", es = "pes")
)

acceptance_anova_results <- as.data.frame(acceptance_anova$anova_table) |>
  tibble::rownames_to_column("effect")

anova_p_column <- grep(
  "^Pr\\(>F",
  names(acceptance_anova_results),
  value = TRUE
)

if (length(anova_p_column) != 1) {
  stop("The repeated-measures ANOVA p-value column could not be identified.")
}

names(acceptance_anova_results)[
  names(acceptance_anova_results) == anova_p_column
] <- "p_value"

acceptance_anova_results <- acceptance_anova_results |>
  dplyr::mutate(
    p_fdr_bh = stats::p.adjust(p_value, method = "BH"),
    p_adjustment_family = "Three omnibus repeated-measures ANOVA effects"
  )

readr::write_csv(
  acceptance_anova_results,
  file.path(results_directory, "acceptance_rm_anova.csv")
)

# A trial-level logistic mixed model supplies a directional, continuous test of
# whether acceptance increases with offer fairness. It also tests the partner
# effect and whether fairness sensitivity differs by partner. The random-effects
# terms are uncorrelated (||) to improve stability while retaining participant-
# specific fairness and partner effects.
core_acceptance_glmm <- lme4::glmer(
  accept ~ fairness_10_c * partner_social +
    (1 + fairness_10_c + partner_social || sub_id),
  data = trial_data,
  family = stats::binomial,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

core_coefficient_table <- as.data.frame(
  summary(core_acceptance_glmm)$coefficients
) |>
  tibble::rownames_to_column("term") |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::transmute(
    term,
    test = dplyr::case_when(
      term == "fairness_10_c" ~
        "Acceptance increases with offer fairness",
      term == "partner_social" ~
        "Main effect of partner type at mean fairness",
      term == "fairness_10_c:partner_social" ~
        "Difference in fairness sensitivity by partner type",
      TRUE ~ term
    ),
    log_odds_estimate = Estimate,
    std_error = `Std. Error`,
    z_value = `z value`,
    p_value = `Pr(>|z|)`,
    log_odds_ci_low = log_odds_estimate - stats::qnorm(0.975) * std_error,
    log_odds_ci_high = log_odds_estimate + stats::qnorm(0.975) * std_error,
    odds_ratio = exp(log_odds_estimate),
    odds_ratio_ci_low = exp(log_odds_ci_low),
    odds_ratio_ci_high = exp(log_odds_ci_high),
    p_fdr_bh = stats::p.adjust(p_value, method = "BH"),
    p_adjustment_family = "Three fixed effects in the core acceptance GLMM",
    n_trials = stats::nobs(core_acceptance_glmm),
    n_participants = dplyr::n_distinct(trial_data$sub_id),
    singular_fit = lme4::isSingular(core_acceptance_glmm, tol = 1e-4),
    model = paste(deparse(stats::formula(core_acceptance_glmm)), collapse = " ")
  )

readr::write_csv(
  core_coefficient_table,
  file.path(results_directory, "acceptance_core_glmm.csv")
)

# The slope comparison is also reported directly in the units produced by
# build_behavioral_data.R. diff_slope is social minus nonsocial.
slope_partner_test <- stats::t.test(
  participant_data$slope_social,
  participant_data$slope_nonsocial,
  paired = TRUE
)

slope_partner_results <- tibble::tibble(
  test = "Paired comparison of social and nonsocial fairness slopes",
  mean_social_slope = mean(participant_data$slope_social),
  mean_nonsocial_slope = mean(participant_data$slope_nonsocial),
  mean_difference_social_minus_nonsocial = mean(participant_data$diff_slope),
  difference_ci_low = unname(slope_partner_test$conf.int[1]),
  difference_ci_high = unname(slope_partner_test$conf.int[2]),
  t_value = unname(slope_partner_test$statistic),
  degrees_of_freedom = unname(slope_partner_test$parameter),
  p_value = slope_partner_test$p.value,
  p_fdr_bh = stats::p.adjust(slope_partner_test$p.value, method = "BH"),
  n_participants = nrow(participant_data)
)

readr::write_csv(
  slope_partner_results,
  file.path(results_directory, "fairness_slope_partner_comparison.csv")
)


# -----------------------------------------------------------------------------
# 5. Acceptance-by-fairness figure
# -----------------------------------------------------------------------------

acceptance_plot <- ggplot2::ggplot(
  acceptance_plot_data,
  ggplot2::aes(
    x = fairness_level,
    y = mean_acceptance_rate,
    fill = partner_type
  )
) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.78),
    width = 0.70,
    color = "black",
    linewidth = 0.25
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = mean_acceptance_rate - se_acceptance_rate,
      ymax = mean_acceptance_rate + se_acceptance_rate
    ),
    position = ggplot2::position_dodge(width = 0.78),
    width = 0.18,
    linewidth = 0.45
  ) +
  ggplot2::scale_fill_manual(
    values = c("Social" = "#E65D2C", "Nonsocial" = "#0870B7"),
    name = "Partner type"
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = scales::label_number(suffix = "%"),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::labs(
    x = "Offer fairness",
    y = "Acceptance rate"
  ) +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    legend.position = "top",
    legend.title = ggplot2::element_text(face = "bold"),
    axis.title = ggplot2::element_text(face = "bold"),
    axis.text = ggplot2::element_text(color = "black")
  )

ggplot2::ggsave(
  filename = file.path(
    results_directory,
    "acceptance_by_fairness_partner.tiff"
  ),
  plot = acceptance_plot,
  device = grDevices::tiff,
  width = 6.5,
  height = 5,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

ggplot2::ggsave(
  filename = file.path(
    results_directory,
    "acceptance_by_fairness_partner.svg"
  ),
  plot = acceptance_plot,
  device = grDevices::svg,
  width = 6.5,
  height = 5,
  units = "in"
)


# -----------------------------------------------------------------------------
# 6. Individual-difference model definitions
# -----------------------------------------------------------------------------

# Each interaction is tested in its own model. Interaction models include both
# constituent main effects automatically through the * operator. This gives the
# six requested combinations of the two environmental measures with the three
# person-level measures.
model_specifications <- tibble::tribble(
  ~model_id, ~model_type, ~predictor_1, ~predictor_2,
  "age", "main_effect", "age_z", NA_character_,
  "ecog", "main_effect", "ecog_z", NA_character_,
  "adi", "main_effect", "adi_z", NA_character_,
  "pm25", "main_effect", "pm25_z", NA_character_,
  "mspss", "main_effect", "mspss_z", NA_character_,
  "adi_by_ecog", "interaction", "adi_z", "ecog_z",
  "adi_by_mspss", "interaction", "adi_z", "mspss_z",
  "adi_by_age", "interaction", "adi_z", "age_z",
  "pm25_by_ecog", "interaction", "pm25_z", "ecog_z",
  "pm25_by_mspss", "interaction", "pm25_z", "mspss_z",
  "pm25_by_age", "interaction", "pm25_z", "age_z"
)

acceptance_outcomes <- tibble::tribble(
  ~outcome_label, ~outcome_variable,
  "Agnostic (all trials)", "percent_accept",
  "Social trials", "percent_accept_social",
  "Social minus nonsocial", "percent_accept_diff"
)

slope_outcomes <- tibble::tribble(
  ~outcome_label, ~outcome_variable,
  "Agnostic (all trials)", "slope_all",
  "Social trials", "slope_social",
  "Social minus nonsocial", "diff_slope"
)


# -----------------------------------------------------------------------------
# 7. Repeated helpers for extracting the requested model effects
# -----------------------------------------------------------------------------

# This operation is repeated for every participant-level model, so a small
# extraction helper keeps the loop below readable and ensures identical output.
extract_lm_effect <- function(model, target_term) {
  coefficient_table <- summary(model)$coefficients

  if (!target_term %in% rownames(coefficient_table)) {
    stop("Target term was not found in model: ", target_term, call. = FALSE)
  }

  confidence_interval <- stats::confint(model, parm = target_term, level = 0.95)
  model_summary <- summary(model)

  tibble::tibble(
    term = target_term,
    estimate = coefficient_table[target_term, "Estimate"],
    std_error = coefficient_table[target_term, "Std. Error"],
    t_value = coefficient_table[target_term, "t value"],
    degrees_of_freedom = stats::df.residual(model),
    p_value = coefficient_table[target_term, "Pr(>|t|)"],
    ci_low = confidence_interval[1],
    ci_high = confidence_interval[2],
    r_squared = model_summary$r.squared,
    adjusted_r_squared = model_summary$adj.r.squared,
    n_participants = stats::nobs(model),
    model = paste(deparse(stats::formula(model)), collapse = " ")
  )
}

# This operation is repeated for all trial-level individual-difference models.
# A failed fit is retained as a clearly labeled result row instead of preventing
# all other analyses from being written.
fit_and_extract_glmm_effect <- function(
    analysis_data,
    model_formula,
    target_term) {
  warning_messages <- character()

  fitted_model <- tryCatch(
    withCallingHandlers(
      lme4::glmer(
        model_formula,
        data = analysis_data,
        family = stats::binomial,
        control = lme4::glmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 200000)
        )
      ),
      warning = function(warning_condition) {
        warning_messages <<- c(warning_messages, conditionMessage(warning_condition))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error_condition) error_condition
  )

  formula_text <- paste(deparse(model_formula), collapse = " ")

  if (inherits(fitted_model, "error")) {
    return(
      tibble::tibble(
        term = target_term,
        log_odds_estimate = NA_real_,
        std_error = NA_real_,
        z_value = NA_real_,
        p_value = NA_real_,
        log_odds_ci_low = NA_real_,
        log_odds_ci_high = NA_real_,
        odds_ratio = NA_real_,
        odds_ratio_ci_low = NA_real_,
        odds_ratio_ci_high = NA_real_,
        aic = NA_real_,
        n_trials = nrow(analysis_data),
        n_participants = dplyr::n_distinct(analysis_data$sub_id),
        singular_fit = NA,
        convergence_message = NA_character_,
        warnings = NA_character_,
        error = conditionMessage(fitted_model),
        model = formula_text
      )
    )
  }

  coefficient_table <- summary(fitted_model)$coefficients

  if (!target_term %in% rownames(coefficient_table)) {
    # R may reorder the variables in an interaction coefficient name. Match
    # interaction terms by their components so age_z:partner_social and
    # partner_social:age_z are treated as the same requested effect.
    requested_components <- sort(strsplit(target_term, ":", fixed = TRUE)[[1]])
    candidate_terms <- rownames(coefficient_table)
    matching_terms <- candidate_terms[
      vapply(
        strsplit(candidate_terms, ":", fixed = TRUE),
        function(components) identical(sort(components), requested_components),
        logical(1)
      )
    ]

    if (length(matching_terms) != 1) {
      stop(
        "Target term was not found in fitted GLMM: ",
        target_term,
        "\nModel: ",
        formula_text,
        call. = FALSE
      )
    }

    target_term <- matching_terms
  }

  estimate <- coefficient_table[target_term, "Estimate"]
  standard_error <- coefficient_table[target_term, "Std. Error"]
  confidence_interval <- estimate + c(-1, 1) * stats::qnorm(0.975) * standard_error
  convergence_messages <- fitted_model@optinfo$conv$lme4$messages

  tibble::tibble(
    term = target_term,
    log_odds_estimate = estimate,
    std_error = standard_error,
    z_value = coefficient_table[target_term, "z value"],
    p_value = coefficient_table[target_term, "Pr(>|z|)"],
    log_odds_ci_low = confidence_interval[1],
    log_odds_ci_high = confidence_interval[2],
    odds_ratio = exp(estimate),
    odds_ratio_ci_low = exp(confidence_interval[1]),
    odds_ratio_ci_high = exp(confidence_interval[2]),
    aic = stats::AIC(fitted_model),
    n_trials = stats::nobs(fitted_model),
    n_participants = dplyr::n_distinct(analysis_data$sub_id),
    singular_fit = lme4::isSingular(fitted_model, tol = 1e-4),
    convergence_message = if (is.null(convergence_messages)) {
      NA_character_
    } else {
      paste(convergence_messages, collapse = " | ")
    },
    warnings = if (length(warning_messages) == 0) {
      NA_character_
    } else {
      paste(unique(warning_messages), collapse = " | ")
    },
    error = NA_character_,
    model = formula_text
  )
}


# -----------------------------------------------------------------------------
# 8. Participant-level linear models of acceptance rates
# -----------------------------------------------------------------------------

acceptance_lm_results <- list()
result_number <- 1

for (outcome_number in seq_len(nrow(acceptance_outcomes))) {
  outcome_label <- acceptance_outcomes$outcome_label[outcome_number]
  outcome_variable <- acceptance_outcomes$outcome_variable[outcome_number]

  for (model_number in seq_len(nrow(model_specifications))) {
    specification <- model_specifications[model_number, ]

    if (specification$model_type == "main_effect") {
      predictor_expression <- specification$predictor_1
      target_term <- specification$predictor_1
    } else {
      predictor_expression <- paste0(
        specification$predictor_1,
        " * ",
        specification$predictor_2
      )
      target_term <- paste0(
        specification$predictor_1,
        ":",
        specification$predictor_2
      )
    }

    model_formula <- stats::as.formula(
      paste(outcome_variable, "~", predictor_expression)
    )
    fitted_model <- stats::lm(model_formula, data = participant_data)

    acceptance_lm_results[[result_number]] <- extract_lm_effect(
      fitted_model,
      target_term
    ) |>
      dplyr::mutate(
        analysis_level = "Participant-level linear model of percent accepted",
        outcome = outcome_label,
        model_id = specification$model_id,
        model_type = specification$model_type,
        .before = 1
      )

    result_number <- result_number + 1
  }
}

acceptance_lm_results <- dplyr::bind_rows(acceptance_lm_results) |>
  dplyr::mutate(
    # One FDR family contains all 33 requested acceptance-rate LM target tests.
    p_fdr_bh = stats::p.adjust(p_value, method = "BH"),
    p_adjustment_family =
      "All 33 individual-difference acceptance-rate LM target tests"
  )

readr::write_csv(
  acceptance_lm_results,
  file.path(results_directory, "individual_difference_acceptance_lm.csv")
)


# -----------------------------------------------------------------------------
# 9. Trial-level binomial mixed models of acceptance
# -----------------------------------------------------------------------------

acceptance_glmm_results <- list()
result_number <- 1

for (outcome_number in seq_len(nrow(acceptance_outcomes))) {
  outcome_label <- acceptance_outcomes$outcome_label[outcome_number]

  if (outcome_label == "Social trials") {
    analysis_data <- trial_data |>
      dplyr::filter(condition == "social")
  } else {
    analysis_data <- trial_data
  }

  for (model_number in seq_len(nrow(model_specifications))) {
    specification <- model_specifications[model_number, ]

    if (specification$model_type == "main_effect") {
      individual_difference_expression <- specification$predictor_1
      individual_difference_term <- specification$predictor_1
    } else {
      individual_difference_expression <- paste0(
        specification$predictor_1,
        " * ",
        specification$predictor_2
      )
      individual_difference_term <- paste0(
        specification$predictor_1,
        ":",
        specification$predictor_2
      )
    }

    if (outcome_label == "Social minus nonsocial") {
      fixed_effects <- paste0(
        "fairness_10_c * partner_social + ",
        individual_difference_expression,
        " * partner_social"
      )
      target_term <- paste0(
        individual_difference_term,
        ":partner_social"
      )
      random_effects <- "(1 + fairness_10_c + partner_social || sub_id)"
    } else {
      fixed_effects <- paste0(
        "fairness_10_c + ",
        individual_difference_expression
      )
      target_term <- individual_difference_term
      random_effects <- "(1 + fairness_10_c || sub_id)"
    }

    model_formula <- stats::as.formula(
      paste("accept ~", fixed_effects, "+", random_effects)
    )

    acceptance_glmm_results[[result_number]] <-
      fit_and_extract_glmm_effect(
        analysis_data,
        model_formula,
        target_term
      ) |>
      dplyr::mutate(
        analysis_level = "Trial-level binomial mixed model",
        outcome = outcome_label,
        model_id = specification$model_id,
        model_type = specification$model_type,
        .before = 1
      )

    result_number <- result_number + 1
  }
}

acceptance_glmm_results <- dplyr::bind_rows(acceptance_glmm_results) |>
  dplyr::mutate(
    # One FDR family contains all 33 requested acceptance GLMM target tests.
    p_fdr_bh = stats::p.adjust(p_value, method = "BH"),
    p_adjustment_family =
      "All 33 individual-difference acceptance GLMM target tests"
  )

readr::write_csv(
  acceptance_glmm_results,
  file.path(results_directory, "individual_difference_acceptance_glmm.csv")
)


# -----------------------------------------------------------------------------
# 10. Participant-level linear models of fairness sensitivity slopes
# -----------------------------------------------------------------------------

slope_lm_results <- list()
result_number <- 1

for (outcome_number in seq_len(nrow(slope_outcomes))) {
  outcome_label <- slope_outcomes$outcome_label[outcome_number]
  outcome_variable <- slope_outcomes$outcome_variable[outcome_number]

  for (model_number in seq_len(nrow(model_specifications))) {
    specification <- model_specifications[model_number, ]

    if (specification$model_type == "main_effect") {
      predictor_expression <- specification$predictor_1
      target_term <- specification$predictor_1
    } else {
      predictor_expression <- paste0(
        specification$predictor_1,
        " * ",
        specification$predictor_2
      )
      target_term <- paste0(
        specification$predictor_1,
        ":",
        specification$predictor_2
      )
    }

    model_formula <- stats::as.formula(
      paste(outcome_variable, "~", predictor_expression)
    )
    fitted_model <- stats::lm(model_formula, data = participant_data)

    slope_lm_results[[result_number]] <- extract_lm_effect(
      fitted_model,
      target_term
    ) |>
      dplyr::mutate(
        analysis_level = "Participant-level linear model of fairness slope",
        outcome = outcome_label,
        model_id = specification$model_id,
        model_type = specification$model_type,
        .before = 1
      )

    result_number <- result_number + 1
  }
}

slope_lm_results <- dplyr::bind_rows(slope_lm_results) |>
  dplyr::mutate(
    # One FDR family contains all 33 requested fairness-slope LM target tests.
    p_fdr_bh = stats::p.adjust(p_value, method = "BH"),
    p_adjustment_family =
      "All 33 individual-difference fairness-slope LM target tests"
  )

readr::write_csv(
  slope_lm_results,
  file.path(
    results_directory,
    "individual_difference_fairness_slopes_lm.csv"
  )
)


# -----------------------------------------------------------------------------
# 11. Pearson correlations among individual-difference measures
# -----------------------------------------------------------------------------

correlation_variables <- c(
  "Age" = "sub_age",
  "ECog" = "ecog_score",
  "ADI" = "adi_natrank",
  "PM2.5" = "pm25_mean_2013_2023",
  "MSPSS" = "mspss_sum"
)

correlation_pairs <- utils::combn(
  names(correlation_variables),
  2,
  simplify = FALSE
)

correlation_results <- vector("list", length(correlation_pairs))

for (pair_number in seq_along(correlation_pairs)) {
  variable_labels <- correlation_pairs[[pair_number]]
  variable_1 <- correlation_variables[[variable_labels[1]]]
  variable_2 <- correlation_variables[[variable_labels[2]]]

  correlation_test <- stats::cor.test(
    participant_data[[variable_1]],
    participant_data[[variable_2]],
    method = "pearson"
  )

  correlation_results[[pair_number]] <- tibble::tibble(
    variable_1 = variable_labels[1],
    variable_2 = variable_labels[2],
    pearson_r = unname(correlation_test$estimate),
    ci_low = unname(correlation_test$conf.int[1]),
    ci_high = unname(correlation_test$conf.int[2]),
    t_value = unname(correlation_test$statistic),
    degrees_of_freedom = unname(correlation_test$parameter),
    p_value = correlation_test$p.value,
    n_participants = correlation_test$parameter + 2
  )
}

correlation_results <- dplyr::bind_rows(correlation_results) |>
  dplyr::mutate(
    p_fdr_bh = stats::p.adjust(p_value, method = "BH"),
    p_adjustment_family =
      "All 10 pairwise correlations among individual-difference measures"
  )

readr::write_csv(
  correlation_results,
  file.path(results_directory, "individual_difference_correlations.csv")
)


# -----------------------------------------------------------------------------
# 12. Completion summary
# -----------------------------------------------------------------------------

cat(
  "Behavioral analyses completed successfully.\n",
  "Participants: ", nrow(participant_data), "\n",
  "Responded trials: ", nrow(trial_data), "\n",
  "Results directory: ", normalizePath(results_directory), "\n\n",
  sep = ""
)

print(core_coefficient_table |>
  dplyr::select(test, odds_ratio, odds_ratio_ci_low, odds_ratio_ci_high,
                p_value, p_fdr_bh))

