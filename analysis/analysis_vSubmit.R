# ============================================================
# JudgmentBench analysis script (submission version)
# ============================================================
#
# Reproduces the main analysis for the paper "JudgmentBench:
# Comparing Rubric and Preference Evaluation for Quality Assessment".
#
# Usage
# -----
#   Rscript analysis_vSubmit.R
#
# Configure via environment variables (all optional):
#   EAH_JB_DIR
#       Path to the released JudgmentBench result-dataset directory
#       (the folder containing annotators.csv, tasks.csv, etc.).
#       If unset, the script searches the current working directory
#       and the script directory for a "result-dataset" subfolder.
#   EAH_ANALYSIS_OUTPUT_DIR
#       Where to write figures and tables. Defaults to the script
#       directory.
#   EAH_N_BOOT
#       Number of bootstrap replicates (default 2000).
#   EAH_SAVE_PLOT
#       "true" (default) or "false" to enable/disable PNG output.
#   EAH_BOOTSTRAP_PLOT_PATH
#       Optional explicit path for the bootstrap PNG.
#
# Required CSVs in EAH_JB_DIR:
#   annotators.csv, tasks.csv, outputs.csv, assignment_records.csv,
#   annotations_rubric.csv, annotations_comparative_judgment.csv,
#   rubric_item_scores.csv, plus the GPT-5.4 and GPT-5.4-mini
#   autograder analogues (autograder_*_annotations_*.csv etc.).
#
# Purpose
# -------
# Perform the collapsed quality-level analysis for either:
#   * llm_both:    LLM results where the same pipeline-task unit has
#                  both rubric and CJ evaluations
#   * human_single: human results where each evaluator-task unit has
#                  only one assigned method
#
# Both modes use the same study-level estimand defined in the
# Appendix.
#
# Collapsed quality-level analysis
# --------------------------------
# For each task:
#   * Comparative judgments:
#       - collapse completed cross-quality-level comparisons to the
#         quality-level signal
#       - fit a 3-item Bradley-Terry model on the quality levels
#         intermediate, good, excellent
#       - compare the recovered ordering to the gold ordering using
#         Spearman's rho
#
#   * Rubrics:
#       - average rubric total points within task and quality level
#       - compare the recovered ordering to the gold ordering using
#         Spearman's rho
#
# Study-level comparison
# ----------------------
# Across all estimable tasks within each method:
#   mean_rho_rubric =
#     average task-level Spearman rho under Rubrics
#
#   mean_rho_cj =
#     average task-level Spearman rho under Comparative Judgments
#
#   D̂ = mean_rho_cj - mean_rho_rubric
#
# Inference layer
# ---------------
# This script uses a cluster-first hierarchical bootstrap. In human_single
# mode, the cluster is a lawyer, identified by the assigned pipeline instance:
#   pipeline_instance_id = normalized pipeline_round + pipeline_number
#
# Round 1 exports have blank pipeline_round values, so those are normalized to
# "round1" before constructing the pipeline instance identifier. This matters
# because pipeline_number is reused across study rounds.
#
# In llm_both mode, the same code path treats the pipeline instance as the
# pipeline cluster.
#
# Bootstrap units:
#   * human_single: resample lawyers with replacement
#   * llm_both: resample pipeline instances with replacement
#   * within each sampled cluster, resample completed task blocks with
#     replacement
#
# Bootstrap replicate:
#   1. resample clusters with replacement
#   2. within each sampled cluster, resample completed task blocks with
#      replacement
#   3. recompute task-level method summaries
#   4. recompute average comparative-judgment rho, average rubric rho, and D̂
#   5. use the standard deviation of bootstrap D̂ values as the bootstrap
#      standard error for a confidence interval centered on observed D̂
#
# Output behavior
# ---------------
# Prints a concise console summary and writes:
#   * figure1_main_results.png: 2 x 2 main-result panel with task-level rho
#     and per-evaluation time histograms
#   * figure2_bootstrap_distribution.png: faceted human and LLM bootstrap
#     sampling-error histograms for D-hat
#   * figure3_quality_level_score_distributions.png: rubric score
#     distributions by quality level (shares of rubric positive maximum)
#   * figure4_experience_diagnostic.png: lawyer-level experience diagnostic
#     for D-hat_t
#   * table4_subgroup_recovery.{csv,tex}: subgroup recovery summary
#   * table5_adjacent_pair_recovery.{csv,tex}: per-pair recovery accuracy of
#     rubrics vs. CJ for human and LLM-as-a-Judge evaluators
#   * rubric_score_diagnostic.png: positive-only rubric score fractions
#     (diagnostic; not in the main paper)
# ============================================================

# ============================================================
# Controls
# ============================================================

script_path_from_args <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    # Rscript encodes spaces in --file= as ~+~; decode before normalizing.
    raw <- gsub("~+~", " ", sub("^--file=", "", file_arg[[1]]), fixed = TRUE)
    return(normalizePath(raw, mustWork = FALSE))
  }
  normalizePath(getwd(), mustWork = FALSE)
}

first_existing_path <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) > 0) existing[[1]] else paths[[1]]
}

script_dir <- dirname(script_path_from_args())
# Search for the released dataset directory in a few sensible locations.
# Override via EAH_JB_DIR if your dataset lives elsewhere.
default_judgmentbench_dir <- first_existing_path(c(
  file.path(getwd(), "result-dataset"),
  file.path(script_dir, "result-dataset"),
  file.path(dirname(script_dir), "result-dataset")
))

mini_autograder_subdir <- "gpt_5_4_mini"

# Inputs are always read from the released split-CSV dataset under
# judgmentbench_dir.
judgmentbench_dir <- Sys.getenv(
  "EAH_JB_DIR",
  unset = default_judgmentbench_dir
)

# human_single treats each lawyer/pipeline instance as the bootstrap cluster.
analysis_mode <- "human_single"
truth_order <- c("intermediate", "good", "excellent")
experience_bin_levels <- c("<=3", "4-7", "8-11", "12-15", "16-19", ">=20")
score_tie_tolerance <- 1e-5
seed <- 0L
n_boot <- as.integer(Sys.getenv("EAH_N_BOOT", "2000"))
install_missing_packages <- TRUE
save_plot <- tolower(Sys.getenv("EAH_SAVE_PLOT", unset = "true")) %in% c("true", "1", "yes", "y")

# Optional override for the bootstrap plot path; other outputs use output_dir.
plot_path <- Sys.getenv("EAH_BOOTSTRAP_PLOT_PATH", unset = "")
plot_path <- if (nzchar(plot_path)) plot_path else NULL
output_dir <- Sys.getenv(
  "EAH_ANALYSIS_OUTPUT_DIR",
  unset = script_dir
)

# ============================================================
# Packages
# ============================================================

required_packages <- c(
  "dplyr", "tidyr", "readr", "tibble", "ggplot2", "purrr", "ragg", "gtable", "jsonlite", "patchwork",
  "stringr"
)

if (install_missing_packages) {
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, repos = "https://cloud.r-project.org")
  }
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ============================================================
# Helpers
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path)
  }
}

section_header <- function(title) {
  cat("\n", paste(rep("=", 78), collapse = ""), "\n", sep = "")
  cat(title, "\n", sep = "")
  cat(paste(rep("=", 78), collapse = ""), "\n", sep = "")
}

sub_header <- function(title) {
  cat("\n", title, "\n", sep = "")
  cat(paste(rep("-", nchar(title)), collapse = ""), "\n", sep = "")
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

latex_escape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  sentinel <- "\001"

  x <- stringr::str_replace_all(x, stringr::fixed("\\"), sentinel)
  x <- stringr::str_replace_all(x, stringr::fixed("&"), "\\\\&")
  x <- stringr::str_replace_all(x, stringr::fixed("%"), "\\\\%")
  x <- stringr::str_replace_all(x, stringr::fixed("$"), "\\\\$")
  x <- stringr::str_replace_all(x, stringr::fixed("#"), "\\\\#")
  x <- stringr::str_replace_all(x, stringr::fixed("_"), "\\\\_")
  x <- stringr::str_replace_all(x, stringr::fixed("{"), "\\\\{")
  x <- stringr::str_replace_all(x, stringr::fixed("}"), "\\\\}")
  x <- stringr::str_replace_all(x, stringr::fixed("~"), "\\\\textasciitilde{}")
  x <- stringr::str_replace_all(x, stringr::fixed("^"), "\\\\textasciicircum{}")
  x <- stringr::str_replace_all(x, stringr::fixed("<"), "\\\\textless{}")
  x <- stringr::str_replace_all(x, stringr::fixed(">"), "\\\\textgreater{}")
  stringr::str_replace_all(x, stringr::fixed(sentinel), "\\\\textbackslash{}")
}

latex_table_cell <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  numeric_range <- grepl("^[0-9]+-[0-9]+$", x)
  dplyr::case_when(
    x == "<=3" ~ "$\\leq{}3$",
    x == ">=20" ~ "$\\geq{}20$",
    x == "< median" ~ "$<{}$ median",
    x == ">= median" ~ "$\\geq{}$ median",
    x == "Years of Experience" ~ "\\mbox{Years of Experience}",
    numeric_range ~ sub("-", "--", x, fixed = TRUE),
    TRUE ~ latex_escape(x)
  )
}

write_lines <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, con = path, useBytes = TRUE)
}

validate_analysis_mode <- function(x) {
  allowed_modes <- c("llm_both", "human_single")

  if (!(x %in% allowed_modes)) {
    stop(
      "analysis_mode must be one of: ",
      paste(allowed_modes, collapse = ", "),
      ". Received: ",
      x
    )
  }

  x
}

mode_cluster_label <- function(analysis_mode) {
  analysis_mode <- validate_analysis_mode(analysis_mode)

  if (analysis_mode == "human_single") {
    "Lawyers"
  } else {
    "Pipeline instances"
  }
}

mode_bootstrap_label <- function(analysis_mode) {
  analysis_mode <- validate_analysis_mode(analysis_mode)

  if (analysis_mode == "human_single") {
    "lawyer-first hierarchical bootstrap"
  } else {
    "pipeline-instance-first hierarchical bootstrap"
  }
}

# Round-1 exports may have blank pipeline_round values; map those to "round1"
# so the round component of the pipeline-instance ID is always non-empty.
normalize_pipeline_round <- function(x) {
  x <- as.character(x)
  x <- tolower(trimws(x))
  dplyr::if_else(is.na(x) | x == "", "round1", x)
}

# A lawyer / pipeline instance is identified by (pipeline_round, pipeline_number).
# pipeline_number alone is not unique across rounds, so the round must be
# included to keep clusters distinct in later rounds. This is the bootstrap
# resampling unit and the "lawyer" unit referenced throughout the script.
derive_pipeline_instance_id <- function(dat) {
  if (!("pipeline_number" %in% names(dat))) {
    stop("Input data must contain pipeline_number.")
  }

  pipeline_number <- as.character(dat$pipeline_number)

  if ("pipeline_round" %in% names(dat)) {
    pipeline_round <- normalize_pipeline_round(dat$pipeline_round)
  } else {
    pipeline_round <- rep("round1", length(pipeline_number))
  }

  if (any(is.na(pipeline_number) | trimws(pipeline_number) == "")) {
    stop("Input data contains missing pipeline_number values.")
  }

  paste(pipeline_round, pipeline_number, sep = "::")
}

normalize_method <- function(x) {
  x <- tolower(trimws(x))
  dplyr::case_when(
    x %in% c("preference", "preferences", "p") ~ "preference",
    x %in% c("rubric", "rubrics", "r") ~ "rubric",
    TRUE ~ x
  )
}

normalize_rung <- function(x) {
  tolower(trimws(x))
}

normalize_experience_bin <- function(x) {
  text <- as.character(x)
  text <- trimws(text)
  text[text == "" | is.na(text) | text == "Not reported"] <- NA_character_
  dplyr::case_when(
    text %in% c("<=3", "≤3", "0-3") ~ "<=3",
    text %in% c("4-7", "4 to 7") ~ "4-7",
    text %in% c("8-11", "8 to 11") ~ "8-11",
    text %in% c("12-15", "12 to 15") ~ "12-15",
    text %in% c("16-19", "16 to 19") ~ "16-19",
    text %in% c(">=20", "≥20", "20+") ~ ">=20",
    TRUE ~ text
  )
}

mode_or_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[[which.max(tabulate(match(x, ux)))]]
}

# Replace each near-tied group of values (within `tolerance`) with their mean,
# so floating-point jitter does not produce spurious orderings of equal scores.
# Returned vector has the same length and order as the input.
collapse_near_ties <- function(x, tolerance = score_tie_tolerance) {
  if (length(x) <= 1L || any(is.na(x))) {
    return(x)
  }

  ord <- order(x)
  out <- x
  group <- ord[[1]]
  anchor <- x[[ord[[1]]]]

  flush_group <- function(idx) {
    out[idx] <<- mean(x[idx])
  }

  for (i in ord[-1]) {
    if (abs(x[[i]] - anchor) <= tolerance) {
      group <- c(group, i)
    } else {
      flush_group(group)
      group <- i
      anchor <- x[[i]]
    }
  }

  flush_group(group)
  out
}

# Task-method recovery statistic: Spearman's rho between the gold rung order
# (truth_order) and the method-implied ordering implied by score_named_vector.
# Conventions (per the study-design analysis plan):
#   * Partial ties resolve via averaged ranks.
#   * A fully-tied implied ordering returns rho = 0 (rather than undefined).
#   * Missing implied scores for any rung return NA (caller decides whether
#     the task-method is estimable).
score_recovery_spearman <- function(score_named_vector, truth_order) {
  score_named_vector <- score_named_vector[truth_order]

  if (any(is.na(score_named_vector))) {
    return(NA_real_)
  }

  score_named_vector <- collapse_near_ties(score_named_vector)

  truth_ranks <- seq_along(truth_order)
  observed_ranks <- rank(score_named_vector, ties.method = "average")

  if (stats::sd(observed_ranks) == 0) {
    return(0)
  }

  as.numeric(stats::cor(observed_ranks, truth_ranks, method = "spearman"))
}

# Bradley-Terry latent-strength fit on quality-rung items (typically the three
# rungs in `truth_order`). Returns a tibble with one row per rung and the fitted
# latent score, or NA scores if the optimizer fails. The optional
# bt_sort_for_stability flag forces a deterministic comparison order so that
# bootstrap replicates with identical inputs produce identical fits.
fit_bt_scores <- function(task_comparisons, truth_order) {
  task_comparisons <- task_comparisons %>%
    dplyr::filter(winner %in% truth_order, loser %in% truth_order, winner != loser)

  if ("bt_sort_for_stability" %in% names(task_comparisons) &&
      any(task_comparisons$bt_sort_for_stability, na.rm = TRUE)) {
    task_comparisons <- task_comparisons %>%
      dplyr::arrange(winner, loser)
  }

  if (nrow(task_comparisons) == 0) {
    return(tibble::tibble(rung = truth_order, observed_score = NA_real_))
  }

  item_index <- stats::setNames(seq_along(truth_order), truth_order)
  winner_idx <- unname(item_index[task_comparisons$winner])
  loser_idx <- unname(item_index[task_comparisons$loser])
  k <- length(truth_order)

  # Unpenalized Bradley-Terry log-likelihood with the sum-to-zero
  # identification constraint (theta_k = -sum(theta_{1..k-1})).
  neg_loglik <- function(par) {
    theta <- c(par, -sum(par))
    eta <- theta[winner_idx] - theta[loser_idx]
    -sum(stats::plogis(eta, log.p = TRUE))
  }
  
  opt <- tryCatch(
    stats::optim(rep(0, k - 1), neg_loglik, method = "BFGS"),
    error = function(e) NULL
  )
  
  theta <- if (is.null(opt) || !is.finite(opt$value)) {
    rep(NA_real_, k)
  } else {
    collapse_near_ties(c(opt$par, -sum(opt$par)))
  }
  
  tibble::tibble(rung = truth_order, observed_score = theta)
}

# ============================================================
# Input preparation and validation
# ============================================================

derive_cluster_id <- function(dat, analysis_mode) {
  analysis_mode <- validate_analysis_mode(analysis_mode)
  pipeline_instance_id <- derive_pipeline_instance_id(dat)

  # For human annotations, the study design treats each lawyer as the assigned
  # pipeline instance: normalized pipeline_round + pipeline_number.
  pipeline_instance_id
}

validate_lawyer_identifier_integrity <- function(dat, analysis_mode) {
  analysis_mode <- validate_analysis_mode(analysis_mode)

  if (analysis_mode != "human_single" || !("user_email" %in% names(dat))) {
    return(invisible(TRUE))
  }

  id_audit <- dat %>%
    dplyr::distinct(pipeline_instance_id, user_email) %>%
    dplyr::filter(!is.na(pipeline_instance_id), !is.na(user_email), user_email != "")

  shared_pipeline_instances <- id_audit %>%
    dplyr::count(pipeline_instance_id, name = "n_users") %>%
    dplyr::filter(n_users > 1L)

  users_with_multiple_pipeline_instances <- id_audit %>%
    dplyr::count(user_email, name = "n_pipeline_instances") %>%
    dplyr::filter(n_pipeline_instances > 1L)

  if (nrow(shared_pipeline_instances) > 0 || nrow(users_with_multiple_pipeline_instances) > 0) {
    stop(
      "Lawyer identifier integrity check failed: expected one user per ",
      "pipeline_instance_id and one pipeline_instance_id per user_email."
    )
  }

  invisible(TRUE)
}

blank_to_na <- function(x) {
  x <- as.character(x)
  x[is.na(x) | trimws(x) == ""] <- NA_character_
  x
}

clean_task_type_label <- function(x) {
  as.character(x)
}

build_judgmentbench_task_meta <- function(tasks) {
  tasks %>%
    dplyr::transmute(
      task_id,
      task_number = suppressWarnings(as.integer(stringr::str_remove(task_id, "^task_"))),
      task_category,
      task_type = clean_task_type_label(task_type),
      task_name = task,
      task_max_points = suppressWarnings(as.numeric(max_points))
    )
}

build_judgmentbench_item_score_json <- function(rubric_item_scores) {
  if (nrow(rubric_item_scores) == 0) {
    return(tibble::tibble(annotation_id = character(), rubric_item_scores_json = character()))
  }

  rubric_item_scores %>%
    dplyr::arrange(annotation_id, score_order) %>%
    dplyr::mutate(
      awarded_points_num = suppressWarnings(as.numeric(awarded_points)),
      score_json = paste0("{\"awardedPoints\":", awarded_points_num, "}")
    ) %>%
    dplyr::group_by(annotation_id) %>%
    dplyr::summarise(
      rubric_item_scores_json = paste0("[", paste(score_json, collapse = ","), "]"),
      .groups = "drop"
    )
}

combine_comparative_comments <- function(comment_a, comment_b) {
  comment_a <- blank_to_na(comment_a)
  comment_b <- blank_to_na(comment_b)
  dplyr::case_when(
    !is.na(comment_a) & !is.na(comment_b) ~ paste0(
      "Comment on Version A:\n\n", comment_a,
      "\n\n\nComment on Version B:\n\n", comment_b
    ),
    !is.na(comment_a) ~ paste0("Comment on Version A:\n\n", comment_a),
    !is.na(comment_b) ~ paste0("Comment on Version B:\n\n", comment_b),
    TRUE ~ NA_character_
  )
}

build_judgmentbench_annotator_lookup <- function(annotators) {
  annotators %>%
    dplyr::transmute(
      annotator_id,
      # The public dataset intentionally exposes release-native annotator IDs,
      # not the private study pipeline numbers. For the analysis, the annotator
      # is the bootstrap cluster, so the release ID is the natural cluster key.
      pipeline_number = annotator_id,
      pipeline_round = "judgmentbench",
      user_email = annotator_id,
      user_name = NA_character_,
      user_title = as.character(title),
      user_firm = as.character(organization_type),
      user_practice_areas = as.character(practice_areas),
      user_practice_experience = NA_character_,
      user_total_yoe = dplyr::if_else(
        years_experience == "Not reported",
        NA_character_,
        normalize_experience_bin(years_experience)
      ),
      user_registered_at = NA_character_,
      pipeline_starting_method = NA_character_
    )
}

build_judgmentbench_assignment_lookup <- function(assignment_records) {
  if (nrow(assignment_records) == 0) {
    return(tibble::tibble(
      annotator_id = character(),
      task_id = character(),
      task_slot_order = character(),
      method_step_order = character(),
      method = character(),
      output_id = character(),
      option_a_output_id = character(),
      option_b_output_id = character(),
      assignment_position = integer(),
      assignment_time_spent_seconds = numeric()
    ))
  }

  assignment_records %>%
    dplyr::mutate(
      method = normalize_method(method),
      output_id = blank_to_na(output_id),
      option_a_output_id = blank_to_na(option_a_output_id),
      option_b_output_id = blank_to_na(option_b_output_id)
    ) %>%
    dplyr::filter(status == "completed") %>%
    dplyr::transmute(
      annotator_id,
      task_id,
      task_slot_order = as.character(task_slot_order),
      method_step_order = as.character(method_step_order),
      method,
      output_id,
      option_a_output_id,
      option_b_output_id,
      assignment_position = suppressWarnings(as.integer(assignment_order)),
      assignment_time_spent_seconds = suppressWarnings(as.numeric(time_spent_seconds))
    )
}

build_judgmentbench_noncompleted_rows <- function(assignment_records, annotator_lookup, task_meta) {
  if (nrow(assignment_records) == 0) {
    return(tibble::tibble())
  }

  assignment_records %>%
    dplyr::mutate(
      method = normalize_method(method),
      status = tolower(trimws(status))
    ) %>%
    dplyr::filter(status != "completed") %>%
    dplyr::left_join(task_meta, by = "task_id") %>%
    dplyr::left_join(annotator_lookup, by = "annotator_id") %>%
    dplyr::transmute(
      exported_at = NA_character_,
      environment = "judgmentbench",
      user_email, user_name, user_title, user_firm, user_practice_areas,
      user_practice_experience, user_total_yoe, user_registered_at,
      pipeline_number, pipeline_round, pipeline_starting_method,
      assignment_position = suppressWarnings(as.integer(assignment_order)),
      task_slot_number = suppressWarnings(as.integer(task_slot_order)),
      method_step_number = suppressWarnings(as.integer(method_step_order)),
      method,
      status,
      task_number,
      task_category,
      task_type,
      task_name,
      task_max_points,
      output_id = NA_character_,
      rung_alias = NA_character_,
      rung_label = NA_character_,
      version_number = NA_real_,
      ladder_id = NA_character_,
      model = NA_character_,
      source_run_id = NA_character_,
      option_a_output_id = NA_character_,
      option_a_rung_alias = NA_character_,
      option_a_rung_label = NA_character_,
      option_a_version_number = NA_real_,
      option_b_output_id = NA_character_,
      option_b_rung_alias = NA_character_,
      option_b_rung_label = NA_character_,
      option_b_version_number = NA_real_,
      preferred_output_id = NA_character_,
      preferred_option = NA_character_,
      preferred_rung_alias = NA_character_,
      preferred_version_number = NA_real_,
      time_spent_seconds = suppressWarnings(as.numeric(time_spent_seconds)),
      rubric_total_points = NA_real_,
      rubric_max_points = NA_real_,
      comment = NA_character_,
      rubric_item_scores_json = NA_character_,
      assignment_created_at = NA_character_,
      eval_created_at = NA_character_,
      source_step_key = assignment_record_id
    )
}

read_study_results_judgmentbench <- function(jb_dir,
                                             evaluator_type = c("human", "autograder"),
                                             autograder_subdir = "gpt_5_4",
                                             autograder_model_label = "gpt-5.4") {
  # Resolves CSV paths against the released directory layout:
  #   <jb_dir>/human/...                     human annotation CSVs
  #   <jb_dir>/autograders/<subdir>/...      one autograder model's CSVs
  #   <jb_dir>/base/{tasks,rubric_items}.csv shared task metadata
  #   <jb_dir>/outputs/outputs.csv           constructed-output catalogue
  evaluator_type <- match.arg(evaluator_type)

  annotations_dir <- if (evaluator_type == "human") {
    file.path(jb_dir, "human")
  } else {
    file.path(jb_dir, "autograders", autograder_subdir)
  }
  annotators_path <- file.path(jb_dir, "human", "annotators.csv")
  assignment_records_path <- file.path(jb_dir, "human", "assignment_records.csv")
  tasks_path <- file.path(jb_dir, "base", "tasks.csv")
  outputs_path <- file.path(jb_dir, "outputs", "outputs.csv")
  ann_rubric_path <- file.path(annotations_dir, "annotations_rubric.csv")
  ann_cj_path <- file.path(annotations_dir, "annotations_comparative_judgment.csv")
  item_scores_path <- file.path(annotations_dir, "rubric_item_scores.csv")

  for (f in c(annotators_path, tasks_path, outputs_path, assignment_records_path,
              ann_rubric_path, ann_cj_path, item_scores_path)) {
    stop_if_missing(f)
  }

  annotators <- readr::read_csv(annotators_path, show_col_types = FALSE)
  tasks <- readr::read_csv(tasks_path, show_col_types = FALSE)
  outputs <- readr::read_csv(outputs_path, show_col_types = FALSE)
  assignment_records <- readr::read_csv(assignment_records_path, show_col_types = FALSE)
  task_meta <- build_judgmentbench_task_meta(tasks)
  annotator_lookup <- build_judgmentbench_annotator_lookup(annotators)
  assignment_lookup <- build_judgmentbench_assignment_lookup(assignment_records)

  output_lookup <- outputs %>%
    dplyr::left_join(task_meta %>% dplyr::select(task_id, task_number), by = "task_id") %>%
    dplyr::transmute(
      output_id,
      output_release_id = output_id,
      quality_level,
      quality_level_order,
      version_number = suppressWarnings(as.numeric(version_number))
    )

  if (evaluator_type == "human") {
    ann_rubric <- readr::read_csv(ann_rubric_path, show_col_types = FALSE)
    ann_cj <- readr::read_csv(ann_cj_path, show_col_types = FALSE)
    item_scores_json <- readr::read_csv(item_scores_path, show_col_types = FALSE) %>%
      build_judgmentbench_item_score_json()
    noncompleted_rows <- build_judgmentbench_noncompleted_rows(assignment_records, annotator_lookup, task_meta)
  } else {
    ann_rubric <- readr::read_csv(ann_rubric_path, show_col_types = FALSE) %>%
      dplyr::rename(annotator_id = corresponding_annotator_id) %>%
      dplyr::mutate(time_spent_seconds = NA_real_)
    ann_cj <- readr::read_csv(ann_cj_path, show_col_types = FALSE) %>%
      dplyr::rename(annotator_id = corresponding_annotator_id) %>%
      dplyr::mutate(time_spent_seconds = NA_real_)
    item_scores_json <- readr::read_csv(item_scores_path, show_col_types = FALSE) %>%
      build_judgmentbench_item_score_json()
    noncompleted_rows <- tibble::tibble()
  }

  rubric_long <- ann_rubric %>%
    dplyr::left_join(item_scores_json, by = "annotation_id") %>%
    dplyr::left_join(
      output_lookup %>%
        dplyr::select(output_id, output_release_id, version_number),
      by = "output_id"
    ) %>%
    dplyr::mutate(
      task_slot_order = as.character(task_slot_order),
      method_step_order = as.character(method_step_order),
      output_id = blank_to_na(output_id)
    ) %>%
    dplyr::left_join(
      assignment_lookup %>%
        dplyr::filter(method == "rubric") %>%
        dplyr::select(
          annotator_id, task_id, task_slot_order, method_step_order, output_id,
          assignment_position, assignment_time_spent_seconds
        ),
      by = c("annotator_id", "task_id", "task_slot_order", "method_step_order", "output_id")
    ) %>%
    dplyr::left_join(task_meta, by = "task_id") %>%
    dplyr::left_join(annotator_lookup, by = "annotator_id") %>%
    dplyr::transmute(
      exported_at = NA_character_,
      environment = "judgmentbench",
      user_email, user_name, user_title, user_firm, user_practice_areas,
      user_practice_experience, user_total_yoe, user_registered_at,
      pipeline_number, pipeline_round, pipeline_starting_method,
      assignment_position = dplyr::coalesce(
        assignment_position,
        suppressWarnings(as.integer(annotation_order))
      ),
      task_slot_number = suppressWarnings(as.integer(task_slot_order)),
      method_step_number = suppressWarnings(as.integer(method_step_order)),
      method = "rubric",
      status = "completed",
      task_number,
      task_category,
      task_type,
      task_name,
      task_max_points,
      output_id = output_release_id,
      rung_alias = output_quality_level,
      rung_label = output_quality_level,
      version_number,
      ladder_id = NA_character_,
      model = if (evaluator_type == "autograder") autograder_model_label else NA_character_,
      source_run_id = NA_character_,
      option_a_output_id = NA_character_,
      option_a_rung_alias = NA_character_,
      option_a_rung_label = NA_character_,
      option_a_version_number = NA_real_,
      option_b_output_id = NA_character_,
      option_b_rung_alias = NA_character_,
      option_b_rung_label = NA_character_,
      option_b_version_number = NA_real_,
      preferred_output_id = NA_character_,
      preferred_option = NA_character_,
      preferred_rung_alias = NA_character_,
      preferred_version_number = NA_real_,
      time_spent_seconds = if (evaluator_type == "autograder") {
        NA_real_
      } else {
        suppressWarnings(as.numeric(dplyr::coalesce(time_spent_seconds, assignment_time_spent_seconds)))
      },
      rubric_total_points = suppressWarnings(as.numeric(rubric_total_points)),
      rubric_max_points = suppressWarnings(as.numeric(rubric_max_points)),
      comment = blank_to_na(comment),
      rubric_item_scores_json = dplyr::coalesce(rubric_item_scores_json, NA_character_),
      assignment_created_at = NA_character_,
      eval_created_at = NA_character_,
      source_step_key = annotation_id
    )

  cj_long <- ann_cj %>%
    dplyr::mutate(
      task_slot_order = as.character(task_slot_order),
      method_step_order = as.character(method_step_order),
      option_a_output_id = blank_to_na(option_a_output_id),
      option_b_output_id = blank_to_na(option_b_output_id),
      preferred_output_id = blank_to_na(preferred_output_id)
    ) %>%
    dplyr::left_join(
      output_lookup %>%
        dplyr::select(option_a_output_id = output_id, option_a_release_id = output_release_id, option_a_version_number = version_number),
      by = "option_a_output_id"
    ) %>%
    dplyr::left_join(
      output_lookup %>%
        dplyr::select(option_b_output_id = output_id, option_b_release_id = output_release_id, option_b_version_number = version_number),
      by = "option_b_output_id"
    ) %>%
    dplyr::left_join(
      output_lookup %>%
        dplyr::select(preferred_output_id = output_id, preferred_release_id = output_release_id, preferred_version_number = version_number),
      by = "preferred_output_id"
    ) %>%
    dplyr::left_join(
      assignment_lookup %>%
        dplyr::filter(method == "preference") %>%
        dplyr::select(
          annotator_id, task_id, task_slot_order, method_step_order,
          option_a_output_id, option_b_output_id,
          assignment_position, assignment_time_spent_seconds
        ),
      by = c(
        "annotator_id", "task_id", "task_slot_order", "method_step_order",
        "option_a_output_id", "option_b_output_id"
      )
    ) %>%
    dplyr::left_join(task_meta, by = "task_id") %>%
    dplyr::left_join(annotator_lookup, by = "annotator_id") %>%
    dplyr::mutate(
      preferred_quality = dplyr::if_else(
        preferred_option == "A",
        option_a_quality_level,
        option_b_quality_level
      )
    ) %>%
    dplyr::transmute(
      exported_at = NA_character_,
      environment = "judgmentbench",
      user_email, user_name, user_title, user_firm, user_practice_areas,
      user_practice_experience, user_total_yoe, user_registered_at,
      pipeline_number, pipeline_round, pipeline_starting_method,
      assignment_position = dplyr::coalesce(
        assignment_position,
        suppressWarnings(as.integer(annotation_order))
      ),
      task_slot_number = suppressWarnings(as.integer(task_slot_order)),
      method_step_number = suppressWarnings(as.integer(method_step_order)),
      method = "preference",
      status = "completed",
      task_number,
      task_category,
      task_type,
      task_name,
      task_max_points,
      output_id = NA_character_,
      rung_alias = NA_character_,
      rung_label = NA_character_,
      version_number = NA_real_,
      ladder_id = NA_character_,
      model = if (evaluator_type == "autograder") autograder_model_label else NA_character_,
      source_run_id = NA_character_,
      option_a_output_id = option_a_release_id,
      option_a_rung_alias = option_a_quality_level,
      option_a_rung_label = option_a_quality_level,
      option_a_version_number,
      option_b_output_id = option_b_release_id,
      option_b_rung_alias = option_b_quality_level,
      option_b_rung_label = option_b_quality_level,
      option_b_version_number,
      preferred_output_id = preferred_release_id,
      preferred_option,
      preferred_rung_alias = preferred_quality,
      preferred_version_number,
      time_spent_seconds = if (evaluator_type == "autograder") {
        NA_real_
      } else {
        suppressWarnings(as.numeric(dplyr::coalesce(time_spent_seconds, assignment_time_spent_seconds)))
      },
      rubric_total_points = NA_real_,
      rubric_max_points = NA_real_,
      comment = combine_comparative_comments(comment_a, comment_b),
      rubric_item_scores_json = NA_character_,
      assignment_created_at = NA_character_,
      eval_created_at = NA_character_,
      source_step_key = annotation_id
    )

  dplyr::bind_rows(rubric_long, cj_long) %>%
    dplyr::bind_rows(noncompleted_rows) %>%
    dplyr::arrange(
      pipeline_round,
      suppressWarnings(as.integer(pipeline_number)),
      pipeline_number,
      suppressWarnings(as.integer(assignment_position)),
      dplyr::if_else(method == "rubric", 1L, 2L),
      suppressWarnings(as.integer(task_slot_number)),
      suppressWarnings(as.integer(method_step_number)),
      task_number
    )
}

read_study_results <- function(analysis_mode,
                               judgmentbench_dir,
                               evaluator_type = "human",
                               autograder_subdir = "gpt_5_4",
                               autograder_model_label = "gpt-5.4") {
  analysis_mode <- validate_analysis_mode(analysis_mode)

  raw_dat <- read_study_results_judgmentbench(
    jb_dir = judgmentbench_dir,
    evaluator_type = evaluator_type,
    autograder_subdir = autograder_subdir,
    autograder_model_label = autograder_model_label
  )

  pipeline_instance_id <- derive_pipeline_instance_id(raw_dat)
  pipeline_round_normalized <- if ("pipeline_round" %in% names(raw_dat)) {
    normalize_pipeline_round(raw_dat$pipeline_round)
  } else {
    rep("round1", length(pipeline_instance_id))
  }
  cluster_id <- derive_cluster_id(raw_dat, analysis_mode)
  # Task slots can be reused after skips. Include task_number so a
  # replacement task cannot share a block key with the skipped task. Include
  # pipeline_instance_id because pipeline_number is reused across rounds.
  task_unit_key <- paste(
    pipeline_instance_id,
    raw_dat$task_slot_number,
    raw_dat$task_number,
    sep = "::"
  )

  out <- raw_dat %>%
    dplyr::mutate(
      method = normalize_method(method),
      status = tolower(trimws(status)),
      rung_label = normalize_rung(rung_label),
      option_a_rung_label = normalize_rung(option_a_rung_label),
      option_b_rung_label = normalize_rung(option_b_rung_label),
      preferred_rung_label = normalize_rung(preferred_rung_alias),
      pipeline_round_normalized = pipeline_round_normalized,
      pipeline_instance_id = pipeline_instance_id,
      lawyer_id = if (analysis_mode == "human_single") pipeline_instance_id else NA_character_,
      cluster_id = cluster_id,
      task_unit_key = task_unit_key,
      block_key = paste(task_unit_key, method, sep = "::")
    )

  validate_lawyer_identifier_integrity(out, analysis_mode)

  out
}

# Apply the complete-block inclusion rule. Each (lawyer, task, method) block
# is kept only if it has exactly three completed rows of the expected shape:
# three rung scores under rubric, or three cross-rung comparisons under CJ.
# Partial / fully-skipped / unfinished blocks are dropped from the analysis
# set. Returns a list with $analysis_dat (the kept rows) and $block_summary
# (per-block diagnostics, including why each excluded block was excluded).
validate_blocks <- function(dat, analysis_mode) {
  analysis_mode <- validate_analysis_mode(analysis_mode)

  block_summary <- dat %>%
    dplyr::group_by(
      block_key,
      cluster_id,
      pipeline_instance_id,
      pipeline_round_normalized,
      pipeline_number,
      task_slot_number,
      task_unit_key,
      task_number,
      task_name,
      method
    ) %>%
    dplyr::summarise(
      # If any row in a task-method block is completed but not all rows are
      # completed, treat the block as partial. This covers partial skips: the
      # annotator completed one or two expected rows and then skipped the rest.
      # Partial blocks are audited below but excluded from the analysis set.
      block_status = dplyr::case_when(
        all(status == "completed", na.rm = TRUE) ~ "completed",
        any(status == "completed", na.rm = TRUE) ~ "partial",
        TRUE ~ mode_or_first(status)
      ),
      n_rows = dplyr::n(),
      n_rubric_rungs = dplyr::n_distinct(rung_label[!is.na(rung_label)]),
      n_pref_pairs = dplyr::n_distinct(
        paste(option_a_rung_label, option_b_rung_label, sep = "__")[
          !is.na(option_a_rung_label) & !is.na(option_b_rung_label)
        ]
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # A block can have block_status == "completed" but still fail shape checks
      # if the export snapshot caught an in-progress task-method block. In that
      # case, all rows currently present are completed, but one or two expected
      # rows are not present yet because they are still not_started and filtered
      # out of the completed-and-skipped export.
      is_complete_expected_shape = dplyr::case_when(
        method == "rubric" ~ (block_status == "completed" & n_rows == 3L & n_rubric_rungs == 3L),
        method == "preference" ~ (block_status == "completed" & n_rows == 3L & n_pref_pairs == 3L),
        TRUE ~ FALSE
      )
    )

  block_audit <- block_summary %>%
    dplyr::count(method, block_status, is_complete_expected_shape) %>%
    dplyr::arrange(method, block_status, dplyr::desc(is_complete_expected_shape))

  complete_blocks <- block_summary %>%
    dplyr::filter(is_complete_expected_shape)

  complete_task_units <- complete_blocks %>%
    dplyr::distinct(
      cluster_id,
      pipeline_instance_id,
      pipeline_round_normalized,
      pipeline_number,
      task_slot_number,
      task_number,
      task_name,
      task_unit_key,
      method
    )

  paired_task_units <- complete_task_units %>%
    dplyr::transmute(
      cluster_id,
      pipeline_instance_id,
      pipeline_round_normalized,
      pipeline_number,
      task_slot_number,
      task_number,
      task_name,
      task_unit_key,
      method,
      method_present = TRUE
    ) %>%
    tidyr::pivot_wider(
      names_from = method,
      values_from = method_present,
      values_fill = FALSE
    ) %>%
    dplyr::mutate(
      has_complete_rubric = rubric,
      has_complete_preference = preference,
      is_dual_complete = has_complete_rubric & has_complete_preference
    )

  analysis_block_keys <- complete_blocks %>%
    dplyr::select(
      block_key,
      cluster_id,
      pipeline_instance_id,
      pipeline_round_normalized,
      pipeline_number,
      task_slot_number,
      task_number,
      task_name,
      task_unit_key,
      method
    )

  analysis_task_units <- complete_blocks %>%
    dplyr::distinct(task_unit_key)

  tasks_with_complete_blocks <- complete_blocks %>%
    dplyr::distinct(task_number, task_name, method)

  analysis_unit_label <- "Complete task blocks analyzed"

  analysis_dat <- dat %>%
    dplyr::semi_join(
      analysis_block_keys,
      by = c(
        "block_key",
        "cluster_id",
        "pipeline_instance_id",
        "pipeline_round_normalized",
        "pipeline_number",
        "task_slot_number",
        "task_number",
        "task_name",
        "task_unit_key",
        "method"
      )
    )

  if (any(analysis_dat$status != "completed", na.rm = TRUE)) {
    stop("Internal validation failed: non-completed rows reached analysis_dat.")
  }

  list(
    analysis_mode = analysis_mode,
    block_summary = block_summary,
    block_audit = block_audit,
    complete_blocks = complete_blocks,
    paired_task_units = paired_task_units,
    tasks_with_complete_blocks = tasks_with_complete_blocks,
    analysis_block_keys = analysis_block_keys,
    analysis_dat = analysis_dat,
    analysis_unit_count = nrow(dplyr::distinct(analysis_dat, block_key)),
    analysis_cluster_count = dplyr::n_distinct(analysis_dat$cluster_id),
    analysis_unit_label = analysis_unit_label
  )
}

# ============================================================
# Collapsed quality-level analysis
# ============================================================

empty_preference_task_summary <- function() {
  tibble::tibble(
    task_number = numeric(),
    task_name = character(),
    score_intermediate = numeric(),
    score_good = numeric(),
    score_excellent = numeric(),
    rho_preference = numeric()
  )
}

empty_rubric_task_summary <- function() {
  tibble::tibble(
    task_number = numeric(),
    task_name = character(),
    score_intermediate = numeric(),
    score_good = numeric(),
    score_excellent = numeric(),
    rho_rubric = numeric()
  )
}

build_preference_comparisons <- function(dat_complete, truth_order) {
  valid_pairs <- c(
    "intermediate__good", "good__excellent", "intermediate__excellent",
    "good__intermediate", "excellent__good", "excellent__intermediate"
  )
  
  dat_complete %>%
    dplyr::filter(method == "preference") %>%
    dplyr::transmute(
      block_key,
      task_number,
      task_name,
      winner = preferred_rung_label,
      loser = dplyr::case_when(
        preferred_rung_label == option_a_rung_label ~ option_b_rung_label,
        preferred_rung_label == option_b_rung_label ~ option_a_rung_label,
        TRUE ~ NA_character_
      ),
      pair_key = paste(option_a_rung_label, option_b_rung_label, sep = "__"),
      bt_sort_for_stability = if ("model" %in% names(dat_complete)) {
        !is.na(model) & model %in% c("gpt-5.4", "gpt-5.4-mini")
      } else {
        FALSE
      }
    ) %>%
    dplyr::filter(
      pair_key %in% valid_pairs,
      winner %in% truth_order,
      loser %in% truth_order,
      winner != loser
    )
}

# Per-task preference recovery: pool all annotators' cross-rung comparisons
# within each task, fit one Bradley-Terry model on the three rungs, then
# compute Spearman's rho between the gold rung order and the BT-implied
# ordering. Returns one row per task with the rung scores and rho_preference.
summarise_preference_tasks <- function(dat_complete, truth_order) {
  comps <- build_preference_comparisons(dat_complete, truth_order)

  if (nrow(comps) == 0) {
    return(empty_preference_task_summary())
  }

  task_scores <- comps %>%
    dplyr::group_by(task_number, task_name) %>%
    dplyr::group_modify(~ fit_bt_scores(.x, truth_order)) %>%
    dplyr::ungroup()
  
  task_scores %>%
    dplyr::group_by(task_number, task_name) %>%
    tidyr::complete(rung = truth_order) %>%
    dplyr::summarise(
      score_intermediate = observed_score[rung == "intermediate"],
      score_good = observed_score[rung == "good"],
      score_excellent = observed_score[rung == "excellent"],
      rho_preference = score_recovery_spearman(
        stats::setNames(
          c(score_intermediate, score_good, score_excellent),
          c("intermediate", "good", "excellent")
        ),
        truth_order = truth_order
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(task_number)
}

# Per-task rubric recovery: average each annotator's rubric_total_points
# within (task, rung), then compute Spearman's rho between the gold rung order
# and the order implied by the rung-level mean rubric scores. One row per task.
summarise_rubric_tasks <- function(dat_complete, truth_order) {
  rubric_dat <- dat_complete %>%
    dplyr::filter(method == "rubric", rung_label %in% truth_order, !is.na(rubric_total_points))

  if (nrow(rubric_dat) == 0) {
    return(empty_rubric_task_summary())
  }

  rubric_dat %>%
    dplyr::group_by(task_number, task_name, rung_label) %>%
    dplyr::summarise(observed_score = mean(rubric_total_points, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(task_number, task_name) %>%
    tidyr::complete(rung_label = truth_order) %>%
    dplyr::summarise(
      score_intermediate = observed_score[rung_label == "intermediate"],
      score_good = observed_score[rung_label == "good"],
      score_excellent = observed_score[rung_label == "excellent"],
      rho_rubric = score_recovery_spearman(
        stats::setNames(
          c(score_intermediate, score_good, score_excellent),
          c("intermediate", "good", "excellent")
        ),
        truth_order = truth_order
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(task_number)
}

build_method_summary <- function(pref_task_summary, rubric_task_summary) {
  rubric_estimable <- rubric_task_summary %>%
    dplyr::filter(is.finite(rho_rubric))

  pref_estimable <- pref_task_summary %>%
    dplyr::filter(is.finite(rho_preference))

  tibble::tibble(
    method = c("rubric", "preference"),
    n_estimable_tasks = c(
      nrow(rubric_estimable),
      nrow(pref_estimable)
    ),
    mean_task_rho = c(
      if (nrow(rubric_estimable) == 0) NA_real_ else mean(rubric_estimable$rho_rubric),
      if (nrow(pref_estimable) == 0) NA_real_ else mean(pref_estimable$rho_preference)
    ),
    median_task_rho = c(
      if (nrow(rubric_estimable) == 0) NA_real_ else stats::median(rubric_estimable$rho_rubric),
      if (nrow(pref_estimable) == 0) NA_real_ else stats::median(pref_estimable$rho_preference)
    ),
    exact_recovery_rate = c(
      if (nrow(rubric_estimable) == 0) NA_real_ else mean(rubric_estimable$rho_rubric == 1),
      if (nrow(pref_estimable) == 0) NA_real_ else mean(pref_estimable$rho_preference == 1)
    )
  )
}

summarise_method_agreement <- function(rubric_task_summary, pref_task_summary, truth_order) {
  if (nrow(rubric_task_summary) == 0 || nrow(pref_task_summary) == 0) {
    return(tibble::tibble(
      task_number = numeric(),
      task_name = character(),
      rho_method_agreement = numeric()
    ))
  }

  rubric_scores <- rubric_task_summary %>%
    dplyr::transmute(
      task_number,
      task_name,
      rubric_intermediate = score_intermediate,
      rubric_good = score_good,
      rubric_excellent = score_excellent
    )

  pref_scores <- pref_task_summary %>%
    dplyr::transmute(
      task_number,
      task_name,
      pref_intermediate = score_intermediate,
      pref_good = score_good,
      pref_excellent = score_excellent
    )

  joined <- dplyr::inner_join(rubric_scores, pref_scores, by = c("task_number", "task_name"))

  if (nrow(joined) == 0) {
    return(tibble::tibble(
      task_number = numeric(),
      task_name = character(),
      rho_method_agreement = numeric()
    ))
  }

  joined %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      rho_method_agreement = {
        rubric_vec <- stats::setNames(
          c(rubric_intermediate, rubric_good, rubric_excellent),
          c("intermediate", "good", "excellent")
        )[truth_order]
        pref_vec <- stats::setNames(
          c(pref_intermediate, pref_good, pref_excellent),
          c("intermediate", "good", "excellent")
        )[truth_order]

        if (any(is.na(rubric_vec)) || any(is.na(pref_vec))) {
          NA_real_
        } else {
          rubric_ranks <- rank(rubric_vec, ties.method = "average")
          pref_ranks <- rank(pref_vec, ties.method = "average")
          if (stats::sd(rubric_ranks) == 0 || stats::sd(pref_ranks) == 0) {
            0
          } else {
            as.numeric(stats::cor(rubric_ranks, pref_ranks, method = "spearman"))
          }
        }
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(task_number, task_name, rho_method_agreement) %>%
    dplyr::arrange(task_number)
}

# Study-level estimand:
#   D_hat = mean(rho_preference over estimable preference tasks)
#         - mean(rho_rubric    over estimable rubric    tasks)
# The two method averages may use different task sets if a task is estimable
# under one method but not the other.
compute_study_estimate <- function(pref_task_summary, rubric_task_summary) {
  rubric_estimable <- rubric_task_summary %>%
    dplyr::filter(is.finite(rho_rubric)) %>%
    dplyr::arrange(task_number)

  pref_estimable <- pref_task_summary %>%
    dplyr::filter(is.finite(rho_preference)) %>%
    dplyr::arrange(task_number)

  mean_rho_rubric <- if (nrow(rubric_estimable) == 0) NA_real_ else mean(rubric_estimable$rho_rubric)
  mean_rho_preference <- if (nrow(pref_estimable) == 0) NA_real_ else mean(pref_estimable$rho_preference)
  # Positive D_hat favors Comparative Judgments.
  D_hat <- if (is.finite(mean_rho_rubric) && is.finite(mean_rho_preference)) {
    mean_rho_preference - mean_rho_rubric
  } else {
    NA_real_
  }

  tibble::tibble(
    n_tasks_rubric = nrow(rubric_estimable),
    n_tasks_preference = nrow(pref_estimable),
    mean_rho_rubric = mean_rho_rubric,
    mean_rho_preference = mean_rho_preference,
    D_hat = D_hat
  )
}

# ============================================================
# Cluster-first hierarchical bootstrap
# ============================================================
#
# Inference layer for D_hat. For each replicate:
#   1. Resample clusters (lawyers / pipeline instances) with replacement.
#   2. Within each sampled cluster, resample completed task blocks with
#      replacement, assigning fresh ids so duplicated draws stay distinct.
#   3. Recompute task-level method summaries on the resampled data.
#   4. Re-derive D_hat* via compute_study_estimate().
# The bootstrap SE is the SD of D_hat* across finite replicates; the reported
# 95% CI is observed D_hat ± 1.96 * SE (not a percentile interval).

bootstrap_cluster_hierarchical <- function(
  dat_complete,
  truth_order,
  n_boot,
  seed,
  observed_D_hat = NULL
) {
  set.seed(seed)

  # Sample at the evaluator/pipeline level first, then resample task units
  # inside each sampled cluster.
  sampled_units_frame <- dat_complete %>%
    dplyr::distinct(
      cluster_id,
      pipeline_instance_id,
      pipeline_round_normalized,
      pipeline_number,
      task_slot_number,
      task_number,
      task_name,
      task_unit_key
    )
  
  clusters <- sort(unique(sampled_units_frame$cluster_id))
  
  if (length(clusters) == 0) {
    return(list(
      boot_tbl = tibble::tibble(
        .replicate = integer(),
        mean_rho_rubric = numeric(),
        mean_rho_preference = numeric(),
        D_hat = numeric(),
        n_tasks_rubric = integer(),
        n_tasks_preference = integer()
      ),
      summary = tibble::tibble(
        n_boot = 0L,
        n_finite_boot = 0L,
        observed_D_hat = observed_D_hat %||% NA_real_,
        bootstrap_se_D_hat = NA_real_,
        centered_ci_lower = NA_real_,
        centered_ci_upper = NA_real_,
        bootstrap_mean_D_hat = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        prop_D_hat_gt_0 = NA_real_,
        prop_D_hat_lt_0 = NA_real_,
        prop_D_hat_eq_0 = NA_real_
      )
    ))
  }
  
  split_units <- split(sampled_units_frame, sampled_units_frame$cluster_id)
  boot_results <- vector("list", n_boot)
  
  progress_bar <- utils::txtProgressBar(min = 0, max = n_boot, style = 3, width = 40)
  on.exit(close(progress_bar), add = TRUE)
  
  for (b in seq_len(n_boot)) {
    sampled_clusters <- sample(clusters, size = length(clusters), replace = TRUE)
    sampled_units_list <- vector("list", length(sampled_clusters))
    
    for (i in seq_along(sampled_clusters)) {
      sampled_cluster <- sampled_clusters[[i]]
      cluster_units <- split_units[[as.character(sampled_cluster)]]
      
      if (is.null(cluster_units) || nrow(cluster_units) == 0) {
        sampled_units_list[[i]] <- tibble::tibble()
      } else {
        drawn_idx <- sample.int(nrow(cluster_units), size = nrow(cluster_units), replace = TRUE)
        drawn_units <- cluster_units[drawn_idx, , drop = FALSE]
        # New ids keep duplicated bootstrap draws from collapsing back together.
        drawn_units$sampled_cluster_id <- paste0(sampled_cluster, "__draw", i)
        drawn_units$sampled_task_unit_key <- paste0(
          drawn_units$task_unit_key,
          "__draw",
          seq_len(nrow(drawn_units)),
          "__cluster",
          i
        )
        sampled_units_list[[i]] <- drawn_units
      }
    }
    
    sampled_units <- dplyr::bind_rows(sampled_units_list)
    
    boot_dat <- sampled_units %>%
      dplyr::left_join(
        dat_complete,
        by = c(
          "cluster_id",
          "pipeline_instance_id",
          "pipeline_round_normalized",
          "pipeline_number",
          "task_unit_key",
          "task_slot_number",
          "task_number",
          "task_name"
        ),
        relationship = "many-to-many"
      ) %>%
      dplyr::mutate(
        cluster_id = sampled_cluster_id,
        task_unit_key = sampled_task_unit_key,
        block_key = paste(task_unit_key, method, sep = "::")
      )
    
    pref_task_summary <- summarise_preference_tasks(boot_dat, truth_order)
    rubric_task_summary <- summarise_rubric_tasks(boot_dat, truth_order)
    study_estimate <- compute_study_estimate(pref_task_summary, rubric_task_summary)
    
    boot_results[[b]] <- tibble::tibble(
      .replicate = b,
      mean_rho_rubric = study_estimate$mean_rho_rubric[[1]],
      mean_rho_preference = study_estimate$mean_rho_preference[[1]],
      D_hat = study_estimate$D_hat[[1]],
      n_tasks_rubric = study_estimate$n_tasks_rubric[[1]],
      n_tasks_preference = study_estimate$n_tasks_preference[[1]]
    )
    
    utils::setTxtProgressBar(progress_bar, b)
  }
  
  cat("\n")
  
  boot_tbl <- dplyr::bind_rows(boot_results)

  finite_D_hat <- boot_tbl$D_hat[is.finite(boot_tbl$D_hat)]
  observed_D_hat <- observed_D_hat %||% NA_real_
  bootstrap_se_D_hat <- if (length(finite_D_hat) > 1L) {
    stats::sd(finite_D_hat)
  } else {
    NA_real_
  }
  normal_critical_value <- stats::qnorm(0.975)
  centered_ci_lower <- if (is.finite(observed_D_hat) && is.finite(bootstrap_se_D_hat)) {
    observed_D_hat - normal_critical_value * bootstrap_se_D_hat
  } else {
    NA_real_
  }
  centered_ci_upper <- if (is.finite(observed_D_hat) && is.finite(bootstrap_se_D_hat)) {
    observed_D_hat + normal_critical_value * bootstrap_se_D_hat
  } else {
    NA_real_
  }

  if (length(finite_D_hat) == 0) {
    summary_tbl <- tibble::tibble(
      n_boot = nrow(boot_tbl),
      n_finite_boot = 0L,
      observed_D_hat = observed_D_hat,
      bootstrap_se_D_hat = NA_real_,
      centered_ci_lower = NA_real_,
      centered_ci_upper = NA_real_,
      bootstrap_mean_D_hat = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      prop_D_hat_gt_0 = NA_real_,
      prop_D_hat_lt_0 = NA_real_,
      prop_D_hat_eq_0 = NA_real_
    )
  } else {
    summary_tbl <- tibble::tibble(
      n_boot = nrow(boot_tbl),
      n_finite_boot = length(finite_D_hat),
      observed_D_hat = observed_D_hat,
      bootstrap_se_D_hat = bootstrap_se_D_hat,
      centered_ci_lower = centered_ci_lower,
      centered_ci_upper = centered_ci_upper,
      bootstrap_mean_D_hat = mean(finite_D_hat),
      ci_lower = as.numeric(stats::quantile(finite_D_hat, probs = 0.025)),
      ci_upper = as.numeric(stats::quantile(finite_D_hat, probs = 0.975)),
      prop_D_hat_gt_0 = mean(finite_D_hat > 0),
      prop_D_hat_lt_0 = mean(finite_D_hat < 0),
      prop_D_hat_eq_0 = mean(finite_D_hat == 0)
    )
  }
  
  list(boot_tbl = boot_tbl, summary = summary_tbl)
}
# ============================================================
# Plotting
# ============================================================

method_display <- function(method) {
  dplyr::case_when(
    method == "rubric" ~ "Rubrics",
    method == "preference" ~ "Comparative Judgments",
    TRUE ~ method
  )
}

method_levels <- c("Comparative Judgments", "Rubrics")
time_metric_label <- "Time Spent Per Evaluation (≤ 30 min only)"
metric_levels <- c("Spearman\u2019s \u03c1", time_metric_label)

plot_theme_publication <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 5),
      plot.subtitle = ggplot2::element_text(size = base_size - 1, color = "grey25"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(color = "grey20"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey88", linewidth = 0.35),
      strip.background = ggplot2::element_rect(fill = "grey94", color = "grey70", linewidth = 0.4),
      strip.text = ggplot2::element_text(face = "bold", color = "grey15"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

publication_png_device <- function(res) {
  force(res)

  function(filename, width, height, bg = "white", ...) {
    ragg::agg_png(
      filename = filename,
      width = width,
      height = height,
      units = "in",
      res = res,
      background = bg,
      ...
    )
  }
}

# Assemble the data backing Figure 1: per-task rho values for both methods,
# plus per-evaluation timing rows for the time histogram. The timing side
# excludes outliers above 30 minutes (these are also the rows reported as
# excluded in the paper's outlier count).
build_main_plot_data <- function(pref_task_summary, rubric_task_summary, dat_complete) {
  rho_plot_data <- dplyr::bind_rows(
    rubric_task_summary %>%
      dplyr::filter(is.finite(rho_rubric)) %>%
      dplyr::transmute(
        method = factor("Rubrics", levels = method_levels),
        metric = factor("Spearman\u2019s \u03c1", levels = metric_levels),
        value = rho_rubric
      ),
    pref_task_summary %>%
      dplyr::filter(is.finite(rho_preference)) %>%
      dplyr::transmute(
        method = factor("Comparative Judgments", levels = method_levels),
        metric = factor("Spearman\u2019s \u03c1", levels = metric_levels),
        value = rho_preference
      )
  )

  # Keep the time-panel publication view aligned with the under-30-minute
  # convention. The raw table still keeps the full time data for analysis.
  time_plot_data <- dat_complete %>%
    dplyr::filter(is.finite(time_spent_seconds), time_spent_seconds >= 0) %>%
    dplyr::mutate(time_spent_minutes = time_spent_seconds / 60) %>%
    dplyr::filter(time_spent_minutes <= 30) %>%
    dplyr::transmute(
      method = factor(method_display(method), levels = method_levels),
      metric = factor(time_metric_label, levels = metric_levels),
      value = time_spent_minutes
    )

  dplyr::bind_rows(rho_plot_data, time_plot_data)
}

plot_main_result_panel <- function(main_plot_data) {
  fill_values <- c(
    "Comparative Judgments" = "#1B9E77",
    "Rubrics" = "#D95F02"
  )

  central_df <- main_plot_data %>%
    dplyr::group_by(method, metric) %>%
    dplyr::summarise(
      # Recovery panels use means; time panels use medians to match the labels.
      center_value = dplyr::if_else(
        metric[[1]] == "Spearman\u2019s \u03c1",
        mean(value, na.rm = TRUE),
        stats::median(value, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      center_label = dplyr::if_else(
        metric == "Spearman\u2019s \u03c1",
        sprintf("Mean = %.3f", center_value),
        sprintf("Median = %.1f min", center_value)
      ),
      hjust_value = dplyr::case_when(
        metric == "Spearman\u2019s \u03c1" & center_value > 0.75 ~ 1.05,
        TRUE ~ -0.05
      ),
      line_y_start = 0,
      # Inf lets coord_cartesian clip the dashed reference lines to each panel.
      line_y_end = Inf
    )

  x_breaks_without_time_edge <- function(limits) {
    if (is.finite(max(limits)) && max(limits) > 10) {
      breaks <- seq(0, ceiling(max(limits) / 10) * 10, by = 10)
      breaks <- breaks[breaks >= limits[[1]] & breaks <= limits[[2]]]
      return(breaks)
    }

    breaks <- pretty(limits, n = 5)
    breaks <- breaks[breaks >= limits[[1]] & breaks <= limits[[2]]]
    breaks
  }

  ggplot2::ggplot(main_plot_data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(
      data = dplyr::filter(main_plot_data, metric == "Spearman\u2019s \u03c1"),
      binwidth = 0.2,
      boundary = -1.1,
      ggplot2::aes(y = ggplot2::after_stat(count / sum(count)), fill = method),
      color = "white",
      linewidth = 0.45,
      alpha = 0.95
    ) +
    ggplot2::geom_histogram(
      data = dplyr::filter(main_plot_data, metric == time_metric_label),
      binwidth = 2,
      boundary = 0,
      ggplot2::aes(y = ggplot2::after_stat(count / sum(count)), fill = method),
      color = "white",
      linewidth = 0.45,
      alpha = 0.95
    ) +
    ggplot2::geom_segment(
      data = central_df,
      ggplot2::aes(
        x = center_value,
        xend = center_value,
        y = line_y_start,
        yend = line_y_end,
        color = method
      ),
      linetype = "dashed",
      linewidth = 1,
      lineend = "butt"
    ) +
    ggplot2::geom_text(
      data = central_df,
      ggplot2::aes(
        x = center_value,
        y = Inf,
        label = center_label,
        color = method,
        hjust = hjust_value
      ),
      inherit.aes = FALSE,
      vjust = 2.25,
      size = 3.7,
      fontface = "bold"
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(method),
      cols = ggplot2::vars(metric),
      # Only x is free; the shared y-axis keeps all four frequencies comparable.
      scales = "free_x",
      switch = "y"
    ) +
    ggplot2::scale_fill_manual(values = fill_values, guide = "none") +
    ggplot2::scale_color_manual(values = fill_values, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = x_breaks_without_time_edge,
      expand = ggplot2::expansion(mult = c(0.08, 0.12))
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%"),
      expand = ggplot2::expansion(mult = c(0.05, 0.44))
    ) +
    ggplot2::coord_cartesian(clip = "on") +
    ggplot2::labs(
      title = "Evaluation Method Recovery and Time",
      x = NULL,
      y = "Relative Frequency"
    ) +
    plot_theme_publication(base_size = 14) +
    ggplot2::theme(
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(angle = 90, size = 13),
      strip.text.x = ggplot2::element_text(
        size = 13,
        margin = ggplot2::margin(t = 5, r = 3, b = 5, l = 3)
      ),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 7)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 14)),
      panel.spacing = ggplot2::unit(2.0, "lines"),
      strip.switch.pad.grid = ggplot2::unit(0.8, "lines"),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 20,
        hjust = 0.5,
        margin = ggplot2::margin(b = 12)
      ),
      plot.subtitle = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 28, r = 30, b = 22, l = 46)
    )
}

add_top_strip_panel_padding <- function(plot, padding = grid::unit(0.20, "in")) {
  plot_grob <- ggplot2::ggplotGrob(plot)
  top_strip_rows <- plot_grob$layout$b[grepl("^strip-t", plot_grob$layout$name)]
  
  if (length(top_strip_rows) == 0) {
    return(plot_grob)
  }
  
  gtable::gtable_add_rows(
    x = plot_grob,
    heights = padding,
    pos = max(top_strip_rows)
  )
}

compute_one_block_rho <- function(block_dat, truth_order) {
  block_method <- block_dat$method[[1]]

  if (block_method == "rubric") {
    scores <- block_dat %>%
      dplyr::filter(rung_label %in% truth_order) %>%
      dplyr::group_by(rung_label) %>%
      dplyr::summarise(observed_score = mean(rubric_total_points, na.rm = TRUE), .groups = "drop")

    return(score_recovery_spearman(
      stats::setNames(scores$observed_score, scores$rung_label),
      truth_order = truth_order
    ))
  }

  if (block_method == "preference") {
    comps <- build_preference_comparisons(block_dat, truth_order)
    bt_scores <- fit_bt_scores(comps, truth_order)

    return(score_recovery_spearman(
      stats::setNames(bt_scores$observed_score, bt_scores$rung),
      truth_order = truth_order
    ))
  }

  NA_real_
}

build_block_rho_summary <- function(dat_complete, truth_order) {
  block_list <- dat_complete %>%
    dplyr::group_by(block_key) %>%
    dplyr::group_split(.keep = TRUE)

  purrr::map_dfr(block_list, function(block_dat) {
    meta <- block_dat[1, , drop = FALSE]

    tibble::tibble(
      block_key = meta$block_key,
      cluster_id = meta$cluster_id,
      pipeline_instance_id = meta$pipeline_instance_id,
      user_email = meta$user_email,
      years_experience_bin = as.character(bucket_years_experience(meta$user_total_yoe)),
      task_number = meta$task_number,
      task_name = meta$task_name,
      task_category = meta$task_category,
      method = meta$method,
      block_rho = compute_one_block_rho(block_dat, truth_order),
      block_time_seconds = sum(block_dat$time_spent_seconds, na.rm = TRUE)
    )
  })
}

build_lawyer_dt_data <- function(block_rho_summary) {
  block_rho_summary %>%
    dplyr::filter(is.finite(block_rho), !is.na(years_experience_bin)) %>%
    dplyr::group_by(cluster_id, pipeline_instance_id, user_email) %>%
    dplyr::summarise(
      years_experience_bin = dplyr::first(years_experience_bin[!is.na(years_experience_bin)]),
      rubric_rho = if (any(method == "rubric")) mean(block_rho[method == "rubric"], na.rm = TRUE) else NA_real_,
      cj_rho = if (any(method == "preference")) mean(block_rho[method == "preference"], na.rm = TRUE) else NA_real_,
      n_rubric_blocks = sum(method == "rubric"),
      n_cj_blocks = sum(method == "preference"),
      .groups = "drop"
    ) %>%
    # D_T is lawyer-level CJs minus Rubrics; positive values favor CJs.
    dplyr::mutate(D_T = cj_rho - rubric_rho) %>%
    dplyr::filter(is.finite(D_T), !is.na(years_experience_bin)) %>%
    dplyr::mutate(
      years_experience_bin = factor(years_experience_bin, levels = experience_bin_levels)
    ) %>%
    dplyr::filter(!is.na(years_experience_bin))
}

# Figure 4 (experience diagnostic). One jittered point per annotator/pipeline
# instance with estimable D_t (CJ recovery minus rubric recovery); blue
# diamonds mark the per-bin means of D_t. No fitted line or confidence band
# is drawn -- the figure is a scatter, not a regression visualization.
plot_experience_dt <- function(lawyer_dt_data) {
  mean_data <- lawyer_dt_data %>%
    dplyr::group_by(years_experience_bin) %>%
    dplyr::summarise(mean_D_T = mean(D_T, na.rm = TRUE), .groups = "drop")

  base_plot <- ggplot2::ggplot(
    lawyer_dt_data,
    ggplot2::aes(x = years_experience_bin, y = D_T)
  ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey88", linewidth = 0.35) +
    ggplot2::geom_jitter(
      width = 0.12,
      height = 0,
      size = 3.2,
      alpha = 0.9,
      color = "#263238",
      fill = "#F4A261",
      shape = 21,
      stroke = 0.65
    ) +
    ggplot2::geom_point(
      data = mean_data,
      ggplot2::aes(x = years_experience_bin, y = mean_D_T),
      inherit.aes = FALSE,
      shape = 23,
      size = 4.4,
      stroke = 0.8,
      color = "#0D47A1",
      fill = "#FFFFFF"
    )

  base_plot +
    ggplot2::labs(
      title = "Method Difference by Experience Bin",
      x = "Experience Bin",
      y = expression(hat(D)[t])
    ) +
    ggplot2::scale_x_discrete(drop = TRUE) +
    plot_theme_publication(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 19,
        hjust = 0.5,
        margin = ggplot2::margin(b = 12)
      ),
      plot.subtitle = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 18, r = 18, b = 14, l = 18)
    )
}

bucket_years_experience <- function(x) {
  normalized <- normalize_experience_bin(x)
  known_bins <- normalized %in% experience_bin_levels
  numeric_years <- readr::parse_number(as.character(x))
  out <- dplyr::case_when(
    known_bins ~ normalized,
    is.na(numeric_years) ~ NA_character_,
    numeric_years <= 3 ~ "<=3",
    numeric_years <= 7 ~ "4-7",
    numeric_years <= 11 ~ "8-11",
    numeric_years <= 15 ~ "12-15",
    numeric_years <= 19 ~ "16-19",
    TRUE ~ ">=20"
  )
  factor(out, levels = experience_bin_levels)
}

summarise_subset_estimate <- function(
  dat_subset,
  truth_order,
  subset_type,
  subset_order,
  subgroup_variable,
  subgroup_level,
  subgroup_order
) {
  pref_summary <- summarise_preference_tasks(dat_subset, truth_order)
  rubric_summary <- summarise_rubric_tasks(dat_subset, truth_order)
  estimate <- compute_study_estimate(pref_summary, rubric_summary)

  tibble::tibble(
    subset_type = subset_type,
    subset_order = subset_order,
    subgroup_variable = subgroup_variable,
    subgroup_level = subgroup_level,
    subgroup_order = subgroup_order,
    count = dplyr::n_distinct(dat_subset$block_key),
    rubric_rho = estimate$mean_rho_rubric[[1]],
    cj_rho = estimate$mean_rho_preference[[1]],
    D_hat = estimate$D_hat[[1]]
  )
}

build_task_subset_tagged_data <- function(dat_complete) {
  base_dat <- dat_complete %>%
    dplyr::mutate(
      category_level = dplyr::if_else(task_category == "Transactional", "Transactions", task_category)
    )

  base_dat %>%
    dplyr::mutate(
      subset_type = "Tasks",
      subset_order = 2L,
      subgroup_variable = "Category",
      subgroup_level = category_level,
      subgroup_order = dplyr::case_when(subgroup_level == "Transactions" ~ 1L, TRUE ~ 2L)
    ) %>%
    dplyr::filter(!is.na(subgroup_level))
}

build_subgroup_tagged_data <- function(dat_complete) {
  task_subset_dat <- build_task_subset_tagged_data(dat_complete)

  block_time <- dat_complete %>%
    dplyr::group_by(block_key) %>%
    dplyr::summarise(block_time_seconds = sum(time_spent_seconds, na.rm = TRUE), .groups = "drop")

  # The time subgroup is descriptive: method and speed may be partially
  # confounded if comparative judgments tend to be faster than rubrics.
  median_block_time <- stats::median(block_time$block_time_seconds, na.rm = TRUE)

  base_dat <- dat_complete %>%
    dplyr::left_join(block_time, by = "block_key") %>%
    dplyr::mutate(
      yoe_bucket = as.character(bucket_years_experience(user_total_yoe)),
      time_bucket = dplyr::if_else(block_time_seconds < median_block_time, "< median", ">= median")
    )

  dplyr::bind_rows(
    task_subset_dat,
    base_dat %>%
      dplyr::mutate(
        subset_type = "Annotators",
        subset_order = 3L,
        subgroup_variable = "Years of Experience",
        subgroup_level = yoe_bucket,
        subgroup_order = dplyr::case_when(
          subgroup_level == "<=3" ~ 1L,
          subgroup_level == "4-7" ~ 2L,
          subgroup_level == "8-11" ~ 3L,
          subgroup_level == "12-15" ~ 4L,
          subgroup_level == "16-19" ~ 5L,
          subgroup_level == ">=20" ~ 6L,
          TRUE ~ 7L
        )
      ),
    base_dat %>%
      dplyr::mutate(
        subset_type = "Annotators",
        subset_order = 3L,
        subgroup_variable = "Time",
        subgroup_level = time_bucket,
        subgroup_order = dplyr::case_when(subgroup_level == "< median" ~ 8L, TRUE ~ 9L)
      )
  ) %>%
    dplyr::filter(!is.na(subgroup_level))
}

empty_autograder_subset_results <- function() {
  tibble::tibble(
    subset_type = character(),
    subgroup_variable = character(),
    subgroup_level = character(),
    auto_count = integer(),
    auto_rubric_rho = numeric(),
    auto_cj_rho = numeric(),
    auto_D_hat = numeric()
  )
}

build_autograder_subset_results <- function(autograder_dat_complete, truth_order) {
  if (is.null(autograder_dat_complete) || nrow(autograder_dat_complete) == 0) {
    return(empty_autograder_subset_results())
  }

  task_subset_dat <- build_task_subset_tagged_data(autograder_dat_complete)

  dplyr::bind_rows(
    summarise_subset_estimate(
      dat_subset = autograder_dat_complete,
      truth_order = truth_order,
      subset_type = "Full Dataset",
      subset_order = 1L,
      subgroup_variable = "Overall",
      subgroup_level = "All",
      subgroup_order = 1L
    ),
    task_subset_dat %>%
      dplyr::group_by(subset_type, subset_order, subgroup_variable, subgroup_level, subgroup_order) %>%
      dplyr::group_split(.keep = TRUE) %>%
      purrr::map_dfr(function(group_dat) {
        summarise_subset_estimate(
          dat_subset = group_dat,
          truth_order = truth_order,
          subset_type = group_dat$subset_type[[1]],
          subset_order = group_dat$subset_order[[1]],
          subgroup_variable = group_dat$subgroup_variable[[1]],
          subgroup_level = group_dat$subgroup_level[[1]],
          subgroup_order = group_dat$subgroup_order[[1]]
        )
      })
  ) %>%
    dplyr::transmute(
      subset_type,
      subgroup_variable,
      subgroup_level,
      auto_count = count,
      auto_rubric_rho = rubric_rho,
      auto_cj_rho = cj_rho,
      auto_D_hat = D_hat
    )
}

build_subgroup_results_table <- function(
  dat_complete,
  truth_order,
  autograder_dat_complete = NULL,
  autograder_mini_dat_complete = NULL
) {
  subgroup_tagged <- build_subgroup_tagged_data(dat_complete)

  # For each subgroup level, recompute the same method-level estimand used in
  # the main analysis: D_hat = mean CJ rho - mean Rubrics rho.
  human_results <- dplyr::bind_rows(
    summarise_subset_estimate(
      dat_subset = dat_complete,
      truth_order = truth_order,
      subset_type = "Full Dataset",
      subset_order = 1L,
      subgroup_variable = "Overall",
      subgroup_level = "All",
      subgroup_order = 1L
    ),
    subgroup_tagged %>%
      dplyr::group_by(subset_type, subset_order, subgroup_variable, subgroup_level, subgroup_order) %>%
      dplyr::group_split(.keep = TRUE) %>%
      purrr::map_dfr(function(group_dat) {
        summarise_subset_estimate(
          dat_subset = group_dat,
          truth_order = truth_order,
          subset_type = group_dat$subset_type[[1]],
          subset_order = group_dat$subset_order[[1]],
          subgroup_variable = group_dat$subgroup_variable[[1]],
          subgroup_level = group_dat$subgroup_level[[1]],
          subgroup_order = group_dat$subgroup_order[[1]]
        )
      })
  )

  autograder_results <- build_autograder_subset_results(
    autograder_dat_complete = autograder_dat_complete,
    truth_order = truth_order
  )
  autograder_mini_results <- build_autograder_subset_results(
    autograder_dat_complete = autograder_mini_dat_complete,
    truth_order = truth_order
  ) %>%
    dplyr::transmute(
      subset_type,
      subgroup_variable,
      subgroup_level,
      auto_mini_D_hat = auto_D_hat
    )

  human_results %>%
    dplyr::left_join(
      autograder_results,
      by = c("subset_type", "subgroup_variable", "subgroup_level")
    ) %>%
    dplyr::left_join(
      autograder_mini_results,
      by = c("subset_type", "subgroup_variable", "subgroup_level")
    ) %>%
    dplyr::mutate(
      subset_type = factor(subset_type, levels = c("Full Dataset", "Tasks", "Annotators")),
      subgroup_variable = factor(subgroup_variable, levels = c("Overall", "Category", "Years of Experience", "Time"))
    ) %>%
    dplyr::arrange(subset_type, subgroup_variable, subgroup_order)
}

auto_mini_display_column <- "Auto \\widehat{D}_{5.4-mini}"

# Convert the long subgroup_results tibble (with humans and both autograders)
# into the human-readable display layout expected by latex_subgroup_results_table.
format_subgroup_results_table <- function(subgroup_results) {
  display_table <- subgroup_results %>%
    dplyr::transmute(
      `Subset Type` = as.character(subset_type),
      Variable = as.character(subgroup_variable),
      Level = subgroup_level,
      Count = count,
      `CJ ρ` = fmt_num(cj_rho, 3),
      `Rubrics ρ` = fmt_num(rubric_rho, 3),
      D_hat = fmt_num(D_hat, 3),
      `Auto Count` = ifelse(is.na(auto_count), "", as.character(auto_count)),
      `Auto CJ ρ` = ifelse(is.na(auto_cj_rho), "", fmt_num(auto_cj_rho, 3)),
      `Auto Rubrics ρ` = ifelse(is.na(auto_rubric_rho), "", fmt_num(auto_rubric_rho, 3)),
      Auto_D_hat = ifelse(is.na(auto_D_hat), "", fmt_num(auto_D_hat, 3)),
      Auto_D_hat_5_4_mini = ifelse(is.na(auto_mini_D_hat), "", fmt_num(auto_mini_D_hat, 3))
    )

  names(display_table)[names(display_table) == "Auto_D_hat_5_4_mini"] <- auto_mini_display_column
  display_table
}

# Emit Table 4 (subgroup recovery summary) as a list of LaTeX source lines.
# Column widths and the rightmost two-line header are tuned to fit the paper
# template; range-bin labels (e.g. "4-7", "8-11") render with en-dashes via
# latex_table_cell, and the leq/geq experience-bin labels become $\leq{}3$ /
# $\geq{}20$ math symbols.
latex_subgroup_results_table <- function(display_table) {
  columns <- c(
    "Subset Type", "Variable", "Level", "Count", "CJ ρ", "Rubrics ρ", "D_hat",
    "Auto Count", "Auto CJ ρ", "Auto Rubrics ρ", "Auto_D_hat", auto_mini_display_column
  )
  header <- c(
    "Subset", "Variable", "Level", "$n$", "\\mbox{$\\bar{R}^{\\mathrm{CJ}}$}", "\\mbox{$\\bar{R}^{\\mathrm{rub}}$}",
    "$\\widehat{D}$", "\\shortstack{GPT-5.4\\\\$n$}", "\\shortstack{GPT-5.4\\\\$\\bar{R}^{\\mathrm{CJ}}$}", "\\shortstack{GPT-5.4\\\\$\\bar{R}^{\\mathrm{rub}}$}",
    "\\shortstack{GPT-5.4\\\\$\\widehat{D}$}", "\\shortstack{GPT-5.4-mini\\\\$\\widehat{D}$}"
  )

  lines <- c(
    "% Requires \\usepackage{booktabs}",
    "% Requires \\usepackage{array}",
    "\\begin{table}[!t]",
    "  \\caption{Subgroup recovery summary. For each subgroup, $\\bar{R}^{\\mathrm{CJ}}$ and $\\bar{R}^{\\mathrm{rub}}$ report the mean task-level recovery statistic under comparative judgment and rubric scoring, respectively, where task-level recovery is Spearman's rank correlation between the constructed quality ordering and the method-implied ordering. $\\widehat{D}=\\bar{R}^{\\mathrm{CJ}}-\\bar{R}^{\\mathrm{rub}}$, so positive values favor comparative judgments. The $n$ columns report complete task-method blocks contributing to the subgroup before task-level aggregation. GPT-5.4 columns report the corresponding LLM autograder evaluations; the GPT-5.4-mini column reports the method difference for the same full/task subsets. Autograder columns are blank for annotator-defined subgroups.}",
    "  \\label{tab:subgroup-recovery-summary}",
    "  \\centering",
    "  \\scriptsize",
    "  \\setlength{\\tabcolsep}{1pt}",
    "  \\renewcommand{\\arraystretch}{1.12}",
    "  \\begin{tabular}{@{}",
    "    >{\\raggedright\\arraybackslash}p{0.101\\textwidth}",
    "    >{\\raggedright\\arraybackslash}p{0.156\\textwidth}",
    "    >{\\raggedright\\arraybackslash}p{0.084\\textwidth}",
    "    >{\\centering\\arraybackslash}p{0.037\\textwidth}",
    "    >{\\centering\\arraybackslash}p{0.057\\textwidth}",
    "    >{\\centering\\arraybackslash}p{0.057\\textwidth}",
    "    >{\\centering\\arraybackslash}p{0.057\\textwidth}",
    "    >{\\centering\\arraybackslash}p{0.057\\textwidth}",
    "    >{\\centering\\arraybackslash}p{0.086\\textwidth}",
    "    >{\\centering\\arraybackslash}p{0.086\\textwidth}",
    "    >{\\centering\\arraybackslash}p{0.070\\textwidth}",
    "    >{\\centering\\arraybackslash}p{0.105\\textwidth}",
    "  @{}}",
    "    \\toprule",
    paste0("    ", paste(header, collapse = " & "), " \\\\"),
    "    \\midrule"
  )

  for (i in seq_len(nrow(display_table))) {
    if (i > 1 && display_table$`Subset Type`[[i]] != display_table$`Subset Type`[[i - 1]]) {
      lines <- c(lines, "    \\addlinespace[0.35em]")
    } else if (i > 1 && display_table$Variable[[i]] != display_table$Variable[[i - 1]]) {
      lines <- c(lines, "    \\addlinespace[0.18em]")
    }

    values <- as.character(unlist(display_table[i, columns], use.names = FALSE))
    values[is.na(values)] <- ""
    lines <- c(lines, paste0("    ", paste(latex_table_cell(values), collapse = " & "), " \\\\"))
  }

  c(
    lines,
    "    \\bottomrule",
    "  \\end{tabular}",
    "\\end{table}"
  )
}

sum_positive_rubric_points <- function(scores_json) {
  if (is.na(scores_json) || !nzchar(trimws(scores_json))) {
    return(NA_real_)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(scores_json),
    error = function(error) NULL
  )

  if (!is.data.frame(parsed) || !("awardedPoints" %in% names(parsed))) {
    return(NA_real_)
  }

  awarded_points <- suppressWarnings(as.numeric(parsed$awardedPoints))
  if (all(is.na(awarded_points))) {
    return(NA_real_)
  }

  sum(pmax(awarded_points, 0), na.rm = TRUE)
}

build_positive_rubric_score_rows <- function(dat_complete, evaluator_type) {
  dat_complete %>%
    dplyr::filter(method == "rubric", status == "completed") %>%
    dplyr::mutate(
      rubric_max_points = suppressWarnings(as.numeric(rubric_max_points)),
      positive_points = purrr::map_dbl(rubric_item_scores_json, sum_positive_rubric_points),
      score_fraction = positive_points / rubric_max_points,
      evaluator_type = evaluator_type
    ) %>%
    dplyr::filter(
      is.finite(score_fraction),
      is.finite(rubric_max_points),
      rubric_max_points > 0
    ) %>%
    dplyr::select(
      evaluator_type,
      task_number,
      task_name,
      rung_label,
      rubric_max_points,
      positive_points,
      score_fraction
    )
}

build_positive_rubric_score_data <- function(human_dat_complete, autograder_dat_complete) {
  dplyr::bind_rows(
    build_positive_rubric_score_rows(autograder_dat_complete, "LLM-as-a-Judge"),
    build_positive_rubric_score_rows(human_dat_complete, "Human")
  ) %>%
    dplyr::mutate(
      evaluator_type = factor(evaluator_type, levels = c("Human", "LLM-as-a-Judge"))
    )
}

plot_rubric_score_fractions <- function(score_data) {
  if (nrow(score_data) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = "Distribution of Rubric Score Fractions",
          subtitle = "No completed rubric rows with parseable item-level scores were available."
        )
    )
  }

  make_panel <- function(panel_data, panel_title, fill_color) {
    ggplot2::ggplot(panel_data, ggplot2::aes(x = score_fraction)) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density)),
        breaks = seq(0, 1, by = 0.1),
        fill = fill_color,
        color = "black",
        linewidth = 0.35,
        closed = "right"
      ) +
      ggplot2::coord_cartesian(xlim = c(0, 1), clip = "off") +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 1, by = 0.1),
        expand = ggplot2::expansion(mult = c(0.04, 0.04))
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.08))
      ) +
      ggplot2::labs(
        title = panel_title,
        x = "Score Fraction of Max (positive rubric points only)",
        y = "Density"
      ) +
      ggplot2::theme_classic(base_size = 15) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          hjust = 0.5,
          size = 18,
          margin = ggplot2::margin(b = 10)
        ),
        axis.title = ggplot2::element_text(size = 14),
        axis.text = ggplot2::element_text(size = 12),
        plot.margin = ggplot2::margin(t = 8, r = 14, b = 18, l = 14)
      )
  }

  llm_panel <- make_panel(
    dplyr::filter(score_data, evaluator_type == "LLM-as-a-Judge"),
    "LLM-as-a-Judge",
    "#2C7FB8"
  )
  human_panel <- make_panel(
    dplyr::filter(score_data, evaluator_type == "Human"),
    "Human",
    "#008A00"
  )

  patchwork::wrap_plots(llm_panel, human_panel, ncol = 1) +
    patchwork::plot_annotation(
      title = "Distribution of Rubric Score Fractions",
      subtitle = "Fractions count affirmative/positive rubric points only; deduction items are ignored.",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          size = 18,
          hjust = 0.5,
          margin = ggplot2::margin(b = 6)
        ),
        plot.subtitle = ggplot2::element_text(
          size = 11,
          hjust = 0.5,
          color = "#444444",
          margin = ggplot2::margin(b = 10)
        )
      )
    )
}

build_quality_level_score_rows <- function(
  dat_complete,
  evaluator_type,
  truth_order,
  rubric_score_type = c("total", "positive")
) {
  rubric_score_type <- match.arg(rubric_score_type)
  empty_rows <- tibble::tibble(
    evaluator_type = character(),
    method = character(),
    task_number = integer(),
    task_name = character(),
    quality_level = character(),
    value = numeric()
  )

  # Plot rubric scores as shares of the rubric maximum so tasks with different
  # rubric scales are comparable in the score-distribution panels.
  rubric_raw <- dat_complete %>%
    dplyr::filter(method == "rubric", rung_label %in% truth_order, !is.na(rubric_total_points)) %>%
    dplyr::mutate(
      rubric_max_points = suppressWarnings(as.numeric(rubric_max_points))
    ) %>%
    dplyr::filter(is.finite(rubric_max_points), rubric_max_points > 0)

  rubric_rows <- if (nrow(rubric_raw) == 0) {
    empty_rows
  } else {
    if (rubric_score_type == "positive") {
      rubric_raw <- rubric_raw %>%
        dplyr::mutate(
          value = purrr::map_dbl(rubric_item_scores_json, sum_positive_rubric_points) / rubric_max_points
        ) %>%
        dplyr::filter(is.finite(value))
    } else {
      rubric_raw <- rubric_raw %>%
        dplyr::mutate(value = rubric_total_points / rubric_max_points)
    }

    if (nrow(rubric_raw) == 0) {
      empty_rows
    } else {
      rubric_raw %>%
        dplyr::group_by(task_number, task_name, rung_label) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::group_by(task_number, task_name) %>%
        tidyr::complete(rung_label = truth_order) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(
          evaluator_type = evaluator_type,
          method = "Rubrics",
          task_number,
          task_name,
          quality_level = rung_label,
          value
        )
    }
  }

  comps <- build_preference_comparisons(dat_complete, truth_order)
  preference_rows <- if (nrow(comps) == 0) {
    empty_rows
  } else {
    comps %>%
      dplyr::group_by(task_number, task_name) %>%
      dplyr::group_modify(~ fit_bt_scores(.x, truth_order)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(task_number, task_name) %>%
      tidyr::complete(rung = truth_order) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        evaluator_type = evaluator_type,
        method = "Comparative Judgments",
        task_number,
        task_name,
        quality_level = rung,
        value = observed_score
      )
  }

  dplyr::bind_rows(rubric_rows, preference_rows)
}

build_quality_level_score_data <- function(
  human_dat_complete,
  autograder_dat_complete,
  truth_order,
  rubric_score_type = c("total", "positive")
) {
  rubric_score_type <- match.arg(rubric_score_type)

  dplyr::bind_rows(
    build_quality_level_score_rows(human_dat_complete, "Human", truth_order, rubric_score_type),
    build_quality_level_score_rows(autograder_dat_complete, "LLM-as-a-Judge", truth_order, rubric_score_type)
  ) %>%
    dplyr::mutate(
      evaluator_type = factor(evaluator_type, levels = c("Human", "LLM-as-a-Judge")),
      method = factor(method, levels = c("Rubrics", "Comparative Judgments")),
      quality_level = factor(quality_level, levels = truth_order)
    )
}

# Per-method agreement between humans and a single autograder. For each
# (method, task) with both evaluators estimable across all three quality
# levels, compute Spearman's rho between the human and autograder
# implied-strength vectors of length 3, then aggregate (mean / median / exact
# agreement rate) across tasks within method. The caller picks whether the
# autograder side is GPT-5.4 or GPT-5.4-mini by passing the corresponding
# quality_level_total_score_data (built via build_quality_level_score_data).
summarise_human_autograder_quality_agreement <- function(score_data) {
  score_data %>%
    dplyr::filter(is.finite(value)) %>%
    dplyr::mutate(
      evaluator_type = dplyr::recode(
        as.character(evaluator_type),
        "LLM-as-a-Judge" = "LLM autograder",
        .default = as.character(evaluator_type)
      )
    ) %>%
    dplyr::select(evaluator_type, method, task_number, task_name, quality_level, value) %>%
    tidyr::pivot_wider(names_from = evaluator_type, values_from = value) %>%
    dplyr::filter(is.finite(Human), is.finite(`LLM autograder`)) %>%
    dplyr::group_by(method, task_number, task_name) %>%
    dplyr::summarise(
      n_quality_levels = dplyr::n(),
      spearman = if (
        dplyr::n() == 3L &&
          stats::sd(Human) > 0 &&
          stats::sd(`LLM autograder`) > 0
      ) {
        stats::cor(Human, `LLM autograder`, method = "spearman")
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    dplyr::filter(is.finite(spearman)) %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(
      n_tasks = dplyr::n(),
      mean_task_spearman = mean(spearman),
      median_task_spearman = stats::median(spearman),
      exact_agreement_rate = mean(spearman == 1),
      .groups = "drop"
    )
}

print_human_autograder_quality_agreement <- function(agreement_summary, autograder_label = "LLM autograder") {
  sub_header(sprintf("Human--%s quality-ordering agreement", autograder_label))
  cat(
    sprintf("For each task and method, Spearman correlation is computed across the three quality levels between human and %s implied strengths; rows report the mean across tasks.\n", autograder_label),
    sep = ""
  )
  print(
    agreement_summary %>%
      dplyr::mutate(
        `Mean task Spearman` = fmt_num(mean_task_spearman, 3),
        `Median task Spearman` = fmt_num(median_task_spearman, 3),
        `Exact agreement rate` = fmt_pct(exact_agreement_rate, 1)
      ) %>%
      dplyr::select(
        Method = method,
        `Tasks compared` = n_tasks,
        `Mean task Spearman`,
        `Median task Spearman`,
        `Exact agreement rate`
      ),
    n = nrow(agreement_summary)
  )
  rubric_value <- agreement_summary$mean_task_spearman[agreement_summary$method == "Rubrics"]
  cj_value <- agreement_summary$mean_task_spearman[agreement_summary$method == "Comparative Judgments"]
  cat("\nSentence values:\n")
  cat(sprintf("Rubrics:                 %s\n", fmt_num(rubric_value, 3)))
  cat(sprintf("Comparative Judgments:   %s\n", fmt_num(cj_value, 3)))
}

# Inter-rater reliability for rubric scoring on overlapping cells.
# A "cell" is a (task, quality level, output version) triple rated by >=2
# annotators. The function reports cell counts, pairwise Pearson and Spearman
# correlations on the score share (rubric_total / rubric_max), the mean
# absolute difference of score shares within cells, and ICC(1,1) from a
# one-way ANOVA over the cells (this last value is the IRR statistic
# reported in the paper).
compute_rubric_overlap_irr <- function(raw_dat) {
  ratings <- raw_dat %>%
    dplyr::filter(
      method == "rubric",
      status == "completed",
      !is.na(rubric_total_points),
      !is.na(version_number),
      !is.na(user_email)
    ) %>%
    dplyr::mutate(
      rubric_max_points = suppressWarnings(as.numeric(rubric_max_points)),
      score = rubric_total_points / rubric_max_points
    ) %>%
    dplyr::filter(
      is.finite(rubric_max_points),
      rubric_max_points > 0,
      is.finite(score)
    ) %>%
    dplyr::transmute(
      item = paste(task_number, rung_label, version_number, sep = "|"),
      rater = user_email,
      score
    ) %>%
    dplyr::distinct(item, rater, .keep_all = TRUE)

  cell_counts <- ratings %>% dplyr::count(item, name = "n_raters")
  overlap_items <- cell_counts %>% dplyr::filter(n_raters >= 2)

  if (nrow(overlap_items) == 0) {
    return(NULL)
  }

  overlap_ratings <- ratings %>% dplyr::semi_join(overlap_items, by = "item")

  pairs <- overlap_ratings %>%
    dplyr::group_by(item) %>%
    dplyr::group_modify(~ {
      sc <- .x$score
      idx <- utils::combn(length(sc), 2)
      tibble::tibble(score_a = sc[idx[1, ]], score_b = sc[idx[2, ]])
    }) %>%
    dplyr::ungroup()

  pearson <- stats::cor(pairs$score_a, pairs$score_b, method = "pearson")
  spearman <- stats::cor(pairs$score_a, pairs$score_b, method = "spearman")
  mae <- mean(abs(pairs$score_a - pairs$score_b))

  fit <- stats::aov(score ~ factor(item), data = overlap_ratings)
  s <- summary(fit)[[1]]
  ms_between <- s[1, "Mean Sq"]
  ms_within <- s[2, "Mean Sq"]

  n_per_item <- overlap_ratings %>% dplyr::count(item) %>% dplyr::pull(n)
  k_groups <- length(n_per_item)
  total_n <- sum(n_per_item)
  k0 <- if (k_groups > 1) {
    (total_n - sum(n_per_item^2) / total_n) / (k_groups - 1)
  } else {
    NA_real_
  }
  icc_11 <- if (is.finite(k0) && (ms_between + (k0 - 1) * ms_within) > 0) {
    (ms_between - ms_within) / (ms_between + (k0 - 1) * ms_within)
  } else {
    NA_real_
  }

  list(
    n_overlap_items = nrow(overlap_items),
    n_overlap_ratings = nrow(overlap_ratings),
    n_pairs = nrow(pairs),
    rater_count_breakdown = table(overlap_items$n_raters),
    pearson = pearson,
    spearman = spearman,
    mae = mae,
    icc_11 = icc_11
  )
}

# Adjacent-pair recovery accuracy. For each task and method, look at each pair
# of quality levels and ask whether the higher-quality level received the
# higher implied score. "Adjacent" pools intermediate-vs-good and good-vs-
# excellent; "non-adjacent" is intermediate-vs-excellent only. Returns one row
# per (evaluator_type, method) with the per-pair and pooled accuracy rates
# that populate Table 5.
compute_adjacent_pair_accuracy <- function(score_data, truth_order = c("intermediate", "good", "excellent")) {
  pair_levels <- c("intermediate vs good", "good vs excellent", "intermediate vs excellent")

  wide <- score_data %>%
    dplyr::filter(is.finite(value)) %>%
    dplyr::mutate(quality_level = as.character(quality_level)) %>%
    tidyr::pivot_wider(
      id_cols = c("evaluator_type", "method", "task_number", "task_name"),
      names_from = "quality_level",
      values_from = "value"
    ) %>%
    dplyr::filter(
      !is.na(.data$intermediate),
      !is.na(.data$good),
      !is.na(.data$excellent)
    )

  if (nrow(wide) == 0) {
    return(tibble::tibble(
      evaluator_type = factor(character(), levels = levels(score_data$evaluator_type)),
      method = factor(character(), levels = levels(score_data$method)),
      pair = factor(character(), levels = c(pair_levels, "adjacent", "non-adjacent")),
      n_tasks = integer(),
      n_correct = integer(),
      accuracy = numeric()
    ))
  }

  per_task <- wide %>%
    dplyr::transmute(
      evaluator_type,
      method,
      task_number,
      `intermediate vs good` = as.integer(.data$good > .data$intermediate),
      `good vs excellent` = as.integer(.data$excellent > .data$good),
      `intermediate vs excellent` = as.integer(.data$excellent > .data$intermediate)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(pair_levels),
      names_to = "pair",
      values_to = "correct"
    )

  per_pair <- per_task %>%
    dplyr::group_by(evaluator_type, method, pair) %>%
    dplyr::summarise(
      n_tasks = dplyr::n(),
      n_correct = sum(correct),
      accuracy = mean(correct),
      .groups = "drop"
    )

  adjacent <- per_pair %>%
    dplyr::filter(pair %in% c("intermediate vs good", "good vs excellent")) %>%
    dplyr::group_by(evaluator_type, method) %>%
    dplyr::summarise(
      pair = "adjacent",
      n_tasks = sum(n_tasks),
      n_correct = sum(n_correct),
      accuracy = sum(n_correct) / sum(n_tasks),
      .groups = "drop"
    )

  non_adjacent <- per_pair %>%
    dplyr::filter(pair == "intermediate vs excellent") %>%
    dplyr::transmute(
      evaluator_type,
      method,
      pair = "non-adjacent",
      n_tasks,
      n_correct,
      accuracy
    )

  dplyr::bind_rows(per_pair, adjacent, non_adjacent) %>%
    dplyr::mutate(
      pair = factor(pair, levels = c(pair_levels, "adjacent", "non-adjacent"))
    ) %>%
    dplyr::arrange(evaluator_type, method, pair)
}

format_adjacency_table_wide <- function(adjacency_data) {
  pair_levels <- levels(adjacency_data$pair)
  adjacency_data %>%
    dplyr::transmute(
      evaluator_type = dplyr::recode(
        as.character(evaluator_type),
        "LLM-as-a-Judge" = "Autograder",
        .default = as.character(evaluator_type)
      ),
      method = dplyr::recode(
        as.character(method),
        "Comparative Judgments" = "CJ",
        .default = as.character(method)
      ),
      pair,
      cell = sprintf("%.3f", accuracy)
    ) %>%
    dplyr::mutate(
      evaluator_order = factor(evaluator_type, levels = c("Human", "Autograder")),
      method_order = factor(method, levels = c("Rubrics", "CJ"))
    ) %>%
    tidyr::pivot_wider(
      id_cols = c("evaluator_type", "method", "evaluator_order", "method_order"),
      names_from = "pair",
      values_from = "cell"
    ) %>%
    dplyr::arrange(evaluator_order, method_order) %>%
    dplyr::select(
      `Evaluator` = evaluator_type,
      `Method` = method,
      dplyr::all_of(pair_levels)
    )
}

latex_adjacency_table <- function(adjacency_data) {
  pair_levels <- levels(adjacency_data$pair)
  header_label_map <- c(
    "intermediate vs good" = "I vs G",
    "good vs excellent" = "G vs E",
    "intermediate vs excellent" = "I vs E",
    "adjacent" = "Adjacent",
    "non-adjacent" = "Non-adj."
  )
  header_cells <- c("Evaluator", "Method", header_label_map[pair_levels])

  lines <- c(
    "% Requires \\usepackage{booktabs}",
    "\\begin{table}[!t]",
    "  \\caption{Adjacent-pair recovery accuracy. For each task, each method's implied score (mean rubric total or Bradley-Terry latent utility) is compared between the two indicated quality levels; a task is counted as correct if the higher-quality level receives the higher score. Cells report the share of tasks correctly ordered. Column headers abbreviate the three quality levels intermediate (I), good (G), and excellent (E). \\textit{Adjacent} pools I-vs-G and G-vs-E; \\textit{Non-adj.} is I-vs-E.}",
    "  \\label{tab:adjacent-pair-accuracy}",
    "  \\centering",
    "  \\small",
    "  \\setlength{\\tabcolsep}{4pt}",
    paste0("  \\begin{tabular}{ll", paste(rep("c", length(pair_levels)), collapse = ""), "}"),
    "    \\toprule",
    paste0("    ", paste(header_cells, collapse = " & "), " \\\\"),
    "    \\midrule"
  )

  display <- format_adjacency_table_wide(adjacency_data) %>%
    dplyr::mutate(
      Evaluator = as.character(Evaluator),
      Method = as.character(Method)
    )
  for (i in seq_len(nrow(display))) {
    if (i > 1 && display$Evaluator[[i]] != display$Evaluator[[i - 1]]) {
      lines <- c(lines, "    \\addlinespace[0.35em]")
    }
    values <- as.character(unlist(display[i, ], use.names = FALSE))
    values[is.na(values)] <- ""
    lines <- c(lines, paste0("    ", paste(values, collapse = " & "), " \\\\"))
  }

  c(
    lines,
    "    \\bottomrule",
    "  \\end{tabular}",
    "\\end{table}"
  )
}

plot_quality_level_scores <- function(
  score_data,
  title = "Score Distributions",
  x_label = "Average rubric score share of maximum or BT latent utility",
  rubric_binwidth = 0.05,
  cj_binwidth = 0.2
) {
  plot_df <- score_data %>%
    dplyr::filter(is.finite(value))
  rubric_plot_df <- plot_df %>%
    dplyr::filter(method == "Rubrics")
  cj_plot_df <- plot_df %>%
    dplyr::filter(method == "Comparative Judgments")
  fill_values <- c(
    "Comparative Judgments" = "#1B9E77",
    "Rubrics" = "#D95F02"
  )
  rubric_axis_anchor_df <- tidyr::expand_grid(
    evaluator_type = levels(score_data$evaluator_type),
    method = "Rubrics",
    value = c(0, 1)
  ) %>%
    dplyr::mutate(
      evaluator_type = factor(evaluator_type, levels = levels(score_data$evaluator_type)),
      method = factor(method, levels = levels(score_data$method))
    )

  if (nrow(plot_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = title
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
    )
  }

  ggplot2::ggplot() +
    ggplot2::geom_blank(
      data = rubric_axis_anchor_df,
      ggplot2::aes(x = value, y = 0)
    ) +
    ggplot2::geom_histogram(
      data = rubric_plot_df,
      ggplot2::aes(
        x = value,
        y = ggplot2::after_stat(count / sum(count)),
        fill = method,
        group = interaction(method, evaluator_type)
      ),
      binwidth = rubric_binwidth,
      boundary = 0,
      color = "white",
      linewidth = 0.45,
      alpha = 0.95
    ) +
    ggplot2::geom_histogram(
      data = cj_plot_df,
      ggplot2::aes(
        x = value,
        y = ggplot2::after_stat(count / sum(count)),
        fill = method,
        group = interaction(method, evaluator_type)
      ),
      binwidth = cj_binwidth,
      boundary = 0,
      color = "white",
      linewidth = 0.45,
      alpha = 0.95
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(evaluator_type),
      cols = ggplot2::vars(method),
      scales = "free_x",
      switch = "y"
    ) +
    ggplot2::scale_fill_manual(values = fill_values, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = function(limits) pretty(limits, n = 5),
      expand = ggplot2::expansion(mult = c(0.04, 0.06))
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%"),
      expand = ggplot2::expansion(mult = c(0, 0.10))
    ) +
    ggplot2::coord_cartesian(clip = "on") +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = "Relative Frequency"
    ) +
    plot_theme_publication(base_size = 14) +
    ggplot2::theme(
      strip.placement = "outside",
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 20,
        hjust = 0.5,
        margin = ggplot2::margin(b = 12)
      ),
      plot.subtitle = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = 13,
        margin = ggplot2::margin(t = 5, r = 4, b = 5, l = 4)
      ),
      strip.text.y = ggplot2::element_text(
        angle = 90,
        face = "bold",
        size = 11,
        margin = ggplot2::margin(t = 4, r = 5, b = 4, l = 5)
      ),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      panel.spacing = ggplot2::unit(1.2, "lines"),
      strip.switch.pad.grid = ggplot2::unit(0.7, "lines"),
      plot.margin = ggplot2::margin(t = 24, r = 24, b = 18, l = 46)
    )
}

plot_rubric_scores_by_quality_level <- function(
  score_data,
  title = "Rubric Score Distributions by Quality Level",
  x_label = "Rubric score share of positive maximum",
  binwidth = 0.05
) {
  plot_df <- score_data %>%
    dplyr::filter(method == "Rubrics", is.finite(value)) %>%
    dplyr::mutate(
      quality_level = factor(
        tools::toTitleCase(as.character(quality_level)),
        levels = tools::toTitleCase(levels(score_data$quality_level))
      )
    )

  axis_anchor_df <- tidyr::expand_grid(
    evaluator_type = levels(score_data$evaluator_type),
    quality_level = levels(plot_df$quality_level),
    value = c(0, 1)
  ) %>%
    dplyr::mutate(
      evaluator_type = factor(evaluator_type, levels = levels(score_data$evaluator_type)),
      quality_level = factor(quality_level, levels = levels(plot_df$quality_level))
    )

  if (nrow(plot_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::labs(title = title) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    )
  }

  ggplot2::ggplot() +
    ggplot2::geom_blank(
      data = axis_anchor_df,
      ggplot2::aes(x = value, y = 0)
    ) +
    ggplot2::geom_histogram(
      data = plot_df,
      ggplot2::aes(
        x = value,
        y = ggplot2::after_stat(count / sum(count)),
        group = interaction(evaluator_type, quality_level)
      ),
      fill = "#D95F02",
      binwidth = binwidth,
      boundary = 0,
      color = "white",
      linewidth = 0.45,
      alpha = 0.95
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(evaluator_type),
      cols = ggplot2::vars(quality_level),
      switch = "y"
    ) +
    ggplot2::scale_x_continuous(
      breaks = function(limits) pretty(limits, n = 5),
      expand = ggplot2::expansion(mult = c(0.04, 0.06))
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%"),
      expand = ggplot2::expansion(mult = c(0, 0.10))
    ) +
    ggplot2::coord_cartesian(clip = "on") +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = "Relative Frequency"
    ) +
    plot_theme_publication(base_size = 14) +
    ggplot2::theme(
      strip.placement = "outside",
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 20,
        hjust = 0.5,
        margin = ggplot2::margin(b = 12)
      ),
      plot.subtitle = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = 13,
        margin = ggplot2::margin(t = 5, r = 4, b = 5, l = 4)
      ),
      strip.text.y = ggplot2::element_text(
        angle = 90,
        face = "bold",
        size = 11,
        margin = ggplot2::margin(t = 4, r = 5, b = 4, l = 5)
      ),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      panel.spacing = ggplot2::unit(1.2, "lines"),
      strip.switch.pad.grid = ggplot2::unit(0.7, "lines"),
      plot.margin = ggplot2::margin(t = 24, r = 24, b = 18, l = 46)
    )
}

plot_bootstrap_distribution <- function(boot_tbl, observed_D_hat = NULL) {
  if (!("evaluator_type" %in% names(boot_tbl))) {
    boot_tbl$evaluator_type <- "Human"
  }

  plot_df <- boot_tbl %>%
    dplyr::transmute(
      evaluator_type = .data$evaluator_type,
      bootstrap_D_hat = .data$D_hat
    ) %>%
    dplyr::filter(is.finite(.data$bootstrap_D_hat)) %>%
    dplyr::mutate(
      evaluator_type = factor(
        .data$evaluator_type,
        levels = c("Human", "LLM-as-a-Judge")
      )
    )
  line_color <- "#1565C0"
  estimate_color <- "#B45309"

  if (nrow(plot_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = "Bootstrap Distribution Unavailable"
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
    )
  }

  observed_df <- tibble::tibble(
    evaluator_type = character(),
    observed_D_hat = numeric()
  )

  if (!is.null(observed_D_hat)) {
    if (is.null(names(observed_D_hat))) {
      names(observed_D_hat) <- levels(plot_df$evaluator_type)[seq_along(observed_D_hat)]
    }
    observed_df <- tibble::tibble(
      evaluator_type = factor(names(observed_D_hat), levels = levels(plot_df$evaluator_type)),
      observed_D_hat = as.numeric(observed_D_hat)
    ) %>%
      dplyr::filter(!is.na(.data$evaluator_type), is.finite(.data$observed_D_hat))
  }

  plot_df <- plot_df %>%
    dplyr::left_join(observed_df, by = "evaluator_type") %>%
    dplyr::group_by(.data$evaluator_type) %>%
    dplyr::mutate(
      bootstrap_center = mean(.data$bootstrap_D_hat, na.rm = TRUE),
      plot_center = dplyr::if_else(
        is.finite(.data$observed_D_hat),
        .data$observed_D_hat,
        .data$bootstrap_center
      ),
      plotted_D_hat = .data$plot_center + (.data$bootstrap_D_hat - .data$bootstrap_center)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(.data$plotted_D_hat))

  observed_line_df <- plot_df %>%
    dplyr::distinct(.data$evaluator_type, .data$plot_center)

  ci_df <- plot_df %>%
    dplyr::group_by(.data$evaluator_type) %>%
    dplyr::summarise(
      plot_center = dplyr::first(.data$plot_center),
      bootstrap_se_D_hat = stats::sd(.data$bootstrap_D_hat),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      centered_ci_lower = .data$plot_center - stats::qnorm(0.975) * .data$bootstrap_se_D_hat,
      centered_ci_upper = .data$plot_center + stats::qnorm(0.975) * .data$bootstrap_se_D_hat
    ) %>%
    dplyr::filter(
      is.finite(.data$centered_ci_lower),
      is.finite(.data$centered_ci_upper)
    ) %>%
    tidyr::pivot_longer(
      cols = c("centered_ci_lower", "centered_ci_upper"),
      names_to = "bound",
      values_to = "xintercept"
    )

  # Keep only a small amount of x-axis padding while preserving all interval lines.
  x_range <- range(c(plot_df$plotted_D_hat, observed_line_df$plot_center, ci_df$xintercept, 0), na.rm = TRUE)
  x_span <- diff(x_range)

  if (!is.finite(x_span) || x_span <= 0) {
    x_span <- 1
  }

  x_left <- x_range[[1]] - 0.035 * x_span
  x_right <- x_range[[2]] + 0.035 * x_span

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$plotted_D_hat)) +
    ggplot2::geom_histogram(
      bins = 30,
      fill = "grey75",
      color = "black",
      linewidth = 0.35
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      color = line_color,
      linetype = "solid",
      linewidth = 1.1
    ) +
    ggplot2::geom_vline(
      data = ci_df,
      ggplot2::aes(xintercept = .data$xintercept),
      color = estimate_color,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::geom_vline(
      data = observed_line_df,
      ggplot2::aes(xintercept = .data$plot_center),
      color = estimate_color,
      linetype = "solid",
      linewidth = 1.05
    ) +
    ggplot2::facet_wrap(
      ~ evaluator_type,
      nrow = 1
    ) +
    ggplot2::coord_cartesian(
      xlim = c(x_left, x_right),
      clip = "on"
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    ggplot2::labs(
      title = "Bootstrap Uncertainty Around the Observed Study-Level Method Difference",
      x = expression(widehat(D)),
      y = "Bootstrap replicates"
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 20,
        hjust = 0.5,
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 16),
      axis.text = ggplot2::element_text(size = 13),
      panel.grid.minor = ggplot2::element_line(linewidth = 0.25),
      strip.background = ggplot2::element_rect(fill = "grey94", color = "grey70", linewidth = 0.35),
      strip.text = ggplot2::element_text(face = "bold", size = 14),
      panel.spacing = ggplot2::unit(1.0, "lines"),
      plot.margin = ggplot2::margin(t = 28, r = 28, b = 12, l = 12)
    )
}

# ============================================================
# Console reporting
# ============================================================

print_validation_summary <- function(validation, study_estimate) {
  block_summary <- validation$block_summary
  
  rubric_completed <- sum(block_summary$method == "rubric" & block_summary$is_complete_expected_shape)
  pref_completed <- sum(block_summary$method == "preference" & block_summary$is_complete_expected_shape)
  rubric_excluded <- sum(block_summary$method == "rubric" & !block_summary$is_complete_expected_shape)
  pref_excluded <- sum(block_summary$method == "preference" & !block_summary$is_complete_expected_shape)
  cluster_label <- mode_cluster_label(validation$analysis_mode)
  
  sub_header("Sample and analysis set")
  cat(sprintf("Complete rubric blocks:      %d\n", rubric_completed))
  cat(sprintf("Complete CJ blocks:          %d\n", pref_completed))
  cat(sprintf("%-30s %d\n", paste0(validation$analysis_unit_label, ":"), validation$analysis_unit_count))
  cat(sprintf("Excluded rubric blocks:      %d\n", rubric_excluded))
  cat(sprintf("Excluded CJ blocks:          %d\n", pref_excluded))
  cat(sprintf("%-30s %d\n", paste0(cluster_label, " in bootstrap:"), validation$analysis_cluster_count))
  cat(sprintf("Estimable rubric tasks:      %d\n", study_estimate$n_tasks_rubric[[1]]))
  cat(sprintf("Estimable CJ tasks:          %d\n", study_estimate$n_tasks_preference[[1]]))
}

print_method_summary <- function(method_summary) {
  out <- method_summary %>%
    dplyr::transmute(
      Method = dplyr::if_else(method == "rubric", "Rubrics", "Comparative Judgments"),
      `Estimable tasks` = n_estimable_tasks,
      `Mean task-level rho` = round(mean_task_rho, 3),
      `Median task-level rho` = round(median_task_rho, 3),
      `Exact recovery rate` = paste0(round(100 * exact_recovery_rate, 1), "%")
    )
  
  sub_header("Method-level results")
  print(out, n = nrow(out))
}

print_method_agreement <- function(method_agreement) {
  sub_header("Method agreement (Rubric ranks vs. CJ ranks, per task)")
  finite_rho <- method_agreement$rho_method_agreement[is.finite(method_agreement$rho_method_agreement)]

  if (length(finite_rho) == 0) {
    cat("No tasks had estimable rankings under both methods.\n")
    return(invisible(NULL))
  }

  cat(sprintf("Tasks with both methods estimable:     %d\n", length(finite_rho)))
  cat(sprintf("Mean task-level rho (rubric vs CJ):    %s\n", fmt_num(mean(finite_rho), 3)))
  cat(sprintf("Median task-level rho (rubric vs CJ):  %s\n", fmt_num(stats::median(finite_rho), 3)))
  cat(sprintf("Tasks with rho = 1 (full agreement):   %s\n", fmt_pct(mean(finite_rho == 1), 1)))
  cat(sprintf("Tasks with rho >= 0.5:                 %s\n", fmt_pct(mean(finite_rho >= 0.5), 1)))
  cat(sprintf("Tasks with rho <= 0:                   %s\n", fmt_pct(mean(finite_rho <= 0), 1)))
}

detect_score_tie_groups <- function(score_named_vector, truth_order, tolerance = score_tie_tolerance) {
  score_named_vector <- score_named_vector[truth_order]

  if (any(is.na(score_named_vector))) {
    return(list())
  }

  ord <- order(score_named_vector)
  groups <- list()
  current_group <- ord[[1]]
  anchor <- score_named_vector[[ord[[1]]]]

  flush_group <- function(idx) {
    if (length(idx) > 1L) {
      groups[[length(groups) + 1L]] <<- names(score_named_vector)[idx]
    }
  }

  for (i in ord[-1]) {
    if (abs(score_named_vector[[i]] - anchor) <= tolerance) {
      current_group <- c(current_group, i)
    } else {
      flush_group(current_group)
      current_group <- i
      anchor <- score_named_vector[[i]]
    }
  }

  flush_group(current_group)
  groups
}

build_tie_rows_for_method <- function(task_summary, method, truth_order) {
  if (nrow(task_summary) == 0) {
    return(tibble::tibble(task_number = numeric(), tie_text = character()))
  }

  rho_col <- if (method == "rubric") "rho_rubric" else "rho_preference"

  purrr::map_dfr(seq_len(nrow(task_summary)), function(i) {
    row <- task_summary[i, , drop = FALSE]
    scores <- stats::setNames(
      c(row$score_intermediate, row$score_good, row$score_excellent),
      c("intermediate", "good", "excellent")
    )
    tie_groups <- detect_score_tie_groups(scores, truth_order)

    if (length(tie_groups) == 0) {
      return(tibble::tibble())
    }

    tie_type <- if (length(tie_groups) == 1L && length(tie_groups[[1]]) == length(truth_order)) {
      "complete"
    } else {
      "partial"
    }
    tie_label <- paste(vapply(tie_groups, paste, collapse = " = ", FUN.VALUE = character(1)), collapse = "; ")
    rho_value <- row[[rho_col]][[1]]

    tibble::tibble(
      task_number = row$task_number[[1]],
      tie_text = sprintf(
        "Task %02d: %s, %s; rho %s",
        as.integer(row$task_number[[1]]),
        tie_type,
        tie_label,
        fmt_num(rho_value, 3)
      )
    )
  }) %>%
    dplyr::arrange(task_number)
}

format_tie_cell <- function(tie_rows) {
  if (nrow(tie_rows) == 0) {
    return("None")
  }

  paste(tie_rows$tie_text, collapse = "; ")
}

print_tie_summary_table <- function(
  human_rubric_task_summary,
  human_pref_task_summary,
  gpt54_rubric_task_summary,
  gpt54_pref_task_summary,
  mini_rubric_task_summary,
  mini_pref_task_summary,
  truth_order
) {
  tie_table <- tibble::tibble(
    Evaluator = c("Humans", "GPT-5.4", "GPT-5.4-mini"),
    Rubrics = c(
      format_tie_cell(build_tie_rows_for_method(human_rubric_task_summary, "rubric", truth_order)),
      format_tie_cell(build_tie_rows_for_method(gpt54_rubric_task_summary, "rubric", truth_order)),
      format_tie_cell(build_tie_rows_for_method(mini_rubric_task_summary, "rubric", truth_order))
    ),
    `Preferences / CJ` = c(
      format_tie_cell(build_tie_rows_for_method(human_pref_task_summary, "preference", truth_order)),
      format_tie_cell(build_tie_rows_for_method(gpt54_pref_task_summary, "preference", truth_order)),
      format_tie_cell(build_tie_rows_for_method(mini_pref_task_summary, "preference", truth_order))
    )
  )

  sub_header("Tie summary by evaluator and method")
  cat(sprintf("Tie tolerance on method-implied scores: %s\n", format(score_tie_tolerance, scientific = TRUE)))
  cat("| Evaluator | Rubrics | Preferences / CJ |\n")
  cat("|---|---|---|\n")
  for (i in seq_len(nrow(tie_table))) {
    cat(sprintf(
      "| %s | %s | %s |\n",
      tie_table$Evaluator[[i]],
      tie_table$Rubrics[[i]],
      tie_table$`Preferences / CJ`[[i]]
    ))
  }
}

print_main_result <- function(study_estimate, boot_summary, analysis_mode) {
  sub_header("Primary comparison")
  cat("Study-level estimand: D̂ = average Comparative Judgment rho - average Rubric rho\n")
  cat("Inference: ", mode_bootstrap_label(analysis_mode), "\n\n", sep = "")
  cat(sprintf("Rubric tasks averaged:                 %s\n", study_estimate$n_tasks_rubric[[1]]))
  cat(sprintf("Comparative Judgment tasks averaged:   %s\n", study_estimate$n_tasks_preference[[1]]))
  cat(sprintf("Observed mean Rubric rho:              %s\n", fmt_num(study_estimate$mean_rho_rubric[[1]], 3)))
  cat(sprintf("Observed mean Comparative Judgment rho: %s\n", fmt_num(study_estimate$mean_rho_preference[[1]], 3)))
  cat(sprintf("Observed D̂:                           %s\n", fmt_num(study_estimate$D_hat[[1]], 3)))
  cat(sprintf("Finite bootstrap draws:                %s of %s\n", boot_summary$n_finite_boot[[1]], boot_summary$n_boot[[1]]))
  cat(sprintf("Bootstrap SE for observed D̂:          %s\n", fmt_num(boot_summary$bootstrap_se_D_hat[[1]], 3)))
  cat(sprintf(
    "95%% centered bootstrap-SE CI for D̂:   [%s, %s]\n",
    fmt_num(boot_summary$centered_ci_lower[[1]], 3),
    fmt_num(boot_summary$centered_ci_upper[[1]], 3)
  ))
  cat(sprintf("Bootstrap mean D̂ (diagnostic):        %s\n", fmt_num(boot_summary$bootstrap_mean_D_hat[[1]], 3)))
  cat(sprintf(
    "Raw percentile bootstrap CI (diagnostic): [%s, %s]\n",
    fmt_num(boot_summary$ci_lower[[1]], 3),
    fmt_num(boot_summary$ci_upper[[1]], 3)
  ))
  cat(sprintf("Pr(D_boot > 0; diagnostic):           %s\n", fmt_pct(boot_summary$prop_D_hat_gt_0[[1]], 1)))
  cat(sprintf("Pr(D_boot < 0; diagnostic):           %s\n", fmt_pct(boot_summary$prop_D_hat_lt_0[[1]], 1)))

  if (boot_summary$n_finite_boot[[1]] == 0L) {
    cat(
      "Bootstrap note: no bootstrap replicate produced estimable method averages for both Rubrics and Comparative Judgments.\n",
      "This usually means the current completed sample is too sparse for one of the methods after resampling.\n",
      sep = ""
    )
  }
}

print_publication_timing_diagnostic <- function(dat_complete, outlier_threshold_minutes = 30) {
  time_rows <- dat_complete %>%
    dplyr::filter(is.finite(time_spent_seconds), time_spent_seconds >= 0) %>%
    dplyr::mutate(
      time_spent_minutes = time_spent_seconds / 60,
      method_display = method_display(method)
    )

  included_rows <- time_rows %>%
    dplyr::filter(time_spent_minutes <= outlier_threshold_minutes)

  outlier_rows <- time_rows %>%
    dplyr::filter(time_spent_minutes > outlier_threshold_minutes)

  total_seconds <- sum(included_rows$time_spent_seconds, na.rm = TRUE)
  total_hours <- floor(total_seconds / 3600)
  total_minutes <- floor((total_seconds %% 3600) / 60)
  total_decimal_hours <- total_seconds / 3600

  sub_header("Publication timing diagnostic")
  cat("Timing convention: completed analyzed human evaluation rows only.\n")
  cat(sprintf("Outlier threshold:                    > %s minutes\n", fmt_num(outlier_threshold_minutes, 0)))
  cat(sprintf("Rows with finite nonnegative time:     %d\n", nrow(time_rows)))
  cat(sprintf("Rows included in timing summary:       %d\n", nrow(included_rows)))
  cat(sprintf("Rows excluded as time outliers:        %d\n", nrow(outlier_rows)))

  if (nrow(outlier_rows) > 0) {
    cat(sprintf(
      "Outlier recorded-time range (min):     [%s, %s]\n",
      fmt_num(min(outlier_rows$time_spent_minutes, na.rm = TRUE), 1),
      fmt_num(max(outlier_rows$time_spent_minutes, na.rm = TRUE), 1)
    ))

    outlier_by_method <- outlier_rows %>%
      dplyr::count(method_display, name = "n_outliers") %>%
      dplyr::arrange(method_display)

    cat("Outliers by method:\n")
    print(outlier_by_method, n = nrow(outlier_by_method))
  } else {
    cat("Outlier recorded-time range (min):     none\n")
  }

  cat(sprintf(
    "Total attorney time after exclusion:   %d hours %d minutes (%s hours)\n",
    total_hours,
    total_minutes,
    fmt_num(total_decimal_hours, 1)
  ))
}

# ============================================================
# Run
# ============================================================
#
# Orchestrates the full analysis:
#   1. Read the released CSVs for humans, GPT-5.4, and GPT-5.4-mini.
#   2. Apply the complete-block inclusion rule.
#   3. Compute task-level rho's per method and the study-level D_hat.
#   4. Run the cluster-first hierarchical bootstrap for humans and for the
#      GPT-5.4 autograder.
#   5. Build subgroup, adjacency, IRR, and human-autograder-agreement tables.
#   6. Print a console summary and write figures + tables to output_dir.

analysis_mode <- validate_analysis_mode(analysis_mode)

section_header("Collapsed quality-level analysis for study results (v1.23)")
cat("JudgmentBench directory:\n", judgmentbench_dir, "\n", sep = "")
cat("Analysis mode:\n", analysis_mode, "\n", sep = "")

raw_dat <- read_study_results(
  analysis_mode = analysis_mode,
  judgmentbench_dir = judgmentbench_dir,
  evaluator_type = "human"
)
validation <- validate_blocks(
  dat = raw_dat,
  analysis_mode = analysis_mode
)

complete_dat <- validation$analysis_dat

autograder_raw_dat <- read_study_results(
  analysis_mode = "llm_both",
  judgmentbench_dir = judgmentbench_dir,
  evaluator_type = "autograder",
  autograder_subdir = "gpt_5_4",
  autograder_model_label = "gpt-5.4"
)
autograder_validation <- validate_blocks(
  dat = autograder_raw_dat,
  analysis_mode = "llm_both"
)
autograder_complete_dat <- autograder_validation$analysis_dat

mini_autograder_raw_dat <- read_study_results(
  analysis_mode = "llm_both",
  judgmentbench_dir = judgmentbench_dir,
  evaluator_type = "autograder",
  autograder_subdir = mini_autograder_subdir,
  autograder_model_label = "gpt-5.4-mini"
)
mini_autograder_validation <- validate_blocks(
  dat = mini_autograder_raw_dat,
  analysis_mode = "llm_both"
)
mini_autograder_complete_dat <- mini_autograder_validation$analysis_dat

pref_task_summary <- summarise_preference_tasks(complete_dat, truth_order)
rubric_task_summary <- summarise_rubric_tasks(complete_dat, truth_order)
study_estimate <- compute_study_estimate(pref_task_summary, rubric_task_summary)
method_summary <- build_method_summary(pref_task_summary, rubric_task_summary)
method_agreement <- summarise_method_agreement(
  rubric_task_summary,
  pref_task_summary,
  truth_order
)
autograder_pref_task_summary <- summarise_preference_tasks(autograder_complete_dat, truth_order)
autograder_rubric_task_summary <- summarise_rubric_tasks(autograder_complete_dat, truth_order)
autograder_study_estimate <- compute_study_estimate(
  autograder_pref_task_summary,
  autograder_rubric_task_summary
)
autograder_method_summary <- build_method_summary(
  autograder_pref_task_summary,
  autograder_rubric_task_summary
)
autograder_method_agreement <- summarise_method_agreement(
  autograder_rubric_task_summary,
  autograder_pref_task_summary,
  truth_order
)
mini_autograder_pref_task_summary <- summarise_preference_tasks(mini_autograder_complete_dat, truth_order)
mini_autograder_rubric_task_summary <- summarise_rubric_tasks(mini_autograder_complete_dat, truth_order)
main_plot_data <- build_main_plot_data(
  pref_task_summary = pref_task_summary,
  rubric_task_summary = rubric_task_summary,
  dat_complete = complete_dat
)
main_plot <- plot_main_result_panel(main_plot_data)
block_rho_summary <- build_block_rho_summary(complete_dat, truth_order)
lawyer_dt_data <- build_lawyer_dt_data(block_rho_summary)
experience_plot <- plot_experience_dt(lawyer_dt_data)
subgroup_results <- build_subgroup_results_table(
  dat_complete = complete_dat,
  truth_order = truth_order,
  autograder_dat_complete = autograder_complete_dat,
  autograder_mini_dat_complete = mini_autograder_complete_dat
)
subgroup_display_table <- format_subgroup_results_table(subgroup_results)
rubric_score_data <- build_positive_rubric_score_data(
  human_dat_complete = complete_dat,
  autograder_dat_complete = autograder_complete_dat
)
rubric_score_plot <- plot_rubric_score_fractions(rubric_score_data)
quality_level_positive_score_data <- build_quality_level_score_data(
  human_dat_complete = complete_dat,
  autograder_dat_complete = autograder_complete_dat,
  truth_order = truth_order,
  rubric_score_type = "positive"
)
quality_level_positive_score_plot <- plot_rubric_scores_by_quality_level(
  quality_level_positive_score_data,
  title = "Rubric Score Distributions by Quality Level",
  x_label = "Rubric score share of positive maximum"
)
quality_level_total_score_data <- build_quality_level_score_data(
  human_dat_complete = complete_dat,
  autograder_dat_complete = autograder_complete_dat,
  truth_order = truth_order,
  rubric_score_type = "total"
)
human_autograder_quality_agreement <- summarise_human_autograder_quality_agreement(
  quality_level_total_score_data
)
mini_quality_level_total_score_data <- build_quality_level_score_data(
  human_dat_complete = complete_dat,
  autograder_dat_complete = mini_autograder_complete_dat,
  truth_order = truth_order,
  rubric_score_type = "total"
)
human_mini_autograder_quality_agreement <- summarise_human_autograder_quality_agreement(
  mini_quality_level_total_score_data
)
adjacency_data <- compute_adjacent_pair_accuracy(
  quality_level_total_score_data,
  truth_order = truth_order
)

cat("\nRunning human ", mode_bootstrap_label(analysis_mode), "...\n", sep = "")
boot_out <- bootstrap_cluster_hierarchical(
  dat_complete = complete_dat,
  truth_order = truth_order,
  n_boot = n_boot,
  seed = seed,
  observed_D_hat = study_estimate$D_hat[[1]]
)

cat("\nRunning LLM-as-a-Judge ", mode_bootstrap_label("llm_both"), "...\n", sep = "")
autograder_boot_out <- bootstrap_cluster_hierarchical(
  dat_complete = autograder_complete_dat,
  truth_order = truth_order,
  n_boot = n_boot,
  seed = seed + 1L,
  observed_D_hat = autograder_study_estimate$D_hat[[1]]
)

boot_tbl <- dplyr::bind_rows(
  boot_out$boot_tbl %>%
    dplyr::mutate(evaluator_type = "Human"),
  autograder_boot_out$boot_tbl %>%
    dplyr::mutate(evaluator_type = "LLM-as-a-Judge")
)
boot_summary <- boot_out$summary
autograder_boot_summary <- autograder_boot_out$summary

bootstrap_plot <- plot_bootstrap_distribution(
  boot_tbl = boot_tbl,
  observed_D_hat = c(
    "Human" = study_estimate$D_hat[[1]],
    "LLM-as-a-Judge" = autograder_study_estimate$D_hat[[1]]
  )
)

sub_header("Human annotators")
print_validation_summary(validation, study_estimate)
print_method_summary(method_summary)
print_main_result(study_estimate, boot_summary, analysis_mode = analysis_mode)
print_method_agreement(method_agreement)
print_publication_timing_diagnostic(complete_dat)
sub_header("LLM-as-a-Judge")
print_validation_summary(autograder_validation, autograder_study_estimate)
print_method_summary(autograder_method_summary)
print_main_result(
  autograder_study_estimate,
  autograder_boot_summary,
  analysis_mode = "llm_both"
)
print_method_agreement(autograder_method_agreement)
print_tie_summary_table(
  human_rubric_task_summary = rubric_task_summary,
  human_pref_task_summary = pref_task_summary,
  gpt54_rubric_task_summary = autograder_rubric_task_summary,
  gpt54_pref_task_summary = autograder_pref_task_summary,
  mini_rubric_task_summary = mini_autograder_rubric_task_summary,
  mini_pref_task_summary = mini_autograder_pref_task_summary,
  truth_order = truth_order
)
sub_header("Experience diagnostic")
cat(sprintf("Lawyer/pipeline instances plotted: %d\n", nrow(lawyer_dt_data)))
sub_header("Subgroup table")
print(subgroup_display_table, n = nrow(subgroup_display_table))
sub_header("Positive-only rubric score fractions")
print(
  rubric_score_data %>%
    dplyr::group_by(.data$evaluator_type) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(score_fraction),
      median = stats::median(score_fraction),
      p25 = as.numeric(stats::quantile(score_fraction, probs = 0.25)),
      p75 = as.numeric(stats::quantile(score_fraction, probs = 0.75)),
      .groups = "drop"
    )
)
sub_header("Task-quality-level positive-point score distributions")
print(
  quality_level_positive_score_data %>%
    dplyr::filter(is.finite(value)) %>%
    dplyr::count(.data$evaluator_type, .data$method, name = "n_observations")
)
print_human_autograder_quality_agreement(human_autograder_quality_agreement, autograder_label = "GPT-5.4 autograder")
print_human_autograder_quality_agreement(human_mini_autograder_quality_agreement, autograder_label = "GPT-5.4-mini autograder")
sub_header("Adjacent-pair recovery accuracy")
print(format_adjacency_table_wide(adjacency_data), n = nrow(adjacency_data))
sub_header("Rubric IRR on overlapping (task, rung, version) cells (humans)")
human_rubric_irr <- compute_rubric_overlap_irr(raw_dat)
if (is.null(human_rubric_irr)) {
  cat("No (task, rung, version) cells have >= 2 human raters.\n")
} else {
  cat(sprintf("Cells with >= 2 raters:        %d\n", human_rubric_irr$n_overlap_items))
  cat(sprintf("Total ratings on those cells:  %d\n", human_rubric_irr$n_overlap_ratings))
  cat(sprintf("Within-cell rater pairs:       %d\n", human_rubric_irr$n_pairs))
  cat("Rater-count breakdown (n_raters per cell):\n")
  print(human_rubric_irr$rater_count_breakdown)
  cat(sprintf("Pairwise Pearson r:            %.3f\n", human_rubric_irr$pearson))
  cat(sprintf("Pairwise Spearman rho:         %.3f\n", human_rubric_irr$spearman))
  cat(sprintf("Mean abs diff (score share):   %.3f\n", human_rubric_irr$mae))
  cat(sprintf("ICC(1,1) (one-way ANOVA):      %.3f\n", human_rubric_irr$icc_11))
}

if (interactive()) {
  print(main_plot)
  print(experience_plot)
  print(rubric_score_plot)
  print(quality_level_positive_score_plot)
  print(bootstrap_plot)
}

if (isTRUE(save_plot)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  bootstrap_plot_path <- plot_path %||% file.path(
    output_dir,
    "figure2_bootstrap_distribution.png"
  )
  main_plot_path <- file.path(
    output_dir,
    "figure1_main_results.png"
  )
  experience_plot_path <- file.path(
    output_dir,
    "figure4_experience_diagnostic.png"
  )
  table_csv_path <- file.path(
    output_dir,
    "table4_subgroup_recovery.csv"
  )
  table_tex_path <- file.path(
    output_dir,
    "table4_subgroup_recovery.tex"
  )
  rubric_score_plot_path <- file.path(
    output_dir,
    "rubric_score_diagnostic.png"
  )
  quality_level_positive_score_plot_path <- file.path(
    output_dir,
    "figure3_quality_level_score_distributions.png"
  )
  adjacency_csv_path <- file.path(
    output_dir,
    "table5_adjacent_pair_recovery.csv"
  )
  adjacency_tex_path <- file.path(
    output_dir,
    "table5_adjacent_pair_recovery.tex"
  )
  ggplot2::ggsave(
    filename = main_plot_path,
    plot = add_top_strip_panel_padding(main_plot),
    width = 11.8,
    height = 7.8,
    dpi = 320,
    device = publication_png_device(320),
    bg = "white"
  )
  ggplot2::ggsave(
    filename = experience_plot_path,
    plot = experience_plot,
    width = 8.8,
    height = 6.2,
    dpi = 320,
    device = publication_png_device(320),
    bg = "white"
  )
  readr::write_csv(subgroup_results, table_csv_path)
  write_lines(latex_subgroup_results_table(subgroup_display_table), table_tex_path)
  readr::write_csv(adjacency_data, adjacency_csv_path)
  write_lines(latex_adjacency_table(adjacency_data), adjacency_tex_path)
  ggplot2::ggsave(
    filename = rubric_score_plot_path,
    plot = rubric_score_plot,
    width = 8.2,
    height = 9,
    dpi = 320,
    device = publication_png_device(320),
    bg = "white"
  )
  ggplot2::ggsave(
    filename = quality_level_positive_score_plot_path,
    plot = quality_level_positive_score_plot,
    width = 13.5,
    height = 6.8,
    dpi = 320,
    device = publication_png_device(320),
    bg = "white"
  )
  ggplot2::ggsave(
    filename = bootstrap_plot_path,
    plot = bootstrap_plot,
    width = 12,
    height = 6.5,
    dpi = 320,
    device = publication_png_device(320),
    bg = "white"
  )
  cat("\nSaved figure1_main_results.png to:\n", main_plot_path, "\n", sep = "")
  cat("Saved figure4_experience_diagnostic.png to:\n", experience_plot_path, "\n", sep = "")
  cat("Saved table4_subgroup_recovery.csv to:\n", table_csv_path, "\n", sep = "")
  cat("Saved table4_subgroup_recovery.tex to:\n", table_tex_path, "\n", sep = "")
  cat("Saved rubric_score_diagnostic.png to:\n", rubric_score_plot_path, "\n", sep = "")
  cat("Saved figure3_quality_level_score_distributions.png to:\n", quality_level_positive_score_plot_path, "\n", sep = "")
  cat("Saved table5_adjacent_pair_recovery.csv to:\n", adjacency_csv_path, "\n", sep = "")
  cat("Saved table5_adjacent_pair_recovery.tex to:\n", adjacency_tex_path, "\n", sep = "")
  cat("\nSaved figure2_bootstrap_distribution.png to:\n", bootstrap_plot_path, "\n", sep = "")
}
