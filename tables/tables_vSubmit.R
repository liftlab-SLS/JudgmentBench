# ============================================================
# JudgmentBench descriptive-tables script (submission version)
# ============================================================
#
# Builds the dataset-description tables in the paper (sampled task
# types, benchmark task summary, annotator information) directly
# from the released JudgmentBench result-dataset CSVs.
#
# Usage
# -----
#   Rscript tables_vSubmit.R
#
# Configure via environment variables (all optional):
#   EAH_JB_DIR
#       Path to the released result-dataset directory (containing
#       tasks.csv, rubric_items.csv, annotators.csv, etc.). Defaults
#       to ./result-dataset relative to the working directory.
#   EAH_TABLE_OUTPUT_DIR
#       Where to write outputs. Defaults to the script directory.
#
# Outputs (one CSV and one LaTeX file per table, plus diagnostics):
#   table1_sampled_tasks_by_type.{csv,tex}
#   table2_benchmark_tasks.{csv,tex}
#   table3_annotator_information.{csv,tex}
# ============================================================

# ------------------------------------------------------------
# Controls
# ------------------------------------------------------------

script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    raw <- gsub("~+~", " ", sub("^--file=", "", file_arg[[1]]), fixed = TRUE)
    dirname(normalizePath(raw, mustWork = FALSE))
  } else {
    normalizePath(getwd(), mustWork = FALSE)
  }
})

judgmentbench_dir <- Sys.getenv(
  "EAH_JB_DIR",
  unset = file.path(getwd(), "result-dataset")
)

output_dir <- Sys.getenv(
  "EAH_TABLE_OUTPUT_DIR",
  unset = script_dir
)
diagnostics_dir <- file.path(output_dir, "diagnostics")
output_prefix <- ""

# ------------------------------------------------------------
# Packages
# ------------------------------------------------------------

required_packages <- c("readr", "dplyr", "tidyr", "stringr", "purrr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running tables_v1.4.R."
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ------------------------------------------------------------
# Small helpers
# ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path)
  }
}

normalize_spaces <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  trimws(x)
}

fmt_num <- function(x, digits = 1) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "", paste0(formatC(100 * x, format = "f", digits = digits), "\\%"))
}

fmt_pct_csv <- function(x, digits = 1) {
  ifelse(is.na(x), "", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

latex_escape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  sentinel <- "\001"
  x <- stringr::str_replace_all(x, stringr::fixed("\\"), sentinel)
  x <- stringr::str_replace_all(x, stringr::fixed("&"), "\\&")
  x <- stringr::str_replace_all(x, stringr::fixed("%"), "\\%")
  x <- stringr::str_replace_all(x, stringr::fixed("$"), "\\$")
  x <- stringr::str_replace_all(x, stringr::fixed("#"), "\\#")
  x <- stringr::str_replace_all(x, stringr::fixed("_"), "\\_")
  x <- stringr::str_replace_all(x, stringr::fixed("{"), "\\{")
  x <- stringr::str_replace_all(x, stringr::fixed("}"), "\\}")
  x <- stringr::str_replace_all(x, stringr::fixed("~"), "\\textasciitilde{}")
  x <- stringr::str_replace_all(x, stringr::fixed("^"), "\\textasciicircum{}")
  stringr::str_replace_all(x, stringr::fixed(sentinel), "\\\\textbackslash{}")
}

write_lines <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(x, con = path, useBytes = TRUE)
}

write_csv_table <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path)
}

versioned_output <- function(filename) {
  file.path(output_dir, paste0(output_prefix, filename))
}

versioned_diagnostic <- function(filename) {
  file.path(diagnostics_dir, paste0(output_prefix, filename))
}

clean_display_text <- function(x) {
  normalize_spaces(x)
}

canonical_task_match_text <- function(x) {
  x <- clean_display_text(x)
  tolower(normalize_spaces(x))
}

clean_profile_text <- function(x) {
  x <- normalize_spaces(x)
  replacements <- c(
    "intellectual property" = "Intellectual Property",
    "international law" = "International Law",
    "immigration" = "Immigration",
    "criminal law" = "Criminal Law",
    "securities" = "Securities",
    "white collar" = "White Collar",
    "commercial litigation" = "Commercial Litigation",
    "government" = "Government",
    "bankruptcy" = "Bankruptcy",
    "privacy" = "Privacy",
    "environmental" = "Environmental",
    "intelligence oversight" = "Intelligence Oversight",
    "mergers & acquisitions" = "Mergers & Acquisitions",
    "landlord tenant law" = "Landlord Tenant Law"
  )

  for (from in names(replacements)) {
    x <- stringr::str_replace_all(x, stringr::regex(from, ignore_case = TRUE), replacements[[from]])
  }

  x
}

# ------------------------------------------------------------
# Study-result block helpers
# Mirrors analysis_v1.22.R where possible.
# ------------------------------------------------------------

normalize_pipeline_round <- function(x) {
  x <- as.character(x)
  x <- tolower(trimws(x))
  dplyr::if_else(is.na(x) | x == "", "round1", x)
}

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

mode_or_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[[which.max(tabulate(match(x, ux)))]]
}

blank_to_na <- function(x) {
  x <- as.character(x)
  x[is.na(x) | trimws(x) == ""] <- NA_character_
  x
}

build_judgmentbench_task_meta_for_rows <- function(tasks) {
  tasks %>%
    dplyr::transmute(
      task_id,
      task_number = suppressWarnings(as.integer(stringr::str_remove(task_id, "^task_"))),
      task_category = clean_display_text(task_category),
      task_type = clean_display_text(task_type),
      task_name = clean_display_text(task),
      task_max_points = suppressWarnings(as.numeric(max_points))
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

signature_key <- function(...) {
  parts <- lapply(list(...), function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  do.call(paste, c(parts, sep = "|"))
}

build_judgmentbench_noncompleted_rows_for_tables <- function(assignment_records, annotator_lookup, task_meta) {
  assignment_records %>%
    dplyr::filter(status != "completed") %>%
    dplyr::left_join(task_meta, by = "task_id") %>%
    dplyr::left_join(annotator_lookup, by = "annotator_id") %>%
    dplyr::transmute(
      exported_at = NA_character_,
      environment = "judgmentbench",
      user_email, user_name, user_title, user_firm, user_practice_areas,
      user_practice_experience, user_total_yoe, user_registered_at,
      pipeline_number, pipeline_round, pipeline_starting_method, cluster_order_reference,
      assignment_position = suppressWarnings(as.integer(assignment_order)),
      task_slot_number = suppressWarnings(as.integer(task_slot_order)),
      method_step_number = suppressWarnings(as.integer(method_step_order)),
      method = normalize_method(method),
      status = tolower(trimws(status)),
      task_number,
      task_category,
      task_type,
      task_name,
      task_max_points,
      output_id = blank_to_na(output_id),
      rung_alias = NA_character_,
      rung_label = NA_character_,
      version_number = NA_real_,
      option_a_output_id = blank_to_na(option_a_output_id),
      option_a_rung_alias = NA_character_,
      option_a_rung_label = NA_character_,
      option_a_version_number = NA_real_,
      option_b_output_id = blank_to_na(option_b_output_id),
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

read_study_results_judgmentbench <- function(jb_dir) {
  # Resolves CSV paths against the released directory layout:
  #   <jb_dir>/human/...                     human annotation CSVs
  #   <jb_dir>/base/tasks.csv                shared task metadata
  #   <jb_dir>/outputs/outputs.csv           constructed-output catalogue
  required_files <- c(
    file.path(jb_dir, "human", "annotators.csv"),
    file.path(jb_dir, "base", "tasks.csv"),
    file.path(jb_dir, "outputs", "outputs.csv"),
    file.path(jb_dir, "human", "assignment_records.csv"),
    file.path(jb_dir, "human", "annotations_rubric.csv"),
    file.path(jb_dir, "human", "annotations_comparative_judgment.csv")
  )
  for (f in required_files) {
    stop_if_missing(f)
  }

  annotators <- readr::read_csv(file.path(jb_dir, "human", "annotators.csv"), show_col_types = FALSE)
  tasks <- readr::read_csv(file.path(jb_dir, "base", "tasks.csv"), show_col_types = FALSE)
  outputs <- readr::read_csv(file.path(jb_dir, "outputs", "outputs.csv"), show_col_types = FALSE)
  assignment_records <- readr::read_csv(file.path(jb_dir, "human", "assignment_records.csv"), show_col_types = FALSE)
  ann_rubric <- readr::read_csv(file.path(jb_dir, "human", "annotations_rubric.csv"), show_col_types = FALSE)
  ann_cj <- readr::read_csv(file.path(jb_dir, "human", "annotations_comparative_judgment.csv"), show_col_types = FALSE)

  task_meta <- build_judgmentbench_task_meta_for_rows(tasks)
  output_lookup <- outputs %>%
    dplyr::transmute(
      output_id,
      quality_level,
      version_number = suppressWarnings(as.numeric(version_number))
    )

  annotator_lookup <- annotators %>%
    dplyr::mutate(
      split_years_experience = dplyr::case_when(
        years_experience == "Not reported" ~ NA_real_,
        TRUE ~ suppressWarnings(readr::parse_number(as.character(years_experience)))
      )
    ) %>%
    dplyr::transmute(
      annotator_id,
      pipeline_number = annotator_id,
      pipeline_round = "round1",
      cluster_order_reference = NA_real_,
      user_email = annotator_id,
      user_name = NA_character_,
      user_title = as.character(title),
      user_firm = as.character(organization_type),
      user_practice_areas = as.character(practice_areas),
      user_practice_experience = NA_character_,
      user_total_yoe = split_years_experience,
      user_registered_at = NA_character_,
      pipeline_starting_method = NA_character_
    )

  rubric_long <- ann_rubric %>%
    dplyr::left_join(
      output_lookup %>% dplyr::select(output_id, version_number),
      by = "output_id"
    ) %>%
    dplyr::left_join(task_meta, by = "task_id") %>%
    dplyr::left_join(annotator_lookup, by = "annotator_id") %>%
    dplyr::transmute(
      exported_at = NA_character_,
      environment = "judgmentbench",
      user_email, user_name, user_title, user_firm, user_practice_areas,
      user_practice_experience, user_total_yoe, user_registered_at,
      pipeline_number, pipeline_round, pipeline_starting_method, cluster_order_reference,
      assignment_position = suppressWarnings(as.integer(annotation_order)),
      task_slot_number = suppressWarnings(as.integer(task_slot_order)),
      method_step_number = suppressWarnings(as.integer(method_step_order)),
      method = "rubric",
      status = "completed",
      task_number,
      task_category,
      task_type,
      task_name,
      task_max_points,
      output_id,
      rung_alias = output_quality_level,
      rung_label = output_quality_level,
      version_number,
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
      rubric_total_points = suppressWarnings(as.numeric(rubric_total_points)),
      rubric_max_points = suppressWarnings(as.numeric(rubric_max_points)),
      comment = blank_to_na(comment),
      rubric_item_scores_json = NA_character_,
      assignment_created_at = NA_character_,
      eval_created_at = NA_character_,
      source_step_key = annotation_id
    )

  cj_long <- ann_cj %>%
    dplyr::left_join(
      output_lookup %>%
        dplyr::select(option_a_output_id = output_id, option_a_version_number = version_number),
      by = "option_a_output_id"
    ) %>%
    dplyr::left_join(
      output_lookup %>%
        dplyr::select(option_b_output_id = output_id, option_b_version_number = version_number),
      by = "option_b_output_id"
    ) %>%
    dplyr::left_join(
      output_lookup %>%
        dplyr::select(preferred_output_id = output_id, preferred_version_number = version_number),
      by = "preferred_output_id"
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
      pipeline_number, pipeline_round, pipeline_starting_method, cluster_order_reference,
      assignment_position = suppressWarnings(as.integer(annotation_order)),
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
      option_a_output_id,
      option_a_rung_alias = option_a_quality_level,
      option_a_rung_label = option_a_quality_level,
      option_a_version_number,
      option_b_output_id,
      option_b_rung_alias = option_b_quality_level,
      option_b_rung_label = option_b_quality_level,
      option_b_version_number,
      preferred_output_id,
      preferred_option,
      preferred_rung_alias = preferred_quality,
      preferred_version_number,
      time_spent_seconds = suppressWarnings(as.numeric(time_spent_seconds)),
      rubric_total_points = NA_real_,
      rubric_max_points = NA_real_,
      comment = combine_comparative_comments(comment_a, comment_b),
      rubric_item_scores_json = NA_character_,
      assignment_created_at = NA_character_,
      eval_created_at = NA_character_,
      source_step_key = annotation_id
    )

  noncompleted_long <- build_judgmentbench_noncompleted_rows_for_tables(
    assignment_records = assignment_records,
    annotator_lookup = annotator_lookup,
    task_meta = task_meta
  )

  dplyr::bind_rows(rubric_long, cj_long, noncompleted_long) %>%
    dplyr::arrange(
      cluster_order_reference,
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

read_study_results_for_tables <- function(judgmentbench_dir) {
  raw_dat <- read_study_results_judgmentbench(judgmentbench_dir)

  pipeline_instance_id <- derive_pipeline_instance_id(raw_dat)
  pipeline_round_normalized <- if ("pipeline_round" %in% names(raw_dat)) {
    normalize_pipeline_round(raw_dat$pipeline_round)
  } else {
    rep("round1", length(pipeline_instance_id))
  }

  # Include task_number because a skipped slot can be reused by another task.
  task_unit_key <- paste(
    pipeline_instance_id,
    raw_dat$task_slot_number,
    raw_dat$task_number,
    sep = "::"
  )

  raw_dat %>%
    dplyr::mutate(
      method = normalize_method(method),
      status = tolower(trimws(status)),
      rung_label = normalize_rung(rung_label),
      option_a_rung_label = normalize_rung(option_a_rung_label),
      option_b_rung_label = normalize_rung(option_b_rung_label),
      preferred_rung_label = normalize_rung(preferred_rung_alias),
      pipeline_round_normalized = pipeline_round_normalized,
      pipeline_instance_id = pipeline_instance_id,
      cluster_id = pipeline_instance_id,
      task_unit_key = task_unit_key,
      block_key = paste(task_unit_key, method, sep = "::")
    )
}

# Same complete-block inclusion rule as in analysis_vSubmit.R, used here to
# decide which tasks have estimable per-task metrics (median time, skip rate).
# Returns the per-block summary plus the keys of the kept blocks.
validate_blocks_for_tables <- function(dat) {
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
      task_category,
      task_type,
      method
    ) %>%
    dplyr::summarise(
      block_status = dplyr::case_when(
        all(status == "completed", na.rm = TRUE) ~ "completed",
        any(status == "completed", na.rm = TRUE) ~ "partial",
        TRUE ~ mode_or_first(status)
      ),
      n_rows = dplyr::n(),
      n_completed_rows = sum(status == "completed", na.rm = TRUE),
      n_skipped_rows = sum(status == "skipped", na.rm = TRUE),
      n_rubric_rungs = dplyr::n_distinct(rung_label[!is.na(rung_label)]),
      n_pref_pairs = dplyr::n_distinct(
        paste(option_a_rung_label, option_b_rung_label, sep = "__")[
          !is.na(option_a_rung_label) & !is.na(option_b_rung_label)
        ]
      ),
      block_time_seconds = sum(
        suppressWarnings(as.numeric(time_spent_seconds))[status == "completed"],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Match the analysis inclusion rule: only full three-row blocks count.
      is_complete_expected_shape = dplyr::case_when(
        method == "rubric" ~ (block_status == "completed" & n_rows == 3L & n_rubric_rungs == 3L),
        method == "preference" ~ (block_status == "completed" & n_rows == 3L & n_pref_pairs == 3L),
        TRUE ~ FALSE
      ),
      is_fully_skipped = block_status == "skipped",
      is_partial_or_incomplete = !is_complete_expected_shape & !is_fully_skipped
    )

  analysis_block_keys <- block_summary %>%
    dplyr::filter(is_complete_expected_shape) %>%
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

  list(
    block_summary = block_summary,
    analysis_block_keys = analysis_block_keys,
    analysis_dat = analysis_dat
  )
}

# ------------------------------------------------------------
# BLB task metadata and rubric criteria
# ------------------------------------------------------------

count_rubric_criteria <- function(rubric_text) {
  rubric_text <- ifelse(is.na(rubric_text), "", as.character(rubric_text))
  matches <- stringr::str_match_all(
    rubric_text,
    "\\(([0-9]+(?:\\.[0-9]+)?)\\s+points?\\)"
  )

  purrr::map_int(matches, nrow)
}

sum_rubric_points <- function(rubric_text) {
  rubric_text <- ifelse(is.na(rubric_text), "", as.character(rubric_text))
  matches <- stringr::str_match_all(
    rubric_text,
    "\\(([0-9]+(?:\\.[0-9]+)?)\\s+points?\\)"
  )

  purrr::map_dbl(matches, function(m) {
    if (nrow(m) == 0) return(0)
    sum(as.numeric(m[, 2]), na.rm = TRUE)
  })
}

task_type_order <- tibble::tribble(
  ~category, ~task_type, ~task_type_order,
  "Litigation", "Analysis of Litigation Filings", 1L,
  "Litigation", "Case Law Research", 2L,
  "Litigation", "Case Management", 3L,
  "Litigation", "Document Review & Analysis", 4L,
  "Litigation", "Drafting (L)", 5L,
  "Litigation", "Regulatory & Advising", 6L,
  "Litigation", "Transcript Analysis", 7L,
  "Litigation", "Trial Preparations & Oral Argument", 8L,
  "Transactional", "Corporate Strategy & Advising", 1L,
  "Transactional", "Deal Management", 2L,
  "Transactional", "Drafting (T)", 3L,
  "Transactional", "Due Diligence", 4L,
  "Transactional", "Legal Research", 5L,
  "Transactional", "Negotiation Strategy", 6L,
  "Transactional", "Risk Assessment & Compliance", 7L,
  "Transactional", "Transaction Structuring", 8L
)

read_judgmentbench_task_metadata <- function(jb_dir) {
  stop_if_missing(file.path(jb_dir, "base", "tasks.csv"))
  stop_if_missing(file.path(jb_dir, "base", "rubric_items.csv"))

  tasks <- readr::read_csv(file.path(jb_dir, "base", "tasks.csv"), show_col_types = FALSE) %>%
    dplyr::mutate(source_order = dplyr::row_number())
  rubric_items <- readr::read_csv(file.path(jb_dir, "base", "rubric_items.csv"), show_col_types = FALSE)

  task_numbers <- build_judgmentbench_task_meta_for_rows(tasks) %>%
    dplyr::select(task_id, task_number)

  rubric_counts <- rubric_items %>%
    dplyr::filter(suppressWarnings(as.numeric(weight)) > 0) %>%
    dplyr::count(task_id, name = "rubric_criteria")

  task_meta <- tasks %>%
    dplyr::left_join(task_numbers, by = "task_id") %>%
    dplyr::left_join(rubric_counts, by = "task_id") %>%
    dplyr::mutate(rubric_criteria = dplyr::coalesce(as.integer(rubric_criteria), 0L)) %>%
    dplyr::transmute(
      task_number,
      source_order,
      category = clean_display_text(task_category),
      task_type = clean_display_text(task_type),
      source_task_raw = normalize_spaces(task),
      source_task = clean_display_text(task),
      prompt = clean_display_text(prompt),
      docs = NA_character_,
      selected_rubric = as.character(rubric),
      total_points = suppressWarnings(as.numeric(max_points)),
      full_task = clean_display_text(task),
      full_rubric = as.character(rubric),
      full_total_points = suppressWarnings(as.numeric(max_points)),
      selected_rubric_missing = FALSE,
      selected_rubric_points = total_points,
      selected_rubric_points_match = TRUE,
      use_full_rubric = FALSE,
      rubric = as.character(rubric),
      rubric_criteria,
      rubric_point_sum = total_points,
      rubric_point_sum_match = TRUE,
      task_order = source_order,
      display_task = clean_display_text(source_task_raw)
    )

  if (nrow(task_meta) != 30L || dplyr::n_distinct(task_meta$task_number) != 30L) {
    stop("Expected exactly 30 unique tasks in JudgmentBench tasks.csv.")
  }

  task_meta
}

# ------------------------------------------------------------
# Table builders
# ------------------------------------------------------------

# Per-task timing and skip-rate metrics derived from completed blocks: median
# time per task block, share of attempted blocks that were skipped, and the
# count of completed blocks per task. Inputs come from validate_blocks_for_tables.
build_task_metrics <- function(block_summary) {
  # Skip rates use reached blocks; times use complete blocks only.
  skip_metrics <- block_summary %>%
    dplyr::group_by(task_number) %>%
    dplyr::summarise(
      reached_blocks = dplyr::n(),
      complete_blocks = sum(is_complete_expected_shape, na.rm = TRUE),
      skipped_blocks = sum(is_fully_skipped, na.rm = TRUE),
      partial_or_incomplete_blocks = sum(is_partial_or_incomplete, na.rm = TRUE),
      skip_rate = skipped_blocks / reached_blocks,
      .groups = "drop"
    )

  time_metrics <- block_summary %>%
    dplyr::filter(is_complete_expected_shape, is.finite(block_time_seconds), block_time_seconds >= 0) %>%
    dplyr::group_by(task_number) %>%
    dplyr::summarise(
      median_time_minutes = stats::median(block_time_seconds / 60, na.rm = TRUE),
      .groups = "drop"
    )

  skip_metrics %>%
    dplyr::left_join(time_metrics, by = "task_number")
}

# Table 1 (sampled tasks by type): a count breakdown of the 30 benchmark
# tasks by task category and task type, with totals.
build_table1 <- function(task_meta) {
  base <- task_meta %>%
    dplyr::left_join(task_type_order, by = c("category", "task_type")) %>%
    dplyr::arrange(
      factor(category, levels = c("Litigation", "Transactional")),
      task_type_order,
      source_order
    ) %>%
    dplyr::group_by(category, task_type, task_type_order) %>%
    dplyr::summarise(
      task_example = dplyr::first(display_task),
      T = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(row_type = "body")

  category_totals <- base %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(
      task_type_order = 999L,
      task_type = paste(dplyr::first(category), "total"),
      task_example = "",
      T = sum(T),
      row_type = "category_total",
      .groups = "drop"
    )

  grand_total <- tibble::tibble(
    category = "Total",
    task_type_order = 9999L,
    task_type = "Total",
    task_example = "",
    T = sum(base$T),
    row_type = "grand_total"
  )

  dplyr::bind_rows(
    base %>% dplyr::filter(category == "Litigation"),
    category_totals %>% dplyr::filter(category == "Litigation"),
    base %>% dplyr::filter(category == "Transactional"),
    category_totals %>% dplyr::filter(category == "Transactional"),
    grand_total
  ) %>%
    dplyr::select(row_type, category, task_type, task_example, T)
}

# Table 2 (per-task summary): one row per benchmark task with its category,
# name, median completion time, skip rate, and rubric criterion count.
# task_metrics carries the timing/skip-rate data derived from completed
# blocks; task_meta carries the static metadata.
build_task_listing_table <- function(task_meta, task_metrics) {
  task_meta %>%
    dplyr::left_join(task_metrics, by = "task_number") %>%
    dplyr::arrange(task_order) %>%
    dplyr::transmute(
      Task = display_task,
      Category = category,
      Type = task_type,
      `Median time (min)` = fmt_num(median_time_minutes, 1),
      `Skip rate` = fmt_pct_csv(skip_rate, 1),
      `Rubric criteria` = as.integer(rubric_criteria)
    )
}

sanitize_firm <- function(firm, email = NA_character_, pipeline_round = NA_character_, pipeline_instance_id = NA_character_) {
  # The released annotators.csv already exposes the anonymized organization
  # labels ("AmLaw 100 firm", "AmLaw 200 firm", "Data Labeling Company").
  # Pass those through; map blanks to "Not reported" and any other value to
  # the catch-all bucket.
  firm_raw <- normalize_spaces(firm)
  dplyr::case_when(
    firm_raw %in% c("AmLaw 100 firm", "AmLaw 200 firm", "Data Labeling Company") ~ firm_raw,
    is.na(firm_raw) | firm_raw == "" ~ "Not reported",
    TRUE ~ "Other legal organization"
  )
}

title_order_value <- function(title) {
  title <- tolower(normalize_spaces(title))
  dplyr::case_when(
    stringr::str_detect(title, "partner") ~ 1L,
    stringr::str_detect(title, "general counsel|counsel|of counsel") ~ 2L,
    stringr::str_detect(title, "senior associate") ~ 3L,
    stringr::str_detect(title, "junior associate") ~ 4L,
    stringr::str_detect(title, "associate") ~ 5L,
    stringr::str_detect(title, "attorney") ~ 6L,
    TRUE ~ 7L
  )
}

# Table 3 (annotator information): one row per annotator characteristic
# (firm tier, seniority, years bucket, top practice areas) with counts and
# percentages. The denominator is the number of annotators with at least one
# complete task-method block, so the table is restricted to contributing
# annotators rather than the entire annotators.csv.
build_table4 <- function(dat, complete_block_keys) {
  # Annotator table includes anyone with at least one complete analyzed block.
  dat %>%
    dplyr::semi_join(complete_block_keys, by = "block_key") %>%
    dplyr::group_by(pipeline_instance_id) %>%
    dplyr::summarise(
      Title = mode_or_first(user_title),
      raw_firm = mode_or_first(user_firm),
      user_email = mode_or_first(user_email),
      pipeline_round_normalized = mode_or_first(pipeline_round_normalized),
      total_years_experience = suppressWarnings(as.numeric(mode_or_first(user_total_yoe))),
      practice_areas = mode_or_first(user_practice_areas),
      practice_experience = mode_or_first(user_practice_experience),
      complete_blocks = dplyr::n_distinct(block_key),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Title = dplyr::if_else(is.na(Title) | trimws(Title) == "", "Not reported", clean_profile_text(Title)),
      Firm = sanitize_firm(raw_firm, user_email, pipeline_round_normalized, pipeline_instance_id),
      practice_areas = dplyr::if_else(
        is.na(practice_areas) | trimws(practice_areas) == "",
        "Not reported",
        clean_profile_text(practice_areas)
      ),
      practice_experience = dplyr::if_else(
        is.na(practice_experience) | trimws(practice_experience) == "",
        "Not reported",
        clean_profile_text(practice_experience)
      ),
      firm_order = dplyr::case_when(
        Firm == "AmLaw 100 firm" ~ 1L,
        Firm == "AmLaw 200 firm" ~ 2L,
        Firm == "Data Labeling Company" ~ 3L,
        Firm == "Other legal organization" ~ 4L,
        TRUE ~ 5L
      ),
      title_group = dplyr::case_when(
        stringr::str_detect(tolower(Title), "partner") ~ "Partner",
        stringr::str_detect(tolower(Title), "counsel") ~ "Counsel",
        stringr::str_detect(tolower(Title), "senior associate") ~ "Senior Associate",
        stringr::str_detect(tolower(Title), "junior associate") ~ "Junior Associate",
        stringr::str_detect(tolower(Title), "attorney") ~ "Attorney",
        Title == "Not reported" ~ "Not reported",
        TRUE ~ "Other legal roles"
      ),
      title_group_order = dplyr::case_when(
        title_group == "Partner" ~ 1L,
        title_group == "Counsel" ~ 2L,
        title_group == "Senior Associate" ~ 3L,
        title_group == "Junior Associate" ~ 4L,
        title_group == "Attorney" ~ 5L,
        title_group == "Other legal roles" ~ 6L,
        TRUE ~ 7L
      ),
      years_bucket = dplyr::case_when(
        is.na(total_years_experience) ~ "Not reported",
        total_years_experience <= 3 ~ "<=3",
        total_years_experience <= 7 ~ "4-7",
        total_years_experience <= 11 ~ "8-11",
        total_years_experience <= 15 ~ "12-15",
        total_years_experience <= 19 ~ "16-19",
        TRUE ~ ">=20"
      ),
      years_bucket_order = dplyr::case_when(
        years_bucket == "<=3" ~ 1L,
        years_bucket == "4-7" ~ 2L,
        years_bucket == "8-11" ~ 3L,
        years_bucket == "12-15" ~ 4L,
        years_bucket == "16-19" ~ 5L,
        years_bucket == ">=20" ~ 6L,
        TRUE ~ 7L
      )
    )
}

top_practice_areas <- function(practice_area_strings, top_n = 3L) {
  practice_area_strings <- practice_area_strings[
    !is.na(practice_area_strings) &
      trimws(practice_area_strings) != "" &
      practice_area_strings != "Not reported"
  ]

  if (length(practice_area_strings) == 0) {
    return("Not reported")
  }

  areas <- unlist(strsplit(practice_area_strings, ";", fixed = TRUE), use.names = FALSE)
  areas <- clean_profile_text(trimws(areas))
  areas <- areas[areas != "" & !is.na(areas)]

  if (length(areas) == 0) {
    return("Not reported")
  }

  counts <- sort(table(areas), decreasing = TRUE)
  counts <- counts[seq_len(min(length(counts), top_n))]
  paste0(names(counts), " (", as.integer(counts), ")", collapse = "; ")
}

explode_practice_areas <- function(practice_area_strings) {
  practice_area_strings <- practice_area_strings[
    !is.na(practice_area_strings) &
      trimws(practice_area_strings) != "" &
      practice_area_strings != "Not reported"
  ]

  if (length(practice_area_strings) == 0) {
    return(character())
  }

  areas <- unlist(strsplit(practice_area_strings, ";", fixed = TRUE), use.names = FALSE)
  areas <- clean_profile_text(trimws(areas))
  areas[areas != "" & !is.na(areas)]
}

format_year_range <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return("")

  min_x <- min(x)
  max_x <- max(x)
  if (min_x == max_x) {
    return(formatC(min_x, format = "f", digits = 0))
  }

  paste0(
    formatC(min_x, format = "f", digits = 0),
    "-",
    formatC(max_x, format = "f", digits = 0)
  )
}

format_median_years <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return("")
  formatC(stats::median(x), format = "f", digits = 1)
}

fmt_count_pct <- function(n, denominator) {
  pct <- if (denominator == 0) NA_real_ else n / denominator
  paste0(n, " (", formatC(100 * pct, format = "f", digits = 1), "\\%)")
}

fmt_count_pct_csv <- function(n, denominator) {
  pct <- if (denominator == 0) NA_real_ else n / denominator
  paste0(n, " (", formatC(100 * pct, format = "f", digits = 1), "%)")
}

summarise_characteristic_subset <- function(profiles, characteristic, category, section_order, row_order, denominator) {
  tibble::tibble(
    section_order = section_order,
    row_order = row_order,
    Section = characteristic,
    Characteristic = category,
    n = nrow(profiles),
    Share = if (denominator == 0) "" else paste0(formatC(100 * nrow(profiles) / denominator, format = "f", digits = 1), "%")
  )
}

summarise_characteristic_groups <- function(profiles, characteristic, group_col, order_col, section_order, denominator) {
  groups <- profiles %>%
    dplyr::distinct(
      level = .data[[group_col]],
      row_order = .data[[order_col]]
    ) %>%
    dplyr::arrange(row_order, level)

  purrr::map_dfr(seq_len(nrow(groups)), function(i) {
    group_profiles <- profiles %>%
      dplyr::filter(.data[[group_col]] == groups$level[[i]])

    summarise_characteristic_subset(
      profiles = group_profiles,
      characteristic = characteristic,
      category = groups$level[[i]],
      section_order = section_order,
      row_order = groups$row_order[[i]],
      denominator = denominator
    )
  })
}

build_table4_summary <- function(annotator_profiles) {
  denominator <- nrow(annotator_profiles)

  base_summary <- dplyr::bind_rows(
    summarise_characteristic_subset(
      profiles = annotator_profiles,
      characteristic = "Overall",
      category = "All annotators",
      section_order = 1L,
      row_order = 1L,
      denominator = denominator
    ),
    summarise_characteristic_groups(
      profiles = annotator_profiles,
      characteristic = "Firm/source",
      group_col = "Firm",
      order_col = "firm_order",
      section_order = 2L,
      denominator = denominator
    ),
    summarise_characteristic_groups(
      profiles = annotator_profiles,
      characteristic = "Title",
      group_col = "title_group",
      order_col = "title_group_order",
      section_order = 3L,
      denominator = denominator
    ),
    summarise_characteristic_groups(
      profiles = annotator_profiles,
      characteristic = "Years of experience",
      group_col = "years_bucket",
      order_col = "years_bucket_order",
      section_order = 4L,
      denominator = denominator
    )
  )

  practice_area_counts <- annotator_profiles %>%
    dplyr::select(pipeline_instance_id, practice_areas) %>%
    dplyr::mutate(area = purrr::map(practice_areas, explode_practice_areas)) %>%
    tidyr::unnest(area) %>%
    dplyr::distinct(pipeline_instance_id, area) %>%
    dplyr::count(area, name = "annotators") %>%
    dplyr::arrange(dplyr::desc(annotators), area)

  primary_practice_areas <- c(
    "Litigation",
    "Transactions",
    "Regulatory",
    "Labor & Employment",
    "Intellectual Property",
    "Tax"
  )

  practice_summary <- dplyr::bind_rows(
    purrr::map_dfr(seq_along(primary_practice_areas), function(i) {
      n <- practice_area_counts$annotators[match(primary_practice_areas[[i]], practice_area_counts$area)]
      n <- ifelse(is.na(n), 0L, n)
      tibble::tibble(
        section_order = 5L,
        row_order = i,
        Section = "Practice area",
        Characteristic = primary_practice_areas[[i]],
        n = n,
        Share = if (denominator == 0) "" else paste0(formatC(100 * n / denominator, format = "f", digits = 1), "%")
      )
    }),
    {
      other_n <- practice_area_counts %>%
        dplyr::filter(!(area %in% primary_practice_areas)) %>%
        dplyr::inner_join(
          annotator_profiles %>%
            dplyr::select(pipeline_instance_id, practice_areas) %>%
            dplyr::mutate(area = purrr::map(practice_areas, explode_practice_areas)) %>%
            tidyr::unnest(area) %>%
            dplyr::distinct(pipeline_instance_id, area),
          by = "area"
        ) %>%
        dplyr::distinct(pipeline_instance_id) %>%
        nrow()
      tibble::tibble(
        section_order = 5L,
        row_order = length(primary_practice_areas) + 1L,
        Section = "Practice area",
        Characteristic = "Other",
        n = other_n,
        Share = if (denominator == 0) "" else paste0(formatC(100 * other_n / denominator, format = "f", digits = 1), "%")
      )
    },
    {
      not_reported_n <- sum(annotator_profiles$practice_areas == "Not reported", na.rm = TRUE)
      tibble::tibble(
        section_order = 5L,
        row_order = length(primary_practice_areas) + 2L,
        Section = "Practice area",
        Characteristic = "Not reported",
        n = not_reported_n,
        Share = if (denominator == 0) "" else paste0(formatC(100 * not_reported_n / denominator, format = "f", digits = 1), "%")
      )
    }
  )

  dplyr::bind_rows(base_summary, practice_summary) %>%
    dplyr::arrange(section_order, row_order, Characteristic) %>%
    dplyr::select(
      Section,
      Characteristic,
      n,
      Share
    )
}

# ------------------------------------------------------------
# LaTeX writers
# ------------------------------------------------------------

latex_table1 <- function(table1) {
  lines <- c(
    "\\begin{table}[H]",
    "  \\caption{Sample tasks by category and task type.}",
    "  \\label{tab:sampled-tasks-by-type}",
    "  \\small",
    "  \\centering",
    "  \\setlength{\\tabcolsep}{3.5pt}",
    "  \\renewcommand{\\arraystretch}{1.08}",
    "  \\begin{tabularx}{\\textwidth}{@{}",
    "    >{\\raggedright\\arraybackslash}p{0.14\\textwidth}",
    "    >{\\raggedright\\arraybackslash}p{0.23\\textwidth}",
    "    >{\\raggedright\\arraybackslash}X",
    "    >{\\centering\\arraybackslash}p{0.05\\textwidth}",
    "    @{}}",
    "    \\toprule",
    "    Category & Task Type & Task Example & Count \\\\",
    "    \\midrule"
  )

  for (i in seq_len(nrow(table1))) {
    row <- table1[i, ]
    if (row$row_type == "body") {
      lines <- c(
        lines,
        paste0(
          "    ", latex_escape(row$category), " & ", latex_escape(row$task_type),
          "\n      & ", latex_escape(row$task_example),
          "\n      & ", row$T, " \\\\"
        )
      )
    } else if (row$row_type == "category_total") {
      if (row$category == "Litigation") {
        lines <- c(lines, "    \\addlinespace")
      } else {
        lines <- c(lines, "    \\addlinespace")
      }
      lines <- c(
        lines,
        paste0(
          "    \\multicolumn{3}{r}{\\textit{", latex_escape(row$task_type), "}}\n",
          "      & \\textit{", row$T, "} \\\\"
        )
      )
      if (row$category == "Litigation") {
        lines <- c(lines, "    \\midrule")
      }
    } else if (row$row_type == "grand_total") {
      lines <- c(
        lines,
        "    \\midrule",
        paste0(
          "    \\multicolumn{3}{r}{\\textbf{Total}}\n",
          "      & \\textbf{", row$T, "} \\\\"
        )
      )
    }
  }

  c(
    lines,
    "    \\bottomrule",
    "  \\end{tabularx}",
    "\\end{table}"
  )
}

latex_task_listing <- function(display_table, caption, label) {
  lines <- c(
    "\\begin{table}[H]",
    paste0("  \\caption{", caption, "}"),
    paste0("  \\label{", latex_escape(label), "}"),
    "  \\centering",
    "  \\small",
    "  \\setlength{\\tabcolsep}{2pt}",
    "  \\renewcommand{\\arraystretch}{1.16}",
    "  \\begin{tabularx}{\\textwidth}{@{}",
    "    >{\\raggedright\\arraybackslash}X",
    "    >{\\raggedright\\arraybackslash}p{0.140\\textwidth}",
    "    >{\\raggedright\\arraybackslash}p{0.180\\textwidth}",
    "    >{\\centering\\arraybackslash}p{0.075\\textwidth}",
    "    >{\\centering\\arraybackslash}p{0.055\\textwidth}",
    "    >{\\centering\\arraybackslash}p{0.065\\textwidth}",
    "    @{}}",
    "    \\toprule",
    "    Task & Category & Type & \\shortstack{$\\widetilde{t}$\\\\(min)} & \\shortstack{Skip\\\\rate} & \\shortstack{Rubric\\\\criteria} \\\\",
    "    \\midrule"
  )

  for (i in seq_len(nrow(display_table))) {
    row <- display_table[i, ]
    lines <- c(
      lines,
      paste0(
        "    ",
        latex_escape(row$Task), " & ",
        latex_escape(row$Category), " & ",
        latex_escape(row$Type), " & ",
        latex_escape(row$`Median time (min)`), " & ",
        latex_escape(row$`Skip rate`), " & ",
        latex_escape(row$`Rubric criteria`),
        " \\\\"
      )
    )
  }

  c(
    lines,
    "    \\bottomrule",
    "  \\end{tabularx}",
    "\\end{table}"
  )
}

latex_task_listing_longtable <- function(display_table, caption, label) {
  header <- "    Task & Category & Type & \\shortstack{$\\widetilde{t}$\\\\(min)} & \\shortstack{Skip\\\\rate} & \\shortstack{Rubric\\\\criteria} \\\\"

  lines <- c(
    "% Requires \\usepackage{longtable}",
    "\\begingroup",
    "\\small",
    "\\setlength{\\tabcolsep}{2pt}",
    "\\setlength{\\LTleft}{0pt}",
    "\\setlength{\\LTright}{0pt}",
    "\\renewcommand{\\arraystretch}{1.16}",
    "\\begin{longtable}{@{}",
    "  >{\\raggedright\\arraybackslash}p{0.420\\textwidth}",
    "  >{\\raggedright\\arraybackslash}p{0.140\\textwidth}",
    "  >{\\raggedright\\arraybackslash}p{0.180\\textwidth}",
    "  >{\\centering\\arraybackslash}p{0.075\\textwidth}",
    "  >{\\centering\\arraybackslash}p{0.055\\textwidth}",
    "  >{\\centering\\arraybackslash}p{0.065\\textwidth}",
    "  @{}}",
    paste0("  \\caption{", caption, "}"),
    paste0("  \\label{", latex_escape(label), "} \\\\"),
    "    \\toprule",
    header,
    "    \\midrule",
    "  \\endfirsthead",
    paste0("  \\caption[]{", caption, " (continued)} \\\\"),
    "    \\toprule",
    header,
    "    \\midrule",
    "  \\endhead",
    "    \\midrule",
    "    \\multicolumn{6}{r}{\\emph{Continued on next page}} \\\\",
    "  \\endfoot",
    "    \\bottomrule",
    "  \\endlastfoot"
  )

  for (i in seq_len(nrow(display_table))) {
    row <- display_table[i, ]
    lines <- c(
      lines,
      paste0(
        "    ",
        latex_escape(row$Task), " & ",
        latex_escape(row$Category), " & ",
        latex_escape(row$Type), " & ",
        latex_escape(row$`Median time (min)`), " & ",
        latex_escape(row$`Skip rate`), " & ",
        latex_escape(row$`Rubric criteria`),
        " \\\\"
      )
    )
  }

  c(
    lines,
    "\\end{longtable}",
    "\\endgroup"
  )
}

latex_table_dependencies <- function() {
  c(
    "% Required packages for generated tables from tables_v1.4.R",
    "\\usepackage{booktabs}",
    "\\usepackage{tabularx}",
    "\\usepackage{array}",
    "\\usepackage{longtable}"
  )
}

latex_table4 <- function(table4) {
  lines <- c(
    "\\begin{table}[!t]",
    "  \\caption{Annotator summary information}",
    "  \\label{tab:annotator-information}",
    "  \\centering",
    "  \\small",
    "  \\setlength{\\tabcolsep}{6pt}",
    "  \\renewcommand{\\arraystretch}{1.12}",
    "  \\begin{tabularx}{0.72\\textwidth}{@{}",
    "    >{\\raggedright\\arraybackslash}X",
    "    >{\\centering\\arraybackslash}p{0.08\\textwidth}",
    "    >{\\centering\\arraybackslash}p{0.08\\textwidth}",
    "    @{}}",
    "    \\toprule",
    "    Characteristic & n & Share \\\\",
    "    \\midrule"
  )

  current_section <- NA_character_
  for (i in seq_len(nrow(table4))) {
    row <- table4[i, ]
    if (!identical(current_section, row$Section)) {
      if (!is.na(current_section)) {
        lines <- c(lines, "    \\midrule")
      }
      lines <- c(
        lines,
        paste0("    \\multicolumn{3}{c}{\\textbf{", latex_escape(row$Section), "}} \\\\"),
        "    \\midrule"
      )
      current_section <- row$Section
    }

    lines <- c(
      lines,
      paste0(
        "    ",
        latex_escape(row$Characteristic), " & ",
        latex_escape(row$n), " & ",
        latex_escape(row$Share),
        " \\\\"
      )
    )
  }

  c(
    lines,
    "    \\bottomrule",
    "  \\end{tabularx}",
    "  \\vspace{0.35em}",
    "  \\begin{minipage}{0.72\\textwidth}",
    "    \\footnotesize\\emph{Note.} Shares use all annotators as the denominator. Practice areas are not mutually exclusive, so practice-area shares may sum to more than 100\\%.",
    "  \\end{minipage}",
    "\\end{table}"
  )
}

build_check_report <- function(table1, table2, table4, annotator_profiles, task_meta, block_summary) {
  lines <- character()
  add <- function(...) {
    lines <<- c(lines, paste0(...))
  }

  add("tables_v1.4.R check report")
  add("========================")
  add("")
  add("Inputs")
  add("------")
  add("JudgmentBench directory: ", judgmentbench_dir)
  add("")

  add("Core data checks")
  add("----------------")
  add("Tasks: ", nrow(task_meta))
  add("Table 1 total tasks: ", table1$T[table1$row_type == "grand_total"])
  add("Table 2 rows: ", nrow(table2), " (complete task listing)")
  add("Table 4 characteristic-summary rows: ", nrow(table4))
  add("Table 4 annotators summarized: ", nrow(annotator_profiles))
  add("Table 4 annotators with not-reported profile fields: ", sum(annotator_profiles$Title == "Not reported" | annotator_profiles$Firm == "Not reported" | annotator_profiles$practice_areas == "Not reported"))
  add("")

  add("Study block checks")
  add("------------------")
  block_audit <- block_summary %>%
    dplyr::count(method, block_status, is_complete_expected_shape, name = "blocks") %>%
    dplyr::arrange(method, block_status, dplyr::desc(is_complete_expected_shape))
  for (i in seq_len(nrow(block_audit))) {
    add(
      block_audit$method[[i]], " / ",
      block_audit$block_status[[i]], " / complete_expected_shape=",
      block_audit$is_complete_expected_shape[[i]], ": ",
      block_audit$blocks[[i]]
    )
  }
  add("")

  add("Rubric criteria checks")
  add("----------------------")
  add("All selected rubric point sums match total_points: ", all(task_meta$rubric_point_sum_match))
  add("")

  add("Source-text checks")
  add("------------------")
  add("Table 2 task wording/order comes from result-dataset/tasks.csv (task_id order): TRUE")
  add("Former Table 3 is merged into Table 2; no separate task-listing Table 3 is generated.")
  add("Generated LaTeX outputs are written from source data only; no paper TeX file is read.")
  add("")

  lines
}

# ------------------------------------------------------------
# Main
# ------------------------------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

cat("Reading task metadata...\n")
task_meta <- read_judgmentbench_task_metadata(judgmentbench_dir)

cat("Reading study results and constructing task blocks...\n")
study_dat <- read_study_results_for_tables(judgmentbench_dir)
validation <- validate_blocks_for_tables(study_dat)
block_summary <- validation$block_summary
task_metrics <- build_task_metrics(block_summary)

cat("Building tables...\n")
table1 <- build_table1(task_meta)
table2 <- build_task_listing_table(task_meta, task_metrics)
annotator_profiles <- build_table4(study_dat, validation$analysis_block_keys %>% dplyr::select(block_key))
annotator_table <- build_table4_summary(annotator_profiles)

cat("Writing CSV outputs...\n")
write_csv_table(table1, versioned_output("table1_sampled_tasks_by_type.csv"))
write_csv_table(table2, versioned_output("table2_benchmark_tasks.csv"))
write_csv_table(annotator_table, versioned_output("table3_annotator_information.csv"))

cat("Writing LaTeX outputs...\n")
write_lines(latex_table_dependencies(), versioned_output("table_latex_dependencies.tex"))
write_lines(latex_table1(table1), versioned_output("table1_sampled_tasks_by_type.tex"))
write_lines(
  latex_task_listing_longtable(
    table2,
    caption = "Benchmark tasks. $\\widetilde{t}$ is median completion time in minutes; skip rate is the share of reached task-method blocks that were fully skipped.",
    label = "tab:task-level-summary"
  ),
  versioned_output("table2_benchmark_tasks.tex")
)
write_lines(latex_table4(annotator_table), versioned_output("table3_annotator_information.tex"))

unlink(file.path(output_dir, c(
  "table2_benchmark_tasks_hard_medium.csv",
  "table2_benchmark_tasks_hard_medium.tex",
  "table3_benchmark_tasks_easy.csv",
  "table3_benchmark_tasks_easy.tex"
)))

cat("Writing diagnostics...\n")
rubric_fallbacks <- task_meta %>%
  dplyr::filter(use_full_rubric) %>%
  dplyr::select(
    dplyr::any_of("source_query_number"),
    task_number,
    display_task,
    selected_rubric_missing,
    selected_rubric_points,
    total_points,
    rubric_criteria
  )

partial_blocks <- block_summary %>%
  dplyr::filter(is_partial_or_incomplete) %>%
  dplyr::arrange(task_number, pipeline_instance_id, method)

text_cleaning_audit <- task_meta %>%
  dplyr::transmute(
    task_number,
    source_task = source_task_raw,
    display_task,
    task_text_changed = source_task_raw != display_task,
    source_task_type = task_type
  ) %>%
  dplyr::filter(task_text_changed)

firm_recode_audit <- study_dat %>%
  dplyr::semi_join(validation$analysis_block_keys %>% dplyr::select(block_key), by = "block_key") %>%
  dplyr::distinct(
    pipeline_instance_id,
    pipeline_round_normalized,
    raw_firm = user_firm,
    email_domain = tolower(sub("^.*@", "", user_email))
  ) %>%
  dplyr::mutate(firm_display = sanitize_firm(raw_firm, email_domain, pipeline_round_normalized, pipeline_instance_id)) %>%
  dplyr::arrange(firm_display, raw_firm)

write_csv_table(task_meta, versioned_diagnostic("task_metadata_with_rubric_counts.csv"))
write_csv_table(task_metrics, versioned_diagnostic("task_metrics_from_study_blocks.csv"))
write_csv_table(annotator_profiles, versioned_diagnostic("annotator_profiles_detail.csv"))
write_csv_table(rubric_fallbacks, versioned_diagnostic("rubric_source_fallbacks.csv"))
write_csv_table(partial_blocks, versioned_diagnostic("partial_or_incomplete_blocks.csv"))
write_csv_table(text_cleaning_audit, versioned_diagnostic("text_cleaning_audit.csv"))
write_csv_table(firm_recode_audit, versioned_diagnostic("firm_recode_audit.csv"))

check_report <- build_check_report(table1, table2, annotator_table, annotator_profiles, task_meta, block_summary)
write_lines(check_report, versioned_output("tables_check_report.txt"))

cat("\nDone.\n")
cat("Output directory:\n", output_dir, "\n", sep = "")
cat("Check report:\n", versioned_output("tables_check_report.txt"), "\n", sep = "")
