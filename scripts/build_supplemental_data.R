# ============================================================
# Build additive public files from frozen internal exports
# ============================================================
# Does not publish. Only the selected public columns are written.
# Rscript scripts/build_supplemental_data.R --original DIR --followup DIR \
#   --mixed-raw DIR --mixed-outputs FILE --destination DIR \
#   --generation-history FILE [--generation-history FILE ...]

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(jsonlite)
  library(digest)
})
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
root <- dirname(dirname(normalizePath(gsub("~+~", " ", script, fixed = TRUE))))
source(file.path(root, "analysis", "generation_protocol.R"))

read_rows <- function(path, delim = ",") {
  # A few internal comments contain NULs; comments are not released.
  raw <- readBin(path, "raw", n = file.info(path)$size)
  read_delim(I(rawToChar(raw[raw != as.raw(0)])), delim = delim,
             col_types = cols(.default = col_character()), trim_ws = FALSE,
             na = character(), show_col_types = FALSE)
}
write_rows <- function(path, rows) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_csv(bind_rows(rows), path, na = "")
}

build_followup <- function(source_dir, original, dest, histories) {
  comparisons <- read_rows(file.path(source_dir, "matched-comparisons.csv")) %>% arrange(original_annotation_id)
  texts <- read_rows(file.path(source_dir, "outputs.csv"))
  old <- read_rows(file.path(original, "outputs/outputs.csv"))
  historical <- read_rows(file.path(original, "human/annotations_comparative_judgment.csv"))
  stopifnot(nrow(comparisons) == 300L, !anyDuplicated(comparisons$original_annotation_id))
  history <- new.env(parent = emptyenv())
  for (path in histories) {
    for (h in fromJSON(path, simplifyVector = FALSE)) {
      key <- paste(h$generation_id, h$attempt, sep = ":")
      if (exists(key, history, inherits = FALSE)) stopifnot(identical(history[[key]], h))
      history[[key]] <- h
    }
  }
  mapping <- character()
  public_rows <- public_outputs <- attempts <- list()
  for (i in seq_len(nrow(comparisons))) {
    r <- as.list(comparisons[i, ])
    h <- historical[match(r$original_annotation_id, historical$annotation_id), ]
    stopifnot(h$annotator_id == r$annotator_id, h$task_id == r$task_id)
    new_id <- sprintf("length_output_%06d", i)
    for (q in c("E", "lower")) {
      platform_id <- r[[paste0("old_", q, "_platform_output_id")]]
      public_id <- r[[paste0("old_", q, "_dataset_output_id")]]
      stopifnot(texts$output_text[match(platform_id, texts$output_id)] == old$output_text[match(public_id, old$output_id)])
      if (platform_id %in% names(mapping)) stopifnot(mapping[[platform_id]] == public_id)
      mapping[platform_id] <- public_id
    }
    platform_new <- r$new_lower_platform_output_id
    stopifnot(!platform_new %in% names(mapping))
    mapping[platform_new] <- new_id
    text <- texts$output_text[match(platform_new, texts$output_id)]
    wc <- word_count(text)
    e <- as.integer(r$old_E_words); lower <- as.integer(r$old_lower_words); target <- as.integer(r$target_words)
    stopifnot(wc == as.integer(r$new_lower_words), target == 2 * e - lower,
              e > lower, accepted(text, target, e),
              as.integer(r$human_E_win_before) == as.integer(h$preferred_output_id == r$old_E_dataset_output_id),
              as.integer(r$human_E_win_after) == as.integer(r$preferred_output_id == r$old_E_platform_output_id))
    public_outputs[[i]] <- tibble(output_id = new_id, task_id = r$task_id,
      quality_level = if (r$pair_type == "EI") "intermediate" else "good",
      word_count = wc, sha256 = digest(text, algo = "sha256", serialize = FALSE), output_text = text)
    requested <- target
    for (attempt in seq_len(as.integer(r$generation_attempts))) {
      h <- history[[paste(r$generation_id, attempt, sep = ":")]]
      stopifnot(!is.null(h), requested == h$requested_words, h$desired_words == target,
                word_count(h$output_text) == h$words,
                accepted(h$output_text, target, e, h$complete) == h$qualified,
                h$qualified == (attempt == as.integer(r$generation_attempts)))
      if (h$qualified) stopifnot(h$output_text == text)
      attempts[[length(attempts) + 1L]] <- tibble(output_id = new_id, attempt = attempt,
        desired_words = target, requested_words = requested, actual_words = h$words,
        complete = as.integer(h$complete), accepted = as.integer(h$qualified))
      requested <- next_requested(target, requested, h$words, h$complete)
    }
    batch <- c("length-inversion-2026-10" = "initial",
               "length-inversion-2026-10-add29" = "additional_returners",
               "length-inversion-2026-10-expand119" = "expanded_assignments")
    stopifnot(r$round %in% names(batch))
    public_rows[[i]] <- tibble(annotation_id = sprintf("length_annotation_%06d", i),
      original_annotation_id = r$original_annotation_id, annotator_id = r$annotator_id,
      task_id = r$task_id, pair_type = r$pair_type, collection_batch = unname(batch[r$round]),
      old_E_output_id = r$old_E_dataset_output_id, old_lower_output_id = r$old_lower_dataset_output_id,
      new_lower_output_id = new_id, option_a_output_id = unname(mapping[r$option_a_output_id]),
      option_b_output_id = unname(mapping[r$option_b_output_id]), preferred_output_id = unname(mapping[r$preferred_output_id]),
      preferred_option = if (r$preferred_output_id == r$option_a_output_id) "A" else "B",
      human_E_win_before = as.integer(r$human_E_win_before), human_E_win_after = as.integer(r$human_E_win_after),
      old_E_words = e, old_lower_words = lower, target_words = target, new_lower_words = wc,
      generation_attempts = as.integer(r$generation_attempts), retries = as.integer(r$retries),
      time_spent_seconds = r$time_spent_seconds)
  }
  folder <- file.path(dest, "followup/length_inversion")
  write_rows(file.path(folder, "annotations.csv"), public_rows)
  write_rows(file.path(folder, "outputs.csv"), public_outputs)
  write_rows(file.path(folder, "generation_attempts.csv"), attempts)
}

build_mixed <- function(source_dir, outputs_source, task_ids, dest) {
  pref <- read_rows(file.path(source_dir, "preference_evals.csv"))
  rubric <- read_rows(file.path(source_dir, "rubric_evals.csv"))
  stopifnot(nrow(pref) == 1500L, nrow(rubric) == 3000L)
  ids <- sort(unique(rubric$output_id))
  mapping <- setNames(sprintf("mixed_output_%06d", seq_along(ids)), ids)
  raw <- read_rows(outputs_source, delim = "\t")
  raw_id <- sprintf("t%03d-%s-v%02d", as.integer(raw$task_number), raw$rung_id, as.integer(raw$version))
  stopifnot(all(ids %in% raw_id))
  raw <- raw[match(ids, raw_id), ]
  text_col <- intersect(c("output_text", "response", "text"), names(raw))[1]
  stopifnot(!is.na(text_col), all(pref$status == "completed"), all(rubric$status == "completed"),
            all(pref$preferred_result %in% c("A", "B")),
            all(c(raw$task_number, pref$task_number, rubric$task_number) %in% names(task_ids)))
  outputs <- tibble(output_id = unname(mapping[ids]), task_id = unname(task_ids[raw$task_number]),
                    profile = raw$profile, model = raw$model, output_text = raw[[text_col]])
  pr <- tibble(annotation_id = sprintf("mixed_preference_%06d", seq_len(nrow(pref))),
    task_id = unname(task_ids[pref$task_number]), option_a_output_id = unname(mapping[pref$output_a_id]),
    option_b_output_id = unname(mapping[pref$output_b_id]), option_a_role = pref$output_a_rung_role,
    option_b_role = pref$output_b_rung_role, preferred_option = pref$preferred_result, model = pref$model)
  rr <- tibble(annotation_id = sprintf("mixed_rubric_%06d", seq_len(nrow(rubric))),
    task_id = unname(task_ids[rubric$task_number]), output_id = unname(mapping[rubric$output_id]),
    profile_role = rubric$rung_role, rubric_total_points = rubric$rubric_points_awarded,
    rubric_max_points = rubric$rubric_points_max, model = rubric$model)
  stopifnot(!anyNA(pr), !anyNA(rr))
  folder <- file.path(dest, "supplemental/mixed_quality")
  write_rows(file.path(folder, "outputs.csv"), list(outputs))
  write_rows(file.path(folder, "annotations_comparative_judgment.csv"), list(pr))
  write_rows(file.path(folder, "annotations_rubric.csv"), list(rr))
}

args <- commandArgs(trailingOnly = TRUE)
if ("--help" %in% args || !length(args)) {
  cat("Rscript scripts/build_supplemental_data.R --original DIR --followup DIR --mixed-raw DIR --mixed-outputs FILE --destination DIR --generation-history FILE [--generation-history FILE ...]\n")
  quit(status = 0)
}
stopifnot(length(args) %% 2 == 0)
opts <- split(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
required <- c("original", "followup", "mixed-raw", "mixed-outputs", "destination", "generation-history")
stopifnot(setequal(names(opts), required), all(lengths(opts[setdiff(required, "generation-history")]) == 1))
build_followup(opts$followup, opts$original, opts$destination, opts[["generation-history"]])
x <- read_rows(file.path(opts$followup, "matched-comparisons.csv")) %>% distinct(task_number, task_id)
stopifnot(nrow(x) == 30L, !anyDuplicated(x$task_number))
build_mixed(opts[["mixed-raw"]], opts[["mixed-outputs"]], setNames(x$task_id, x$task_number), opts$destination)
cat("Built follow-up and mixed-quality public files. Nothing published.\n")
