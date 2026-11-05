# ============================================================
# Checks on released tables and supplemental results
# ============================================================
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(jsonlite)
  library(digest)
})
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
root <- dirname(dirname(normalizePath(gsub("~+~", " ", script, fixed = TRUE))))
data_dir <- Sys.getenv("EAH_JB_DIR", file.path(root, "result-dataset"))
out_dir <- file.path(Sys.getenv("EAH_ANALYSIS_OUTPUT_DIR", file.path(root, "outputs")), "supplemental")
source(file.path(root, "analysis", "generation_protocol.R"))
read_data <- function(path) read_csv(file.path(data_dir, path), show_col_types = FALSE)
close <- function(actual, expected, tolerance = 1e-8) {
  stopifnot(length(actual) == length(expected), all(abs(unlist(actual) - expected) <= tolerance))
}

old <- read_data("outputs/outputs.csv")
people <- read_data("human/annotators.csv")$annotator_id
tasks <- read_data("base/tasks.csv")$task_id
historical <- read_data("human/annotations_comparative_judgment.csv")
f <- read_data("followup/length_inversion/annotations.csv")
new <- read_data("followup/length_inversion/outputs.csv")
stopifnot(nrow(f) == 300, nrow(new) == 300, n_distinct(f$annotator_id) == 12,
          sum(f$pair_type == "EI") == 180, sum(f$pair_type == "EG") == 120,
          !anyDuplicated(f$annotation_id), !anyDuplicated(f$original_annotation_id), !anyDuplicated(new$output_id),
          all(f$annotator_id %in% people), setequal(f$task_id, tasks),
          !any(new$output_id %in% old$output_id),
          all(c(f$old_E_output_id, f$old_lower_output_id) %in% old$output_id),
          setequal(f$new_lower_output_id, new$output_id))
for (i in seq_len(nrow(f))) {
  r <- f[i, ]; h <- historical[match(r$original_annotation_id, historical$annotation_id), ]
  o <- new[match(r$new_lower_output_id, new$output_id), ]
  stopifnot(h$annotator_id == r$annotator_id, h$task_id == r$task_id,
            setequal(c(h$option_a_output_id, h$option_b_output_id), c(r$old_E_output_id, r$old_lower_output_id)),
            setequal(c(r$option_a_output_id, r$option_b_output_id), c(r$old_E_output_id, r$new_lower_output_id)),
            r$preferred_output_id == r[[paste0("option_", tolower(r$preferred_option), "_output_id")]],
            r$human_E_win_before == as.integer(h$preferred_output_id == r$old_E_output_id),
            r$human_E_win_after == as.integer(r$preferred_output_id == r$old_E_output_id),
            r$target_words == 2 * r$old_E_words - r$old_lower_words,
            r$old_lower_words < r$old_E_words,
            accepted(o$output_text, r$target_words, r$old_E_words),
            r$generation_attempts == r$retries + 1,
            digest(o$output_text, algo = "sha256", serialize = FALSE) == o$sha256,
            word_count(o$output_text) == r$new_lower_words, o$word_count == r$new_lower_words,
            word_count(old$output_text[match(r$old_E_output_id, old$output_id)]) == r$old_E_words,
            word_count(old$output_text[match(r$old_lower_output_id, old$output_id)]) == r$old_lower_words)
}
stopifnot(sum(f$human_E_win_before) == 211, sum(f$human_E_win_after) == 131)
attempts <- read_data("followup/length_inversion/generation_attempts.csv")
stopifnot(!anyDuplicated(attempts[c("output_id", "attempt")]), setequal(attempts$output_id, new$output_id))
for (i in seq_len(nrow(f))) {
  r <- f[i, ]; trace <- attempts %>% filter(output_id == r$new_lower_output_id) %>% arrange(attempt)
  stopifnot(identical(as.integer(trace$attempt), seq_len(r$generation_attempts)),
            sum(trace$accepted) == 1, tail(trace$accepted, 1) == 1)
  requested <- r$target_words
  for (j in seq_len(nrow(trace))) {
    a <- trace[j, ]
    qualifies <- a$complete == 1 && a$actual_words > r$old_E_words &&
      a$actual_words >= ceiling(.95 * r$target_words) && a$actual_words <= floor(1.05 * r$target_words)
    stopifnot(a$requested_words == requested, a$desired_words == r$target_words, a$accepted == as.integer(qualifies))
    requested <- next_requested(r$target_words, a$requested_words, a$actual_words, a$complete)
  }
}

items <- read_data("base/rubric_items.csv")$rubric_item_id
for (folder in list.dirs(file.path(data_dir, "autograders"), recursive = FALSE)) {
  for (filename in c("annotations_comparative_judgment.csv", "annotations_rubric.csv")) {
    a <- read_csv(file.path(folder, filename), show_col_types = FALSE)
    stopifnot(!anyDuplicated(a$annotation_id), all(a$task_id %in% tasks), all(a$corresponding_annotator_id %in% people))
    for (col in grep("output_id$", names(a), value = TRUE)) stopifnot(all(a[[col]] %in% old$output_id))
  }
  rubric <- read_csv(file.path(folder, "annotations_rubric.csv"), show_col_types = FALSE)
  scores <- read_csv(file.path(folder, "rubric_item_scores.csv"), show_col_types = FALSE)
  stopifnot(all(scores$annotation_id %in% rubric$annotation_id), all(scores$rubric_item_id %in% items))
}

mo <- read_csv(file.path(data_dir, "supplemental/mixed_quality/outputs.csv"),
               col_types = cols(profile = col_character()), show_col_types = FALSE)
mp <- read_data("supplemental/mixed_quality/annotations_comparative_judgment.csv")
mr <- read_data("supplemental/mixed_quality/annotations_rubric.csv")
stopifnot(nrow(mp) == 1500, nrow(mr) == 3000, nrow(mo) == 3000,
          !anyDuplicated(mo$output_id), !anyDuplicated(mr$output_id),
          setequal(mo$output_id, mr$output_id))
profile_mean <- vapply(strsplit(mo$profile, ""), function(x) mean(as.integer(x)), 0)
for (side in c("a", "b")) {
  idx <- match(mp[[paste0("option_", side, "_output_id")]], mo$output_id)
  role <- mp[[paste0("option_", side, "_role")]]
  stopifnot(!anyNA(idx), all(mo$task_id[idx] == mp$task_id),
            all(profile_mean[idx] == ifelse(role == "high", 2.5, 1.5)))
}

# ============================================================
# Reported numerical results
# ============================================================
j <- fromJSON(file.path(out_dir, "supplemental_results.json"), simplifyVector = FALSE)
o <- j$followup$results$Overall
close(o$Q, .07); close(o$L, 2 / 15); close(o$implied_length_balanced, .57)
close(o$remaining_share, 21 / 61); close(o$Q_ci, c(.007142857142857143, .13214285714285715))
close(j$stability$interval_alpha_win_counts, .132962026580827)
close(j$stability$consensus, .6623815967523681)
close(round(100 * unlist(j$stability$consensus_ci_reconstruction), 1), c(61.8, 70.7))
close(round(100 * vapply(j$original_length$quartiles, `[[`, 0, "win_rate"), 1), c(64, 67.1, 70.7, 65.7))
close(j$mixed_quality$delta, -.03013956414859127); close(j$mixed_quality$p, .35727464294536726)
close(j$timing$imputed_value, 48519.74642361111, .01)
expected <- list(Human = c(.669, .542, .127), gpt_5_4 = c(.599, .427, .172),
  gpt_5_4_mini = c(.702, .449, .254), claude_opus_4_8 = c(.890, .465, .425),
  claude_sonnet_5 = c(.852, .446, .406), gemini_3_1_pro = c(.820, .533, .287),
  gemini_3_6_flash = c(.819, .560, .259), qwen_3_7_max = c(.841, .524, .317), glm_5_2 = c(.875, .484, .390))
for (r in j$expanded_autograders) close(round(unlist(r[c("preference", "rubric", "difference")]), 3), expected[[r$evaluator]])
auc <- read_csv(file.path(out_dir, "surface_auc.csv"), show_col_types = FALSE)
close(round(auc$auc, 3), c(.670, .566, .746))
pred <- read_csv(file.path(out_dir, "surface_predictions.csv"), show_col_types = FALSE)
stopifnot(all(pred$probability >= 0 & pred$probability <= 1),
          all((pred %>% group_by(evaluator, task_id) %>% summarise(n = n_distinct(fold), .groups = "drop"))$n == 1))
cat("PASS: joins, generation/calibration, reported estimates and intervals, alpha, criterion and model results, and held-out surface AUCs.\n")
