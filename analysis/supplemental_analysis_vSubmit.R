# ============================================================
# Supplemental analyses (submission version)
# ============================================================
# Uses the released CSVs; no model calls. Configure EAH_JB_DIR and
# EAH_ANALYSIS_OUTPUT_DIR as in analysis_vSubmit.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(jsonlite)
  library(ggplot2)
  library(pROC)
  library(ragg)
})

# ============================================================
# Controls and helpers
# ============================================================
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
script <- gsub("~+~", " ", script, fixed = TRUE)
root <- dirname(dirname(normalizePath(script)))
data_dir <- Sys.getenv("EAH_JB_DIR", file.path(root, "result-dataset"))
out_dir <- file.path(Sys.getenv("EAH_ANALYSIS_OUTPUT_DIR", file.path(root, "outputs")), "supplemental")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Saved cluster multiplicities preserve the resamples used for the paper.
# Columns identify annotators (follow-up, stability) or tasks (mixed quality).
weights <- readRDS(file.path(root, "analysis", "bootstrap_weights.rds"))
quality <- c(intermediate = 0L, good = 1L, excellent = 2L)
pairs <- list(c("intermediate", "good"), c("good", "excellent"), c("intermediate", "excellent"))
read_data <- function(path) read_csv(file.path(data_dir, path), show_col_types = FALSE)
ci <- function(x) unname(quantile(x, c(.025, .975)))

preferences <- function(d) {
  d %>% mutate(qa = unname(quality[option_a_quality_level]),
               qb = unname(quality[option_b_quality_level]),
               correct = as.integer(ifelse(preferred_option == "A", qa > qb, qb > qa)),
               pair = paste0(pmin(qa, qb), pmax(qa, qb)))
}

rubric_pairs <- function(d) {
  person <- if ("annotator_id" %in% names(d)) "annotator_id" else "corresponding_annotator_id"
  scores <- d %>% select(all_of(c(person, "task_id", "task_slot_order")),
                         output_quality_level, rubric_total_points) %>%
    pivot_wider(names_from = output_quality_level, values_from = rubric_total_points)
  stopifnot(!anyNA(scores[names(quality)]))
  bind_rows(lapply(pairs, function(p) {
    delta <- scores[[p[2]]] - scores[[p[1]]]
    tibble(task_id = scores$task_id, pair = paste(p, collapse = "/"),
           correct = ifelse(abs(delta) < 1e-5, .5, as.numeric(delta > 0)))
  }))
}

# ============================================================
# Length inversion
# ============================================================
followup <- function() {
  f <- read_data("followup/length_inversion/annotations.csv")
  people <- sort(unique(f$annotator_id))
  stopifnot(identical(people, colnames(weights$followup)))
  results <- list()
  for (group in c("EI", "EG", "Overall")) {
    selected <- if (group == "Overall") f else filter(f, pair_type == group)
    x <- t(vapply(people, function(person) {
      z <- filter(selected, annotator_id == person)
      c(nrow(z), sum(z$human_E_win_before), sum(z$human_E_win_after))
    }, numeric(3)))
    totals <- colSums(x)
    n <- totals[1]; before <- totals[2]; after <- totals[3]
    samples <- weights$followup %*% x
    samples <- samples[samples[, 1] > 0, , drop = FALSE]
    N <- samples[, 1]; B <- samples[, 2]; A <- samples[, 3]
    b <- B / N; a <- A / N
    q <- (b + a - 1) / 2; length <- (b - a) / 2
    valid <- 2 * B != N
    shares <- (B[valid] + A[valid] - N[valid]) / (2 * B[valid] - N[valid])
    results[[group]] <- list(
      n = n, before_wins = before, after_wins = after,
      before = before / n, after = after / n, before_ci = ci(b), after_ci = ci(a),
      drop = (before - after) / n, drop_ci = ci(b - a),
      L = (before - after) / (2 * n), L_ci = ci(length),
      Q = (before + after - n) / (2 * n), Q_ci = ci(q),
      implied_length_balanced = (before + after) / (2 * n),
      implied_length_balanced_ci = ci(.5 + q),
      remaining_share = (before + after - n) / (2 * before - n),
      remaining_share_ci = ci(shares), undefined_share_replicates = sum(!valid))
  }
  table <- bind_rows(lapply(names(results), function(group) {
    as_tibble(c(list(comparison = group), Filter(function(x) length(x) == 1L, results[[group]])))
  }))
  write_csv(table, file.path(out_dir, "length_inversion.csv"))
  list(seed = 20261014, replicates = nrow(weights$followup), annotators = length(people),
       method = "Paired annotator-cluster percentile bootstrap; judgment-weighted rates. Negative shares retained; undefined shares excluded. No additional task clustering.",
       results = results)
}

# ============================================================
# Original length diagnostics
# ============================================================
original_length <- function(cj) {
  outputs <- read_data("outputs/outputs.csv")
  # Original diagnostics used whitespace tokens, not the generation counter.
  words <- lengths(strsplit(trimws(outputs$output_text), "\\s+", perl = TRUE))
  names(words) <- outputs$output_id
  gaps <- unname(words[cj$option_a_output_id] - words[cj$option_b_output_id])
  ord <- order(abs(gaps), seq_along(gaps))
  sizes <- rep(length(ord) %/% 4, 4)
  sizes[seq_len(length(ord) %% 4)] <- sizes[seq_len(length(ord) %% 4)] + 1L
  quartiles <- lapply(split(ord, rep(1:4, sizes)), function(idx) {
    list(n = length(idx), minimum_words = min(abs(gaps)[idx]),
         maximum_words = max(abs(gaps)[idx]), win_rate = mean(cj$correct[idx]))
  })
  quartiles <- lapply(seq_along(quartiles), function(i) c(list(quartile = i), quartiles[[i]]))
  means <- outputs %>% mutate(words = unname(words[output_id])) %>%
    group_by(task_id, quality_level) %>% summarise(words = mean(words), .groups = "drop") %>%
    pivot_wider(names_from = quality_level, values_from = words)
  fit <- glm(correct ~ log1p(abs(gaps)), data = cj, family = binomial())
  coef <- summary(fit)$coefficients[2, ]
  list(quartiles = quartiles,
       longer_has_higher_requested_quality = mean((gaps > 0) == (cj$qa > cj$qb)),
       tasks_E_G_I = sum(means$excellent > means$good & means$good > means$intermediate),
       median_task_mean_length_ratios = setNames(lapply(pairs, function(p) median(means[[p[2]]] / means[[p[1]]])),
                                                 vapply(pairs, function(p) paste(rev(p), collapse = "/"), "")),
       logit = list(coefficient = unname(coef[1]), standard_error = unname(coef[2]), p = unname(coef[4])))
}

# ============================================================
# Rubric criteria
# ============================================================
criteria <- function(folder) {
  items <- read_data("base/rubric_items.csv")
  a <- read_data(file.path(folder, "annotations_rubric.csv"))
  scores <- read_data(file.path(folder, "rubric_item_scores.csv")) %>%
    inner_join(select(items, rubric_item_id, task_id, weight), by = "rubric_item_id") %>%
    inner_join(select(a, annotation_id, output_quality_level), by = "annotation_id")
  positive <- filter(scores, weight > 0)
  negative <- filter(scores, weight < 0)
  groups <- positive %>% group_by(task_id, rubric_item_id) %>% group_split()
  shares <- list()
  for (p in pairs) {
    counts <- Reduce(`+`, lapply(groups, function(g) {
      lo <- g$awarded_points[g$output_quality_level == p[1]]
      hi <- g$awarded_points[g$output_quality_level == p[2]]
      delta <- outer(hi, lo, `-`)
      c(sum(delta > 1e-5), sum(abs(delta) < 1e-5), sum(delta < -1e-5))
    }))
    shares[[paste(rev(p), collapse = "/")]] <- list(n = sum(counts), more_equal_fewer = counts / sum(counts))
  }
  pos <- tapply(positive$awarded_points, positive$annotation_id, sum)
  neg <- tapply(negative$awarded_points, negative$annotation_id, sum)
  a <- a %>% mutate(positive = coalesce(as.numeric(pos[annotation_id]), 0),
                     negative = coalesce(as.numeric(neg[annotation_id]), 0))
  task_order <- function(column) {
    m <- a %>% group_by(task_id, output_quality_level) %>%
      summarise(score = mean(.data[[column]]), .groups = "drop") %>%
      pivot_wider(names_from = output_quality_level, values_from = score)
    setNames(lapply(pairs, function(p) mean(m[[p[2]]] > m[[p[1]]])),
             vapply(pairs, function(p) paste(rev(p), collapse = "/"), ""))
  }
  m <- a %>% group_by(output_quality_level) %>%
    summarise(positive = mean(positive), negative = mean(negative), .groups = "drop")
  list(criterion_shares = shares,
       mean_points = list(positive = as.list(setNames(m$positive, m$output_quality_level)),
                          negative = as.list(setNames(m$negative, m$output_quality_level))),
       original_task_order = task_order("rubric_total_points"),
       positive_only_task_order = task_order("positive"))
}

# ============================================================
# Annotator stability
# ============================================================
stability <- function(cj) {
  strata <- cj %>% group_by(task_id, pair) %>% summarise(n = n(), k = sum(correct), .groups = "drop")
  raw <- with(strata, sum(k * (k - 1) + (n - k) * (n - k - 1)) / sum(n * (n - 1)))
  cc <- cj %>% left_join(strata, by = c("task_id", "pair")) %>%
    mutate(other = k - correct, tied = 2 * other == n - 1,
           consensus = ifelse(tied, NA_real_, as.numeric((2 * other > n - 1) == (correct == 1))))
  people <- sort(unique(cj$annotator_id))
  stopifnot(identical(people, colnames(weights$stability)))
  clusters <- t(vapply(people, function(person) {
    z <- filter(cc, annotator_id == person)
    c(nrow(z), sum(z$correct), sum(!is.na(z$consensus)), sum(z$consensus, na.rm = TRUE))
  }, numeric(4)))
  sample <- weights$stability %*% clusters
  # Reconstruction: resample annotators with observed consensus labels fixed.
  leave <- vapply(people, function(person) mean(cj$correct[cj$annotator_id != person]), 0)
  blocks <- cj %>% group_by(annotator_id, task_id) %>% group_split()
  wins <- bind_rows(lapply(blocks, function(g) {
    stopifnot(nrow(g) == 3L)
    winners <- ifelse(g$preferred_option == "A", g$qa, g$qb)
    tibble(task_id = g$task_id[1], quality = 0:2, wins = tabulate(winners + 1L, nbins = 3))
  }))
  counts <- wins %>% group_by(task_id, quality) %>% group_split() %>%
    lapply(function(g) tabulate(g$wins + 1L, nbins = 3))
  coincidence <- Reduce(`+`, lapply(counts, function(x) (outer(x, x) - diag(x)) / (sum(x) - 1)))
  margin <- rowSums(coincidence)
  expected <- (outer(margin, margin) - diag(margin)) / (sum(margin) - 1)
  distance <- outer(0:2, 0:2, `-`)^2
  alpha <- 1 - (sum(coincidence * distance) / sum(coincidence)) / (sum(expected * distance) / sum(expected))
  list(raw_dyadic = raw, consensus = mean(cc$consensus, na.rm = TRUE),
       tied_other_votes_excluded = sum(cc$tied),
       consensus_ci_reconstruction = ci(sample[, 4] / sample[, 3]),
       higher_quality_win_rate = mean(cj$correct),
       higher_quality_ci_reconstruction = ci(sample[, 2] / sample[, 1]),
       leave_one_annotator_range = range(leave), interval_alpha_win_counts = alpha,
       bootstrap_seed = 20261030,
       bootstrap_note = "Reconstruction: resample annotators with fixed observed leave-one-out consensus labels, exclude tied consensus. Original CI implementation not recovered.")
}

# ============================================================
# Additional evaluators and quality-pair tests
# ============================================================
expanded <- function() {
  dirs <- sort(list.dirs(file.path(data_dir, "autograders"), recursive = FALSE, full.names = FALSE))
  folders <- c(Human = "human", setNames(file.path("autograders", dirs), dirs))
  results <- tests <- list()
  for (name in names(folders)) {
    folder <- folders[[name]]
    cj <- preferences(read_data(file.path(folder, "annotations_comparative_judgment.csv")))
    r <- read_data(file.path(folder, "annotations_rubric.csv"))
    pref <- cj %>% group_by(task_id) %>% summarise(score = mean(correct)) %>% pull(score) %>% mean()
    rub <- rubric_pairs(r) %>% group_by(task_id) %>% summarise(score = mean(correct)) %>% pull(score) %>% mean()
    results[[name]] <- list(evaluator = name, preference = pref, rubric = rub, difference = pref - rub)
    if (name %in% c("Human", "gpt_5_4")) {
      rs <- r %>% group_by(task_id, output_quality_level) %>%
        summarise(score = mean(rubric_total_points), .groups = "drop") %>%
        pivot_wider(names_from = output_quality_level, values_from = score)
      cs <- cj %>% mutate(winner = ifelse(preferred_option == "A", qa, qb)) %>% count(task_id, winner) %>%
        pivot_wider(names_from = winner, values_from = n, values_fill = 0)
      for (method in c("Rubric", "CJ")) {
        frame <- if (method == "Rubric") rs else cs
        for (p in pairs) {
          cols <- if (method == "Rubric") p else as.character(quality[p])
          k <- sum(frame[[cols[2]]] > frame[[cols[1]]]); n <- nrow(frame)
          test <- binom.test(k, n, p = .5)
          tests[[length(tests) + 1L]] <- list(evaluator = name, method = method,
            pair = paste(p, collapse = "/"), k = k, n = n, p = test$p.value,
            adjusted_p_12 = min(1, 12 * test$p.value), exact_ci = unname(test$conf.int))
        }
      }
    }
  }
  write_csv(bind_rows(results), file.path(out_dir, "expanded_autograders.csv"))
  test_table <- bind_rows(lapply(tests, function(t) {
    t$ci_lower <- t$exact_ci[1]; t$ci_upper <- t$exact_ci[2]; t$exact_ci <- NULL
    as_tibble(t)
  }))
  write_csv(test_table, file.path(out_dir, "bonferroni_12.csv"))
  list(results = unname(results), tests = tests)
}

# ============================================================
# Mixed-quality comparisons
# ============================================================
mixed <- function() {
  p <- read_data("supplemental/mixed_quality/annotations_comparative_judgment.csv")
  r <- read_data("supplemental/mixed_quality/annotations_rubric.csv")
  scores <- setNames(r$rubric_total_points, r$output_id)
  delta <- unname(scores[p$option_a_output_id] - scores[p$option_b_output_id]) * ifelse(p$option_a_role == "high", 1, -1)
  m <- p %>% mutate(correct = as.numeric(ifelse(preferred_option == "A", option_a_role == "high", option_b_role == "high")),
                     rubric = ifelse(abs(delta) < 1e-5, .5, as.numeric(delta > 0))) %>%
    group_by(task_id) %>% summarise(correct = mean(correct), rubric = mean(rubric), .groups = "drop")
  pref <- preferences(read_data("autograders/gpt_5_4/annotations_comparative_judgment.csv")) %>%
    filter(abs(qa - qb) == 1) %>% group_by(task_id) %>% summarise(old_pref = mean(correct))
  rub <- rubric_pairs(read_data("autograders/gpt_5_4/annotations_rubric.csv")) %>%
    filter(pair != "intermediate/excellent") %>% group_by(task_id) %>% summarise(old_rub = mean(correct))
  stopifnot(setequal(m$task_id, pref$task_id), setequal(m$task_id, rub$task_id))
  m <- m %>% inner_join(pref, by = "task_id") %>% inner_join(rub, by = "task_id") %>% arrange(task_id)
  stopifnot(identical(m$task_id, colnames(weights$mixed)))
  v <- with(m, correct - rubric - old_pref + old_rub)
  draws <- as.numeric(weights$mixed %*% v) / nrow(m)
  se <- sd(draws)
  list(preference = mean(m$correct), rubric = mean(m$rubric), delta = mean(v), se = se,
       ci = mean(v) + c(-1, 1) * qnorm(.975) * se, p = 2 * pnorm(-abs(mean(v) / se)),
       bootstrap = "2000 paired task resamples, Python random.Random(0), normal interval centered on observed delta")
}

# ============================================================
# Timing and valuation
# ============================================================
timing <- function(cj) {
  r <- read_data("human/annotations_rubric.csv")
  people <- read_data("human/annotators.csv")
  d <- bind_rows(select(cj, annotator_id, time_spent_seconds) %>% mutate(method = "CJ"),
                 select(r, annotator_id, time_spent_seconds) %>% mutate(method = "Rubric")) %>%
    inner_join(people, by = "annotator_id") %>% filter(time_spent_seconds >= 0, time_spent_seconds <= 1800)
  hours <- tapply(d$time_spent_seconds, d$organization_type, sum) / 3600
  minutes <- tapply(d$time_spent_seconds, d$method, median) / 60
  list(median_minutes = as.list(minutes), hours_by_source = as.list(hours),
       imputed_value = sum(hours * ifelse(names(hours) == "Data Labeling Company", 125, 1000)))
}

# ============================================================
# Surface-feature prediction of pairwise choices
# ============================================================
# Recovered rebuttal analysis: five task-held-out folds, with the original
# vowel-group syllable approximation for the Flesch features.
surface_analysis <- function() {
  plot_theme_publication <- function(base_size = 14) {
    theme_minimal(base_size = base_size) +
      theme(
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = base_size + 5),
        plot.subtitle = element_text(size = base_size - 1, color = "grey25"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "grey20"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey88", linewidth = 0.35),
        strip.background = element_rect(fill = "grey94", color = "grey70", linewidth = 0.4),
        strip.text = element_text(face = "bold", color = "grey15"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  }
  judge_colors <- c("Human" = "#1B9E77", "GPT-5.4" = "#D95F02", "GPT-5.4-mini" = "#7570B3")

  # Count matches without extracting substrings.
  cnt_vec <- function(texts, pat) {
    m <- gregexpr(pat, texts, perl = TRUE)
    n <- lengths(m)
    n[vapply(m, function(v) v[1] == -1L, logical(1))] <- 0L
    n
  }

  # Compute all 7 features for a vector of texts at once. Syllables use a
  # vectorized vowel-group heuristic at the document level: total vowel groups,
  # minus silent word-final "e" (consonant+e), floored at >=1 syllable per word.
  feature_names <- c("nw", "wps", "cpw", "flesch", "fk", "n_list", "n_head")
  compute_features <- function(texts) {
    texts[is.na(texts)] <- ""
    lt <- tolower(texts)
    nw <- cnt_vec(texts, "\\w+")                                   # word count
    nc <- nchar(gsub("[^A-Za-z0-9_]", "", texts))                 # word chars
    ns <- pmax(cnt_vec(texts, "[.!?]+"), 1L)                       # sentences, floor 1
    vg <- cnt_vec(lt, "[aeiouy]+")                                # vowel groups
    sil <- cnt_vec(lt, "[bcdfghjklmnpqrstvwxz]e\\b")             # silent final e
    syl <- pmax(vg - sil, nw)                                     # >=1 syllable/word
    wps <- nw / ns; spw <- ifelse(nw > 0, syl / nw, 0)
    M <- cbind(nw = nw, wps = wps, cpw = ifelse(nw > 0, nc / nw, 0),
               flesch = 206.835 - 1.015 * wps - 84.6 * spw,
               fk = 0.39 * wps + 11.8 * spw - 15.59,
               n_list = cnt_vec(texts, "(?m)^\\s*(?:[-*+]|\\d+\\.)\\s+"),
               n_head = cnt_vec(texts, "(?m)^\\s*#{1,6}\\s+"))
    colnames(M) <- feature_names; M
  }

  out <- read.csv(file.path(data_dir, "outputs", "outputs.csv"), stringsAsFactors = FALSE)
  feature_matrix <- compute_features(out$output_text)
  rownames(feature_matrix) <- out$output_id

  diff_matrix <- function(df) {
    feature_matrix[df$option_a_output_id, , drop = FALSE] - feature_matrix[df$option_b_output_id, , drop = FALSE]
  }

  # Five task folds: assign the largest groups to the smallest fold first.
  group_kfold <- function(groups, k = 5) {
    tab <- sort(table(groups), decreasing = TRUE)
    fold_of_group <- integer(0); fold_sizes <- numeric(k)
    for (g in names(tab)) {
      f <- which.min(fold_sizes)
      fold_of_group[g] <- f; fold_sizes[f] <- fold_sizes[f] + tab[[g]]
    }
    fold_of_group[as.character(groups)]
  }

  cv_predict <- function(X, y, groups, k = 5) {
    folds <- group_kfold(groups, k)
    p <- numeric(length(y))
    dat <- data.frame(y = y, X)
    for (f in sort(unique(folds))) {
      tr <- folds != f; te <- folds == f
      m <- suppressWarnings(glm(y ~ ., data = dat[tr, , drop = FALSE], family = binomial()))
      p[te] <- predict(m, newdata = dat[te, , drop = FALSE], type = "response")
    }
    p
  }

  # Wilson intervals for calibration deciles.
  calib <- function(y, p, nb = 10) {
    br <- unique(quantile(p, probs = seq(0, 1, length.out = nb + 1)))
    b <- cut(p, breaks = br, include.lowest = TRUE)
    z <- 1.96
    do.call(rbind, lapply(split(seq_along(p), b), function(idx) {
      n <- length(idx); ph <- mean(y[idx]); den <- 1 + z^2 / n
      ctr <- (ph + z^2 / (2 * n)) / den
      half <- z * sqrt(ph * (1 - ph) / n + z^2 / (4 * n^2)) / den
      data.frame(pred = mean(p[idx]), obs = ph, lo = ctr - half, hi = ctr + half, n = n)
    }))
  }

  read_cj <- function(path) {
    d <- read.csv(path, stringsAsFactors = FALSE)
    d[d$preferred_option %in% c("A", "B"), ]
  }

  human <- read_cj(file.path(data_dir, "human", "annotations_comparative_judgment.csv"))
  judges <- list(
    list(name = "Human", df = human),
    list(name = "GPT-5.4",
         df = read_cj(file.path(data_dir, "autograders", "gpt_5_4", "annotations_comparative_judgment.csv"))),
    list(name = "GPT-5.4-mini",
         df = read_cj(file.path(data_dir, "autograders", "gpt_5_4_mini", "annotations_comparative_judgment.csv")))
  )

  calib_rows <- list(); auc_lab <- character()
  auc_rows <- list(); predictions <- list()
  write.csv(data.frame(output_id = out$output_id, feature_matrix),
            file.path(out_dir, "surface_features.csv"), row.names = FALSE)
  for (j in judges) {
    df <- j$df
    X <- as.data.frame(diff_matrix(df)); names(X) <- feature_names
    y <- as.integer(df$preferred_option == "A")
    p <- cv_predict(X, y, df$task_id)
    a <- as.numeric(pROC::auc(pROC::roc(y, p, direction = "<", quiet = TRUE)))
    cat(sprintf("%s: AUC=%.3f\n", j$name, a))
    auc_rows[[j$name]] <- data.frame(evaluator = j$name, n = length(y), auc = a)
    predictions[[j$name]] <- data.frame(evaluator = j$name, annotation_id = df$annotation_id,
      task_id = df$task_id, fold = unname(group_kfold(df$task_id)), A_preferred = y, probability = p)
    cc <- calib(y, p); cc$judge <- j$name
    cc$label <- sprintf("%s (AUC %.2f)", j$name, a)
    calib_rows[[j$name]] <- cc
    auc_lab[j$name] <- cc$label[1]
  }
  write.csv(bind_rows(auc_rows), file.path(out_dir, "surface_auc.csv"), row.names = FALSE)
  write.csv(bind_rows(predictions), file.path(out_dir, "surface_predictions.csv"), row.names = FALSE)
  calibration <- bind_rows(calib_rows)
  write.csv(calibration, file.path(out_dir, "surface_calibration.csv"), row.names = FALSE)
  calibration$judge <- factor(calibration$judge, levels = names(judge_colors))
  lab_map <- vapply(names(judge_colors), function(n) auc_lab[[n]], character(1))

  plot <- ggplot(calibration, aes(pred, obs, color = judge)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.6) +
    scale_color_manual(values = judge_colors, labels = lab_map, name = NULL) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    scale_x_continuous(breaks = seq(0, 1, 0.2), expand = expansion(mult = 0.01)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), expand = expansion(mult = 0.01)) +
    labs(x = "Predicted P(A preferred)", y = "Observed A-win rate") +
    plot_theme_publication(base_size = 14) +
    theme(legend.position = c(0.03, 0.97), legend.justification = c(0, 1),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key.height = unit(1.1, "lines"), legend.text = element_text(size = 11.5),
          plot.margin = margin(t = 10, r = 16, b = 8, l = 12))
  ggsave(file.path(out_dir, "surface_calibration.png"), plot,
         width = 6.6, height = 6.6, dpi = 220, device = ragg::agg_png)
  invisible(bind_rows(auc_rows))

}

# ============================================================
# Run
# ============================================================
cj <- preferences(read_data("human/annotations_comparative_judgment.csv"))
ex <- expanded()
result <- list(followup = followup(), original_length = original_length(cj),
               human_criteria = criteria("human"), gpt_criteria = criteria("autograders/gpt_5_4"),
               stability = stability(cj), expanded_autograders = ex$results, binomial_tests = ex$tests,
               mixed_quality = mixed(), timing = timing(cj))
write_json(result, file.path(out_dir, "supplemental_results.json"), auto_unbox = TRUE, pretty = TRUE, digits = 16)
write_csv(bind_rows(result$original_length$quartiles), file.path(out_dir, "length_quartiles.csv"))
surface_analysis()
cat("Supplemental analyses complete. Results in", out_dir, "\n")
