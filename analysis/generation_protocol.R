# R equivalents of the Python generation counter and calibration rule.
# Used for validation; this file does not generate outputs.
word_count <- function(text) {
  matches <- gregexpr("(*UCP)[^\\W_]+(?:['’\\-][^\\W_]+)*", text, perl = TRUE)[[1]]
  if (matches[1] == -1L) 0L else length(matches)
}

length_instruction <- function(requested) {
  sprintf("Produce a response in approximately **%d words**, within **±2%%** of that target. Do not mention this length requirement.", requested)
}

accepted <- function(text, desired, old_E_words, complete = TRUE) {
  n <- word_count(text)
  complete && n > old_E_words && n >= ceiling(.95 * desired) && n <= floor(1.05 * desired)
}

next_requested <- function(desired, previous, actual, complete = TRUE) {
  if (!complete || actual <= 0) return(previous)
  multiplier <- min(1.25, max(.8, desired / actual))
  max(1L, floor(previous * multiplier + .5))
}
