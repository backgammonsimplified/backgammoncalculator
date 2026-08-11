#' Prepare match and mirrored-pair outcome data
#'
#' Calculate totals, proportions, and proportional-bar coordinates for the
#' match-score and mirrored-pair outcome figure. This function contains no
#' plotting, file access, or publication-path logic.
#'
#' @param sage_match_wins Number of individual matches won by Sage.
#' @param gnu_match_wins Number of individual matches won by GNU.
#' @param sage_pair_sweeps Number of mirrored pairs swept by Sage.
#' @param tied_pairs Number of mirrored pairs split one match each.
#' @param gnu_pair_sweeps Number of mirrored pairs swept by GNU.
#'
#' @return A data frame with one row per outcome segment.
#'
#' @examples
#' outcomes <- prepare_pair_outcomes(
#'   sage_match_wins = 7,
#'   gnu_match_wins = 13,
#'   sage_pair_sweeps = 1,
#'   tied_pairs = 5,
#'   gnu_pair_sweeps = 4
#' )
#'
#' outcomes[, c("score_type", "outcome", "count", "share")]
#'
#' @export
prepare_pair_outcomes <- function(
    sage_match_wins,
    gnu_match_wins,
    sage_pair_sweeps,
    tied_pairs,
    gnu_pair_sweeps) {
  counts <- c(
    sage_match_wins = sage_match_wins,
    gnu_match_wins = gnu_match_wins,
    sage_pair_sweeps = sage_pair_sweeps,
    tied_pairs = tied_pairs,
    gnu_pair_sweeps = gnu_pair_sweeps
  )

  invalid_counts <-
    !is.numeric(counts) |
    !is.finite(counts) |
    counts < 0 |
    counts != floor(counts)

  if (any(invalid_counts)) {
    stop(
      "All outcome counts must be finite, non-negative whole numbers.",
      call. = FALSE
    )
  }

  match_total <- sage_match_wins + gnu_match_wins
  pair_total <- sage_pair_sweeps + tied_pairs + gnu_pair_sweeps

  if (match_total <= 0) {
    stop(
      "The total number of matches must be greater than zero.",
      call. = FALSE
    )
  }

  if (pair_total <= 0) {
    stop(
      "The total number of mirrored pairs must be greater than zero.",
      call. = FALSE
    )
  }

  data <- rbind(
    data.frame(
      score_type = "matches",
      row_order = 2,
      outcome = c(
        "Sage won",
        "GNU won"
      ),
      record = c(
        NA_character_,
        NA_character_
      ),
      outcome_group = c(
        "Sage",
        "GNU"
      ),
      count = c(
        sage_match_wins,
        gnu_match_wins
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      score_type = "pairs",
      row_order = 1,
      outcome = c(
        "Sage Swept",
        "Tied",
        "GNU Swept"
      ),
      record = c(
        "(2-0)",
        "(1-1)",
        "(2-0)"
      ),
      outcome_group = c(
        "Sage",
        "Tied",
        "GNU"
      ),
      count = c(
        sage_pair_sweeps,
        tied_pairs,
        gnu_pair_sweeps
      ),
      stringsAsFactors = FALSE
    )
  )

  data$total <- stats::ave(
    data$count,
    data$score_type,
    FUN = sum
  )

  data$share <- data$count / data$total

  data$xmax <- stats::ave(
    data$share,
    data$score_type,
    FUN = cumsum
  )

  data$xmin <- data$xmax - data$share
  data$xmid <- (data$xmin + data$xmax) / 2

  data$percentage_label <- sprintf(
    "%.0f%%",
    100 * data$share
  )

  data
}
