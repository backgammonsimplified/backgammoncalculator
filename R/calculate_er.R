#' Calculate ER
#'
#' Calculates the 500-scale error rate from normalized equity lost and the
#' number of eligible decisions.
#'
#' ER is calculated after pooling the numerator and denominator:
#'
#' \deqn{ER = 500 \times \frac{equity\ lost}{eligible\ decisions}}
#'
#' The function is vectorized. A length-one argument is recycled to the length
#' of the other argument. Other unequal lengths are rejected.
#'
#' @param equity_lost A numeric vector of normalized equity lost. Values must
#'   be non-negative and finite. Missing values are allowed.
#' @param eligible_decisions A numeric vector containing eligible-decision
#'   counts. Values must be non-negative and finite. Missing values are
#'   allowed.
#'
#' @return A numeric vector containing ER on the 500 scale. An element is
#'   `NA_real_` when either input is missing or when `eligible_decisions` is
#'   zero.
#'
#' @examples
#' calculate_er(0.856, 1471)
#'
#' calculate_er(
#'   equity_lost = c(0.856, 0.515),
#'   eligible_decisions = c(1471, 1457)
#' )
#'
#' @export
calculate_er <- function(
  equity_lost,
  eligible_decisions
) {
  validate_er_input(
    equity_lost,
    argument = "equity_lost"
  )

  validate_er_input(
    eligible_decisions,
    argument = "eligible_decisions"
  )

  lengths <- c(
    length(equity_lost),
    length(eligible_decisions)
  )

  if (
    lengths[[1L]] != lengths[[2L]] &&
      !any(lengths == 1L)
  ) {
    stop(
      paste0(
        "`equity_lost` and `eligible_decisions` must have equal lengths, ",
        "or one argument must have length one."
      ),
      call. = FALSE
    )
  }

  output_length <- max(lengths)

  if (output_length == 0L) {
    return(numeric())
  }

  equity_lost <- rep_len(
    equity_lost,
    output_length
  )

  eligible_decisions <- rep_len(
    eligible_decisions,
    output_length
  )

  result <- rep(
    NA_real_,
    output_length
  )

  calculable <- !is.na(equity_lost) &
    !is.na(eligible_decisions) &
    eligible_decisions > 0

  result[calculable] <- 500 *
    equity_lost[calculable] /
    eligible_decisions[calculable]

  result
}


validate_er_input <- function(
  value,
  argument
) {
  if (!is.numeric(value)) {
    stop(
      "`",
      argument,
      "` must be numeric.",
      call. = FALSE
    )
  }

  if (any(is.nan(value))) {
    stop(
      "`",
      argument,
      "` must not contain NaN.",
      call. = FALSE
    )
  }

  if (any(is.infinite(value))) {
    stop(
      "`",
      argument,
      "` must contain only finite values or NA.",
      call. = FALSE
    )
  }

  if (any(value < 0, na.rm = TRUE)) {
    stop(
      "`",
      argument,
      "` must not contain negative values.",
      call. = FALSE
    )
  }

  invisible(value)
}
