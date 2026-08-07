#' Summarize ER
#'
#' Pools normalized equity lost and eligible decisions within groups, then
#' calculates ER from the pooled numerator and denominator.
#'
#' This function does not average row-level ER values. For every requested
#' group it calculates:
#'
#' \deqn{ER = 500 \times
#' \frac{\sum equity\ lost}{\sum eligible\ decisions}}
#'
#' @param data A data frame containing the grouping and metric columns.
#' @param group_by A character vector naming columns that define the output
#'   groups. Use `character()` to calculate one summary across all rows.
#' @param equity_lost A single character string naming the normalized-equity-
#'   lost column.
#' @param eligible_decisions A single character string naming the eligible-
#'   decision-count column.
#'
#' @return A data frame containing the grouping columns followed by
#'   `equity_lost`, `eligible_decisions`, and `er`. Groups appear in their
#'   first-observed order. A group returns `NA_real_` for a pooled metric when
#'   any contributing value for that metric is missing. ER is also
#'   `NA_real_` when the pooled eligible-decision count is zero.
#'
#' @examples
#' decisions <- data.frame(
#'   engine = c("Sage", "Sage", "GNU", "GNU"),
#'   loss = c(0.4, 0.6, 0.2, 0.3),
#'   n = c(50, 50, 50, 50)
#' )
#'
#' summarize_er(
#'   data = decisions,
#'   group_by = "engine",
#'   equity_lost = "loss",
#'   eligible_decisions = "n"
#' )
#'
#' @export
summarize_er <- function(
  data,
  group_by,
  equity_lost,
  eligible_decisions
) {
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame.",
      call. = FALSE
    )
  }

  validate_er_column_names(
    group_by = group_by,
    equity_lost = equity_lost,
    eligible_decisions = eligible_decisions
  )

  required_columns <- c(
    group_by,
    equity_lost,
    eligible_decisions
  )

  missing_columns <- setdiff(
    required_columns,
    names(data)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "Missing column(s) in `data`: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  unsupported_group_columns <- group_by[
    !vapply(
      data[group_by],
      is.atomic,
      logical(1)
    )
  ]

  if (length(unsupported_group_columns) > 0L) {
    stop(
      "Grouping columns must be atomic vectors. Unsupported column(s): ",
      paste(unsupported_group_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  validate_er_input(
    data[[equity_lost]],
    argument = equity_lost
  )

  validate_er_input(
    data[[eligible_decisions]],
    argument = eligible_decisions
  )

  if (length(group_by) == 0L) {
    group_values <- data.frame(
      .summary_group = 1L
    )
  } else {
    group_values <- unique(
      data[group_by]
    )

    rownames(group_values) <- NULL
  }

  group_count <- nrow(group_values)

  pooled_equity_lost <- rep(
    NA_real_,
    group_count
  )

  pooled_eligible_decisions <- rep(
    NA_real_,
    group_count
  )

  for (index in seq_len(group_count)) {
    members <- er_group_membership(
      data = data,
      group_by = group_by,
      group_values = group_values,
      index = index
    )

    pooled_equity_lost[[index]] <- sum(
      data[[equity_lost]][members]
    )

    pooled_eligible_decisions[[index]] <- sum(
      data[[eligible_decisions]][members]
    )
  }

  er <- calculate_er(
    equity_lost = pooled_equity_lost,
    eligible_decisions = pooled_eligible_decisions
  )

  if (length(group_by) == 0L) {
    output <- data.frame(
      equity_lost = pooled_equity_lost,
      eligible_decisions = pooled_eligible_decisions,
      er = er
    )
  } else {
    output <- group_values
    output$equity_lost <- pooled_equity_lost
    output$eligible_decisions <- pooled_eligible_decisions
    output$er <- er
  }

  rownames(output) <- NULL
  output
}


validate_er_column_names <- function(
  group_by,
  equity_lost,
  eligible_decisions
) {
  if (
    !is.character(group_by) ||
      anyNA(group_by) ||
      any(!nzchar(group_by))
  ) {
    stop(
      "`group_by` must be a character vector of non-empty column names.",
      call. = FALSE
    )
  }

  if (anyDuplicated(group_by)) {
    stop(
      "`group_by` must not contain duplicate column names.",
      call. = FALSE
    )
  }

  for (
    argument in c(
      "equity_lost",
      "eligible_decisions"
    )
  ) {
    value <- get(
      argument,
      inherits = FALSE
    )

    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !nzchar(value)
    ) {
      stop(
        "`",
        argument,
        "` must be one non-empty column name.",
        call. = FALSE
      )
    }
  }

  if (identical(equity_lost, eligible_decisions)) {
    stop(
      "`equity_lost` and `eligible_decisions` must name different columns.",
      call. = FALSE
    )
  }

  metric_columns_in_groups <- intersect(
    group_by,
    c(
      equity_lost,
      eligible_decisions
    )
  )

  if (length(metric_columns_in_groups) > 0L) {
    stop(
      "Metric columns must not also be grouping columns: ",
      paste(metric_columns_in_groups, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


er_group_membership <- function(
  data,
  group_by,
  group_values,
  index
) {
  if (length(group_by) == 0L) {
    return(
      rep(
        TRUE,
        nrow(data)
      )
    )
  }

  members <- rep(
    TRUE,
    nrow(data)
  )

  for (column in group_by) {
    target <- group_values[[column]][[index]]
    values <- data[[column]]

    if (is.na(target)) {
      members <- members & is.na(values)
    } else {
      members <- members &
        !is.na(values) &
        values == target
    }
  }

  members
}
