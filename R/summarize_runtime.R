#' Summarize Runtime Durations
#'
#' Calculates descriptive runtime summaries within optional groups.
#'
#' The returned statistics are the count, total, arithmetic mean, median,
#' first quartile, third quartile, interquartile range, minimum, and maximum.
#' Quartiles use R's type 7 quantile definition.
#'
#' @param data A data frame containing the runtime and grouping columns.
#' @param runtime A single character string naming the runtime column. Values
#'   must be non-negative, finite, numeric durations with no missing values.
#' @param group_by A character vector naming columns that define the output
#'   groups. Use `character()` to summarize all rows together.
#'
#' @return A data frame containing the grouping columns followed by
#'   `n`, `total_seconds`, `mean_seconds`, `median_seconds`, `q1_seconds`,
#'   `q3_seconds`, `iqr_seconds`, `minimum_seconds`, and `maximum_seconds`.
#'   Groups appear in their first-observed order.
#'
#' @examples
#' runtimes <- data.frame(
#'   runtime_type = c("live", "live", "review", "review"),
#'   seconds = c(10, 20, 30, 50)
#' )
#'
#' summarize_runtime(
#'   data = runtimes,
#'   runtime = "seconds",
#'   group_by = "runtime_type"
#' )
#'
#' @export
summarize_runtime <- function(
  data,
  runtime,
  group_by = character()
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  validate_runtime_column_names(
    runtime = runtime,
    group_by = group_by
  )

  required_columns <- c(group_by, runtime)
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      "Missing column(s) in `data`: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  unsupported_group_columns <- group_by[
    !vapply(data[group_by], is.atomic, logical(1))
  ]

  if (length(unsupported_group_columns) > 0L) {
    stop(
      "Grouping columns must be atomic vectors. Unsupported column(s): ",
      paste(unsupported_group_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  validate_runtime_values(
    data[[runtime]],
    argument = runtime
  )

  if (length(group_by) == 0L) {
    group_values <- data.frame(.runtime_group = 1L)
  } else {
    group_values <- unique(data[group_by])
    rownames(group_values) <- NULL
  }

  group_count <- nrow(group_values)

  n <- integer(group_count)
  total_seconds <- numeric(group_count)
  mean_seconds <- numeric(group_count)
  median_seconds <- numeric(group_count)
  q1_seconds <- numeric(group_count)
  q3_seconds <- numeric(group_count)
  minimum_seconds <- numeric(group_count)
  maximum_seconds <- numeric(group_count)

  for (index in seq_len(group_count)) {
    members <- runtime_group_membership(
      data = data,
      group_by = group_by,
      group_values = group_values,
      index = index
    )

    values <- data[[runtime]][members]

    n[[index]] <- length(values)
    total_seconds[[index]] <- sum(values)
    mean_seconds[[index]] <- mean(values)
    median_seconds[[index]] <- stats::median(values)
    q1_seconds[[index]] <- stats::quantile(
      values,
      probs = 0.25,
      names = FALSE,
      type = 7
    )
    q3_seconds[[index]] <- stats::quantile(
      values,
      probs = 0.75,
      names = FALSE,
      type = 7
    )
    minimum_seconds[[index]] <- min(values)
    maximum_seconds[[index]] <- max(values)
  }

  iqr_seconds <- q3_seconds - q1_seconds

  if (length(group_by) == 0L) {
    output <- data.frame(
      n = n,
      total_seconds = total_seconds,
      mean_seconds = mean_seconds,
      median_seconds = median_seconds,
      q1_seconds = q1_seconds,
      q3_seconds = q3_seconds,
      iqr_seconds = iqr_seconds,
      minimum_seconds = minimum_seconds,
      maximum_seconds = maximum_seconds
    )
  } else {
    output <- group_values
    output$n <- n
    output$total_seconds <- total_seconds
    output$mean_seconds <- mean_seconds
    output$median_seconds <- median_seconds
    output$q1_seconds <- q1_seconds
    output$q3_seconds <- q3_seconds
    output$iqr_seconds <- iqr_seconds
    output$minimum_seconds <- minimum_seconds
    output$maximum_seconds <- maximum_seconds
  }

  rownames(output) <- NULL
  output
}


validate_runtime_column_names <- function(
  runtime,
  group_by
) {
  if (
    !is.character(runtime) ||
      length(runtime) != 1L ||
      is.na(runtime) ||
      !nzchar(runtime)
  ) {
    stop(
      "`runtime` must be one non-empty column name.",
      call. = FALSE
    )
  }

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

  if (runtime %in% group_by) {
    stop(
      "`runtime` must not also be a grouping column.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


validate_runtime_values <- function(
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

  if (anyNA(value)) {
    stop(
      "`",
      argument,
      "` must not contain missing values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(value))) {
    stop(
      "`",
      argument,
      "` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(value < 0)) {
    stop(
      "`",
      argument,
      "` must not contain negative values.",
      call. = FALSE
    )
  }

  invisible(value)
}


runtime_group_membership <- function(
  data,
  group_by,
  group_values,
  index
) {
  if (length(group_by) == 0L) {
    return(rep(TRUE, nrow(data)))
  }

  members <- rep(TRUE, nrow(data))

  for (column in group_by) {
    target <- group_values[[column]][[index]]
    values <- data[[column]]

    if (is.na(target)) {
      members <- members & is.na(values)
    } else {
      members <- members & !is.na(values) & values == target
    }
  }

  members
}
