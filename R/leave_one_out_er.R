#' Recalculate ER After Leaving Out Each Unit
#'
#' Performs a descriptive leave-one-unit-out sensitivity calculation. For each
#' unique unit, the function removes every row belonging to that unit, pools
#' normalized equity lost and eligible decisions for two engines, recalculates
#' ER, and compares the remaining ER values.
#'
#' This function does not produce a bootstrap, confidence interval, or
#' significance test.
#'
#' @param data A data frame containing unit, engine, and metric columns.
#' @param unit A single character string naming the unit to omit in turn, such
#'   as `"pair_id"`.
#' @param engine A single character string naming the engine column.
#' @param equity_lost A single character string naming the normalized-equity-
#'   lost column.
#' @param eligible_decisions A single character string naming the eligible-
#'   decision-count column.
#' @param left_engine A single non-empty character string naming the left-side
#'   engine. Defaults to `"Sage"`.
#' @param right_engine A single non-empty character string naming the
#'   right-side engine. Defaults to `"GNU"`.
#'
#' @return A data frame with one row per omitted unit. It contains
#'   `omitted_value`, `remaining_unit_count`, pooled equity loss, eligible
#'   decisions, and ER for both engines, the left-minus-right ER difference,
#'   and `lower_er_engine`. Engine-specific column names are derived from the
#'   engine labels.
#'
#' @examples
#' decisions <- data.frame(
#'   pair_id = rep(c("pair_01", "pair_02"), each = 2),
#'   engine = rep(c("Sage", "GNU"), 2),
#'   equity_lost = c(1, 0.5, 2, 1),
#'   decisions = c(100, 100, 100, 100)
#' )
#'
#' leave_one_out_er(
#'   data = decisions,
#'   unit = "pair_id",
#'   engine = "engine",
#'   equity_lost = "equity_lost",
#'   eligible_decisions = "decisions"
#' )
#'
#' @export
leave_one_out_er <- function(
  data,
  unit,
  engine,
  equity_lost,
  eligible_decisions,
  left_engine = "Sage",
  right_engine = "GNU"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  validate_leave_one_out_arguments(
    unit = unit,
    engine = engine,
    equity_lost = equity_lost,
    eligible_decisions = eligible_decisions,
    left_engine = left_engine,
    right_engine = right_engine
  )

  required_columns <- c(
    unit,
    engine,
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

  if (!is.atomic(data[[unit]])) {
    stop(
      "`",
      unit,
      "` must be an atomic vector.",
      call. = FALSE
    )
  }

  if (anyNA(data[[unit]])) {
    stop(
      "`",
      unit,
      "` must not contain missing values.",
      call. = FALSE
    )
  }

  engine_values <- data[[engine]]

  if (
    !is.character(engine_values) &&
      !is.factor(engine_values)
  ) {
    stop(
      "`",
      engine,
      "` must be a character or factor column.",
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

  engine_values <- as.character(engine_values)

  selected <- !is.na(engine_values) &
    engine_values %in% c(
      left_engine,
      right_engine
    )

  selected_data <- data[
    selected,
    ,
    drop = FALSE
  ]

  selected_data[[engine]] <- engine_values[
    selected
  ]

  if (nrow(selected_data) == 0L) {
    stop(
      "No rows match `left_engine` or `right_engine`.",
      call. = FALSE
    )
  }

  units <- unique(
    selected_data[[unit]]
  )

  if (length(units) < 2L) {
    stop(
      "At least two unique omission units are required.",
      call. = FALSE
    )
  }

  output_names <- leave_one_out_output_names(
    left_engine = left_engine,
    right_engine = right_engine
  )

  unit_count <- length(units)

  remaining_unit_count <- integer(unit_count)
  left_equity_lost <- rep(NA_real_, unit_count)
  left_eligible_decisions <- rep(NA_real_, unit_count)
  left_er <- rep(NA_real_, unit_count)
  right_equity_lost <- rep(NA_real_, unit_count)
  right_eligible_decisions <- rep(NA_real_, unit_count)
  right_er <- rep(NA_real_, unit_count)
  difference <- rep(NA_real_, unit_count)
  lower_er_engine <- rep(NA_character_, unit_count)

  for (index in seq_len(unit_count)) {
    omitted <- units[[index]]

    remaining <- selected_data[
      selected_data[[unit]] != omitted,
      ,
      drop = FALSE
    ]

    remaining_unit_count[[index]] <- length(
      unique(
        remaining[[unit]]
      )
    )

    summarized <- summarize_er(
      data = remaining,
      group_by = engine,
      equity_lost = equity_lost,
      eligible_decisions = eligible_decisions
    )

    left_row <- which(
      as.character(summarized[[engine]]) ==
        left_engine
    )

    right_row <- which(
      as.character(summarized[[engine]]) ==
        right_engine
    )

    if (length(left_row) == 1L) {
      left_equity_lost[[index]] <- summarized$equity_lost[
        left_row
      ]
      left_eligible_decisions[[index]] <-
        summarized$eligible_decisions[
          left_row
        ]
      left_er[[index]] <- summarized$er[
        left_row
      ]
    }

    if (length(right_row) == 1L) {
      right_equity_lost[[index]] <- summarized$equity_lost[
        right_row
      ]
      right_eligible_decisions[[index]] <-
        summarized$eligible_decisions[
          right_row
        ]
      right_er[[index]] <- summarized$er[
        right_row
      ]
    }

    if (
      !is.na(left_er[[index]]) &&
        !is.na(right_er[[index]])
    ) {
      difference[[index]] <- left_er[[index]] -
        right_er[[index]]

      if (left_er[[index]] < right_er[[index]]) {
        lower_er_engine[[index]] <- left_engine
      } else if (
        right_er[[index]] < left_er[[index]]
      ) {
        lower_er_engine[[index]] <- right_engine
      } else {
        lower_er_engine[[index]] <- "Tie"
      }
    }
  }

  output <- data.frame(
    omitted_value = units,
    remaining_unit_count = remaining_unit_count,
    left_equity_lost = left_equity_lost,
    left_eligible_decisions = left_eligible_decisions,
    left_er = left_er,
    right_equity_lost = right_equity_lost,
    right_eligible_decisions = right_eligible_decisions,
    right_er = right_er,
    difference = difference,
    lower_er_engine = lower_er_engine,
    stringsAsFactors = FALSE
  )

  names(output)[
    names(output) == "left_equity_lost"
  ] <- output_names$left_equity_lost

  names(output)[
    names(output) == "left_eligible_decisions"
  ] <- output_names$left_eligible_decisions

  names(output)[
    names(output) == "left_er"
  ] <- output_names$left_er

  names(output)[
    names(output) == "right_equity_lost"
  ] <- output_names$right_equity_lost

  names(output)[
    names(output) == "right_eligible_decisions"
  ] <- output_names$right_eligible_decisions

  names(output)[
    names(output) == "right_er"
  ] <- output_names$right_er

  names(output)[
    names(output) == "difference"
  ] <- output_names$difference

  rownames(output) <- NULL
  output
}


validate_leave_one_out_arguments <- function(
  unit,
  engine,
  equity_lost,
  eligible_decisions,
  left_engine,
  right_engine
) {
  column_arguments <- c(
    "unit",
    "engine",
    "equity_lost",
    "eligible_decisions"
  )

  for (argument in column_arguments) {
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

  if (anyDuplicated(
    c(
      unit,
      engine,
      equity_lost,
      eligible_decisions
    )
  )) {
    stop(
      "Unit, engine, equity-loss, and decision columns must be different.",
      call. = FALSE
    )
  }

  for (
    argument in c(
      "left_engine",
      "right_engine"
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
        "` must be one non-empty engine label.",
        call. = FALSE
      )
    }
  }

  if (identical(left_engine, right_engine)) {
    stop(
      "`left_engine` and `right_engine` must be different.",
      call. = FALSE
    )
  }

  output_names <- leave_one_out_output_names(
    left_engine = left_engine,
    right_engine = right_engine
  )

  if (identical(
    output_names$left_er,
    output_names$right_er
  )) {
    stop(
      "Engine labels produce identical output column names.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


leave_one_out_output_names <- function(
  left_engine,
  right_engine
) {
  left_name <- compare_er_name_fragment(
    left_engine
  )

  right_name <- compare_er_name_fragment(
    right_engine
  )

  list(
    left_equity_lost = paste0(
      left_name,
      "_equity_lost"
    ),
    left_eligible_decisions = paste0(
      left_name,
      "_eligible_decisions"
    ),
    left_er = paste0(
      left_name,
      "_er"
    ),
    right_equity_lost = paste0(
      right_name,
      "_equity_lost"
    ),
    right_eligible_decisions = paste0(
      right_name,
      "_eligible_decisions"
    ),
    right_er = paste0(
      right_name,
      "_er"
    ),
    difference = paste0(
      "er_difference_",
      left_name,
      "_minus_",
      right_name
    )
  )
}
