#' Compare ER Between Two Engines
#'
#' Places ER values for two engines side by side within each requested group,
#' calculates the left-minus-right ER difference, and identifies the engine
#' with the lower ER.
#'
#' @param data A data frame containing grouped ER values.
#' @param group_by A character vector naming columns that define each
#'   comparison group. Use `character()` for one overall comparison.
#' @param engine A single character string naming the engine column.
#' @param er A single character string naming the ER column.
#' @param left_engine A single non-empty character string naming the engine
#'   shown on the left side. Defaults to `"Sage"`.
#' @param right_engine A single non-empty character string naming the engine
#'   shown on the right side. Defaults to `"GNU"`.
#'
#' @return A data frame containing the grouping columns, one ER column for
#'   each engine, the left-minus-right ER difference, and
#'   `lower_er_engine`. With the defaults, the metric columns are `sage_er`,
#'   `gnu_er`, and `er_difference_sage_minus_gnu`. `lower_er_engine` is
#'   `"Tie"` when the ER values are equal and `NA_character_` when either
#'   side is absent or missing.
#'
#' @examples
#' grouped <- data.frame(
#'   component = c("checker", "checker", "cube", "cube"),
#'   engine = c("Sage", "GNU", "Sage", "GNU"),
#'   er = c(0.29, 0.18, 9.65, 0.02)
#' )
#'
#' compare_er(
#'   data = grouped,
#'   group_by = "component",
#'   engine = "engine",
#'   er = "er"
#' )
#'
#' @export
compare_er <- function(
  data,
  group_by,
  engine,
  er,
  left_engine = "Sage",
  right_engine = "GNU"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  validate_compare_er_arguments(
    group_by = group_by,
    engine = engine,
    er = er,
    left_engine = left_engine,
    right_engine = right_engine
  )

  required_columns <- c(group_by, engine, er)
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

  engine_values <- data[[engine]]

  if (!is.character(engine_values) && !is.factor(engine_values)) {
    stop(
      "`",
      engine,
      "` must be a character or factor column.",
      call. = FALSE
    )
  }

  validate_er_input(data[[er]], argument = er)

  engine_values <- as.character(engine_values)
  selected <- !is.na(engine_values) &
    engine_values %in% c(left_engine, right_engine)

  selected_data <- data[selected, , drop = FALSE]
  selected_engines <- engine_values[selected]

  output_names <- compare_er_output_names(
    left_engine = left_engine,
    right_engine = right_engine
  )

  if (nrow(selected_data) == 0L) {
    output <- data[FALSE, group_by, drop = FALSE]
    output[[output_names$left_er]] <- numeric()
    output[[output_names$right_er]] <- numeric()
    output[[output_names$difference]] <- numeric()
    output$lower_er_engine <- character()
    rownames(output) <- NULL
    return(output)
  }

  if (length(group_by) == 0L) {
    group_values <- data.frame(.comparison_group = 1L)
  } else {
    group_values <- unique(selected_data[group_by])
    rownames(group_values) <- NULL
  }

  group_count <- nrow(group_values)
  left_er <- rep(NA_real_, group_count)
  right_er <- rep(NA_real_, group_count)
  lower_er_engine <- rep(NA_character_, group_count)

  for (index in seq_len(group_count)) {
    members <- compare_er_group_membership(
      data = selected_data,
      group_by = group_by,
      group_values = group_values,
      index = index
    )

    left_rows <- which(members & selected_engines == left_engine)
    right_rows <- which(members & selected_engines == right_engine)

    if (length(left_rows) > 1L) {
      stop(
        "Duplicate `",
        left_engine,
        "` rows for ",
        compare_er_group_label(group_by, group_values, index),
        ".",
        call. = FALSE
      )
    }

    if (length(right_rows) > 1L) {
      stop(
        "Duplicate `",
        right_engine,
        "` rows for ",
        compare_er_group_label(group_by, group_values, index),
        ".",
        call. = FALSE
      )
    }

    if (length(left_rows) == 1L) {
      left_er[[index]] <- selected_data[[er]][left_rows]
    }

    if (length(right_rows) == 1L) {
      right_er[[index]] <- selected_data[[er]][right_rows]
    }

    if (!is.na(left_er[[index]]) && !is.na(right_er[[index]])) {
      if (left_er[[index]] < right_er[[index]]) {
        lower_er_engine[[index]] <- left_engine
      } else if (right_er[[index]] < left_er[[index]]) {
        lower_er_engine[[index]] <- right_engine
      } else {
        lower_er_engine[[index]] <- "Tie"
      }
    }
  }

  difference <- left_er - right_er

  if (length(group_by) == 0L) {
    output <- data.frame(
      left_er = left_er,
      right_er = right_er,
      difference = difference,
      lower_er_engine = lower_er_engine,
      stringsAsFactors = FALSE
    )
  } else {
    output <- group_values
    output$left_er <- left_er
    output$right_er <- right_er
    output$difference <- difference
    output$lower_er_engine <- lower_er_engine
  }

  names(output)[names(output) == "left_er"] <- output_names$left_er
  names(output)[names(output) == "right_er"] <- output_names$right_er
  names(output)[names(output) == "difference"] <- output_names$difference

  rownames(output) <- NULL
  output
}


validate_compare_er_arguments <- function(
  group_by,
  engine,
  er,
  left_engine,
  right_engine
) {
  if (!is.character(group_by) || anyNA(group_by) || any(!nzchar(group_by))) {
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

  for (argument in c("engine", "er")) {
    value <- get(argument, inherits = FALSE)

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

  if (identical(engine, er)) {
    stop(
      "`engine` and `er` must name different columns.",
      call. = FALSE
    )
  }

  overlapping_columns <- intersect(group_by, c(engine, er))

  if (length(overlapping_columns) > 0L) {
    stop(
      "Engine and ER columns must not also be grouping columns: ",
      paste(overlapping_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  for (argument in c("left_engine", "right_engine")) {
    value <- get(argument, inherits = FALSE)

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

  output_names <- compare_er_output_names(left_engine, right_engine)

  if (identical(output_names$left_er, output_names$right_er)) {
    stop(
      "Engine labels produce identical output column names.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


compare_er_output_names <- function(left_engine, right_engine) {
  left_name <- compare_er_name_fragment(left_engine)
  right_name <- compare_er_name_fragment(right_engine)

  list(
    left_er = paste0(left_name, "_er"),
    right_er = paste0(right_name, "_er"),
    difference = paste0(
      "er_difference_",
      left_name,
      "_minus_",
      right_name
    )
  )
}


compare_er_name_fragment <- function(value) {
  output <- tolower(value)
  output <- gsub("[^a-z0-9]+", "_", output)
  output <- gsub("^_+|_+$", "", output)

  if (!nzchar(output)) {
    output <- "engine"
  }

  output
}


compare_er_group_membership <- function(
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


compare_er_group_label <- function(
  group_by,
  group_values,
  index
) {
  if (length(group_by) == 0L) {
    return("the overall comparison")
  }

  values <- vapply(
    group_by,
    function(column) {
      value <- group_values[[column]][[index]]

      if (is.na(value)) {
        value <- "NA"
      }

      paste0(column, " = ", value)
    },
    character(1)
  )

  paste(values, collapse = ", ")
}
