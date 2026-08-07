utils::globalVariables(
  c(
    "xmin",
    "xmax",
    "density",
    "fill_value",
    "x",
    "xend",
    "y",
    "yend",
    "label",
    "colour",
    "fontface",
    "vjust",
    "tooltip",
    "data_id",
    "row",
    "value"
  )
)


#' Summarize a grouped numeric distribution
#'
#' @param data A data frame.
#' @param value_column Name of the numeric value column.
#' @param group_column Name of the grouping column.
#' @param group_levels Optional character vector fixing group order.
#'
#' @return A data frame with count, minimum, mean, median, maximum, and
#'   standard deviation for each group.
#' @export
metric_distribution_summary <- function(
    data,
    value_column,
    group_column,
    group_levels = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  required_columns <- c(value_column, group_column)
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Missing distribution column(s): ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  values <- data[[value_column]]

  if (
    length(values) == 0L ||
      anyNA(values) ||
      any(!is.finite(values)) ||
      any(values < 0)
  ) {
    stop(
      paste0(
        "`",
        value_column,
        "` must contain finite, non-negative observations."
      ),
      call. = FALSE
    )
  }

  observed_groups <- as.character(data[[group_column]])

  if (is.null(group_levels)) {
    group_levels <- unique(observed_groups)
  }

  if (
    length(group_levels) == 0L ||
      anyNA(group_levels) ||
      any(!nzchar(group_levels))
  ) {
    stop("`group_levels` must contain non-empty group names.", call. = FALSE)
  }

  missing_groups <- setdiff(group_levels, unique(observed_groups))

  if (length(missing_groups) > 0L) {
    stop(
      paste0(
        "Missing group value(s): ",
        paste(missing_groups, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  result <- do.call(
    rbind,
    lapply(
      group_levels,
      function(group_name) {
        group_values <- values[observed_groups == group_name]

        data.frame(
          group = group_name,
          n = length(group_values),
          minimum = min(group_values),
          mean = mean(group_values),
          median = stats::median(group_values),
          maximum = max(group_values),
          sd = stats::sd(group_values),
          stringsAsFactors = FALSE
        )
      }
    )
  )

  result$group <- factor(
    result$group,
    levels = group_levels
  )

  result
}


.nice_distribution_step <- function(raw_step) {
  if (
    length(raw_step) != 1L ||
      is.na(raw_step) ||
      !is.finite(raw_step) ||
      raw_step <= 0
  ) {
    stop("`raw_step` must be one positive finite number.", call. = FALSE)
  }

  magnitude <- 10 ^ floor(log10(raw_step))
  normalized <- raw_step / magnitude
  preferred_multipliers <- c(1, 1.25, 2, 2.5, 5, 10)
  available <- preferred_multipliers[preferred_multipliers >= normalized]

  if (length(available) == 0L) {
    return(10 * magnitude)
  }

  available[[1L]] * magnitude
}


#' Calculate readable limits for a non-negative distribution
#'
#' The upper limit is padded beyond the observed maximum and then rounded up
#' to a readable value. It is never allowed to stop on the maximum itself.
#'
#' @param values Numeric observations.
#' @param lower Lower axis limit.
#' @param padding_fraction Fraction of the observed span added above the
#'   maximum before rounding.
#' @param round_to Optional explicit rounding interval.
#' @param minimum_upper Optional minimum upper limit.
#' @param target_intervals Approximate number of intervals used to choose a
#'   readable rounding step.
#'
#' @return A numeric vector of length two.
#' @export
nice_distribution_limits <- function(
    values,
    lower = 0,
    padding_fraction = 0.10,
    round_to = NULL,
    minimum_upper = NULL,
    target_intervals = 12L) {
  if (
    length(values) == 0L ||
      anyNA(values) ||
      any(!is.finite(values))
  ) {
    stop("`values` must contain finite observations.", call. = FALSE)
  }

  target_intervals <- as.integer(target_intervals)

  if (
    length(lower) != 1L ||
      !is.finite(lower) ||
      length(padding_fraction) != 1L ||
      !is.finite(padding_fraction) ||
      padding_fraction < 0 ||
      length(target_intervals) != 1L ||
      is.na(target_intervals) ||
      target_intervals < 2L
  ) {
    stop("Invalid distribution-limit settings.", call. = FALSE)
  }

  observed_upper <- max(values)

  if (observed_upper < lower) {
    stop("The observed maximum is below `lower`.", call. = FALSE)
  }

  observed_span <- observed_upper - lower

  if (!is.finite(observed_span) || observed_span <= 0) {
    observed_span <- max(abs(observed_upper), 1)
  }

  padded_upper <- observed_upper + observed_span * padding_fraction

  if (!is.null(round_to)) {
    if (
      length(round_to) != 1L ||
        !is.finite(round_to) ||
        round_to <= 0
    ) {
      stop("`round_to` must be NULL or one positive number.", call. = FALSE)
    }

    upper <- ceiling(padded_upper / round_to) * round_to
  } else {
    raw_step <- (padded_upper - lower) / target_intervals
    nice_step <- .nice_distribution_step(raw_step)
    upper <- ceiling(padded_upper / nice_step) * nice_step

    tolerance <- sqrt(.Machine$double.eps) * max(1, abs(observed_upper))

    if (upper <= observed_upper + tolerance) {
      upper <- upper + nice_step
    }
  }

  if (!is.null(minimum_upper)) {
    if (
      length(minimum_upper) != 1L ||
        !is.finite(minimum_upper)
    ) {
      stop("`minimum_upper` must be NULL or finite.", call. = FALSE)
    }

    upper <- max(upper, minimum_upper)
  }

  if (upper <= lower) {
    stop("The upper limit must exceed the lower limit.", call. = FALSE)
  }

  c(lower, upper)
}


#' Generate readable breaks for a linear distribution axis
#'
#' @param limits Two increasing finite axis limits.
#' @param n Approximate number of breaks.
#'
#' @return A numeric vector of axis breaks.
#' @export
linear_distribution_breaks <- function(
    limits,
    n = 8L) {
  if (
    length(limits) != 2L ||
      anyNA(limits) ||
      any(!is.finite(limits)) ||
      limits[[2L]] <= limits[[1L]]
  ) {
    stop("`limits` must be two increasing finite values.", call. = FALSE)
  }

  n <- as.integer(n)

  if (length(n) != 1L || is.na(n) || n < 2L) {
    stop("`n` must be an integer of at least 2.", call. = FALSE)
  }

  breaks <- pretty(limits, n = n)
  tolerance <- sqrt(.Machine$double.eps)

  breaks <- breaks[
    is.finite(breaks) &
      breaks >= limits[[1L]] - tolerance &
      breaks <= limits[[2L]] + tolerance
  ]

  sort(unique(c(limits, breaks)))
}


#' Generate readable breaks for a pseudo-log distribution axis
#'
#' Breaks use conventional 1, 2, and 5 multiples of powers of ten rather than
#' inverse-transforming arbitrary pretty values.
#'
#' @param limits Two increasing non-negative axis limits.
#' @param minimum_positive Smallest positive break to consider.
#' @param multipliers Positive multipliers applied to powers of ten.
#' @param max_breaks Maximum number of returned breaks.
#'
#' @return A numeric vector of axis breaks.
#' @export
pseudo_log_distribution_breaks <- function(
    limits,
    minimum_positive = 0.01,
    multipliers = c(1, 2, 5),
    max_breaks = 12L) {
  if (
    length(limits) != 2L ||
      anyNA(limits) ||
      any(!is.finite(limits)) ||
      limits[[1L]] < 0 ||
      limits[[2L]] <= limits[[1L]] ||
      length(minimum_positive) != 1L ||
      !is.finite(minimum_positive) ||
      minimum_positive <= 0 ||
      length(multipliers) == 0L ||
      anyNA(multipliers) ||
      any(!is.finite(multipliers)) ||
      any(multipliers <= 0)
  ) {
    stop("Invalid pseudo-log break settings.", call. = FALSE)
  }

  max_breaks <- as.integer(max_breaks)

  if (length(max_breaks) != 1L || is.na(max_breaks) || max_breaks < 3L) {
    stop("`max_breaks` must be an integer of at least 3.", call. = FALSE)
  }

  first_exponent <- floor(log10(minimum_positive))
  last_exponent <- ceiling(log10(limits[[2L]]))
  powers <- 10 ^ seq(first_exponent, last_exponent)
  candidates <- sort(unique(as.vector(outer(multipliers, powers))))
  candidates <- candidates[
    candidates >= minimum_positive &
      candidates <= limits[[2L]]
  ]

  breaks <- sort(unique(c(0, candidates)))

  if (length(breaks) > max_breaks) {
    sparse_candidates <- sort(unique(as.vector(outer(c(1, 5), powers))))
    sparse_candidates <- sparse_candidates[
      sparse_candidates >= minimum_positive &
        sparse_candidates <= limits[[2L]]
    ]
    breaks <- sort(unique(c(0, sparse_candidates)))
  }

  if (length(breaks) > max_breaks) {
    positive_breaks <- breaks[breaks > 0]
    transformed <- log10(positive_breaks)
    targets <- seq(
      min(transformed),
      max(transformed),
      length.out = max_breaks - 1L
    )
    selected <- unique(vapply(
      targets,
      function(target) {
        positive_breaks[[which.min(abs(transformed - target))]]
      },
      numeric(1)
    ))
    breaks <- sort(unique(c(0, selected)))
  }

  breaks
}


#' Format pseudo-log axis ticks
#'
#' @param x Numeric tick values.
#'
#' @return A character vector.
#' @export
format_pseudo_log_tick <- function(x) {
  tolerance <- sqrt(.Machine$double.eps)

  vapply(
    x,
    function(value) {
      if (abs(value) <= tolerance) {
        return("0")
      }

      if (abs(value) < 1) {
        return(sprintf("%.3f", value))
      }

      if (abs(value - round(value)) <= tolerance * max(1, abs(value))) {
        return(sprintf("%.0f", value))
      }

      sprintf("%.2f", value)
    },
    character(1)
  )
}


.distribution_scale <- function(
    values,
    x_scale,
    lower,
    padding_fraction,
    target_intervals,
    pseudo_log_sigma,
    pseudo_log_base,
    minimum_positive,
    max_breaks,
    linear_breaks) {
  x_scale <- match.arg(x_scale, c("pseudo_log", "linear"))

  limits <- nice_distribution_limits(
    values = values,
    lower = lower,
    padding_fraction = padding_fraction,
    target_intervals = target_intervals
  )

  if (identical(x_scale, "pseudo_log")) {
    transformation <- scales::pseudo_log_trans(
      sigma = pseudo_log_sigma,
      base = pseudo_log_base
    )
    breaks <- pseudo_log_distribution_breaks(
      limits = limits,
      minimum_positive = minimum_positive,
      max_breaks = max_breaks
    )
  } else {
    transformation <- scales::new_transform(
      name = "distribution_identity",
      transform = identity,
      inverse = identity,
      domain = c(-Inf, Inf)
    )
    breaks <- linear_distribution_breaks(
      limits = limits,
      n = linear_breaks
    )
  }

  list(
    name = x_scale,
    transformation = transformation,
    limits = limits,
    breaks = breaks
  )
}


.density_geometry <- function(
    values,
    transformation,
    limits,
    density_points,
    density_adjust) {
  density_points <- as.integer(density_points)

  if (
    length(values) < 2L ||
      anyNA(values) ||
      any(!is.finite(values)) ||
      length(density_points) != 1L ||
      is.na(density_points) ||
      density_points < 128L ||
      length(density_adjust) != 1L ||
      !is.finite(density_adjust) ||
      density_adjust <= 0
  ) {
    stop("Invalid density inputs or settings.", call. = FALSE)
  }

  transformed_values <- transformation$transform(values)
  transformed_limits <- transformation$transform(limits)

  density_object <- stats::density(
    transformed_values,
    n = density_points,
    adjust = density_adjust,
    from = transformed_limits[[1L]],
    to = transformed_limits[[2L]]
  )

  data.frame(
    x_transformed = density_object$x,
    x = transformation$inverse(density_object$x),
    density = density_object$y,
    stringsAsFactors = FALSE
  )
}


.density_height_at <- function(
    density_geometry,
    x_value,
    transformation) {
  stats::approx(
    x = density_geometry$x_transformed,
    y = density_geometry$density,
    xout = transformation$transform(x_value),
    rule = 2
  )$y
}


.resolve_named_setting <- function(
    setting,
    group_name,
    argument_name) {
  if (length(setting) == 1L) {
    return(unname(setting[[1L]]))
  }

  if (!is.null(names(setting)) && group_name %in% names(setting)) {
    return(unname(setting[[group_name]]))
  }

  stop(
    paste0(
      "`",
      argument_name,
      "` must be scalar or named for ",
      group_name,
      "."
    ),
    call. = FALSE
  )
}


.validate_plot_colours <- function(group_colours, group_levels, colours) {
  if (
    is.null(names(group_colours)) ||
      !all(group_levels %in% names(group_colours))
  ) {
    stop(
      "`group_colours` must be named for every group.",
      call. = FALSE
    )
  }

  required_colours <- c(
    "surface",
    "text",
    "text_strong",
    "text_muted",
    "grid",
    "mean",
    "median"
  )

  if (
    is.null(names(colours)) ||
      !all(required_colours %in% names(colours))
  ) {
    stop(
      paste0(
        "`colours` must contain: ",
        paste(required_colours, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.distribution_theme <- function(font_family, colours) {
  ggplot2::theme_minimal(
    base_family = font_family,
    base_size = 12
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        colour = unname(colours[["text_strong"]]),
        face = "bold",
        size = 17
      ),
      plot.subtitle = ggplot2::element_text(
        colour = unname(colours[["text_muted"]]),
        size = 11,
        margin = ggplot2::margin(b = 12)
      ),
      axis.title = ggplot2::element_text(
        colour = unname(colours[["text"]]),
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        colour = unname(colours[["text"]])
      ),
      axis.text.x = ggplot2::element_text(size = 9),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        colour = unname(colours[["grid"]]),
        linewidth = 0.4
      ),
      panel.background = ggplot2::element_rect(
        fill = unname(colours[["surface"]]),
        colour = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = unname(colours[["surface"]]),
        colour = NA
      )
    )
}


.summary_panel <- function(
    stats_row,
    value_formatter,
    colours,
    font_family,
    count_label) {
  summary_data <- data.frame(
    row = 6:1,
    label = c(
      count_label,
      "Mean",
      "Median",
      "Minimum",
      "Maximum",
      "Standard deviation"
    ),
    value = c(
      as.character(stats_row$n),
      value_formatter(stats_row$mean),
      value_formatter(stats_row$median),
      value_formatter(stats_row$minimum),
      value_formatter(stats_row$maximum),
      value_formatter(stats_row$sd)
    ),
    colour = c(
      colours[["text_strong"]],
      colours[["mean"]],
      colours[["median"]],
      colours[["text_strong"]],
      colours[["text_strong"]],
      colours[["text_strong"]]
    ),
    fontface = c(
      "bold",
      "bold",
      "bold",
      "plain",
      "plain",
      "plain"
    ),
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot(summary_data) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = 0,
        y = row,
        label = label,
        colour = colour,
        fontface = fontface
      ),
      hjust = 0,
      family = font_family,
      size = 3.4,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = 1,
        y = row,
        label = value,
        colour = colour,
        fontface = fontface
      ),
      hjust = 1,
      family = font_family,
      size = 3.4,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0.5, 6.5)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = "Summary",
      subtitle = " "
    ) +
    ggplot2::theme_void(base_family = font_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        colour = unname(colours[["text_strong"]]),
        face = "bold",
        size = 17
      ),
      plot.subtitle = ggplot2::element_text(
        colour = unname(colours[["text_muted"]]),
        size = 11,
        margin = ggplot2::margin(b = 12)
      ),
      panel.background = ggplot2::element_rect(
        fill = unname(colours[["surface"]]),
        colour = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = unname(colours[["surface"]]),
        colour = NA
      ),
      plot.margin = ggplot2::margin(t = 5.5, r = 8, b = 5.5, l = 8)
    )
}


.distribution_row <- function(
    data,
    value_column,
    group_column,
    group_name,
    group_levels,
    group_colours,
    scale_spec,
    value_formatter,
    axis_formatter,
    colours,
    font_family,
    panel_title,
    panel_subtitle,
    x_label,
    density_points,
    density_adjust,
    fill_alpha,
    outline_alpha,
    outline_width,
    count_label,
    stats_width) {
  summary_data <- metric_distribution_summary(
    data = data,
    value_column = value_column,
    group_column = group_column,
    group_levels = group_levels
  )

  group_rows <- as.character(data[[group_column]]) == group_name
  values <- data[group_rows, value_column]
  stats_row <- summary_data[
    as.character(summary_data$group) == group_name,
    ,
    drop = FALSE
  ]

  density_geometry <- .density_geometry(
    values = values,
    transformation = scale_spec$transformation,
    limits = scale_spec$limits,
    density_points = density_points,
    density_adjust = density_adjust
  )

  marker_data <- data.frame(
    x = c(stats_row$mean, stats_row$median),
    xend = c(stats_row$mean, stats_row$median),
    y = c(0, 0),
    yend = c(
      .density_height_at(
        density_geometry,
        stats_row$mean,
        scale_spec$transformation
      ),
      .density_height_at(
        density_geometry,
        stats_row$median,
        scale_spec$transformation
      )
    ),
    colour = c(
      unname(colours[["mean"]]),
      unname(colours[["median"]])
    ),
    tooltip = c(
      paste0("Mean: ", value_formatter(stats_row$mean)),
      paste0("Median: ", value_formatter(stats_row$median))
    ),
    data_id = paste(group_name, c("mean", "median"), sep = "--"),
    stringsAsFactors = FALSE
  )

  group_colour <- unname(group_colours[[group_name]])

  density_plot <- ggplot2::ggplot(
    density_geometry,
    ggplot2::aes(x = x, y = density)
  ) +
    ggplot2::geom_area(
      fill = group_colour,
      alpha = fill_alpha,
      colour = NA
    ) +
    ggplot2::geom_line(
      colour = group_colour,
      alpha = outline_alpha,
      linewidth = outline_width,
      lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = marker_data,
      ggplot2::aes(
        x = x,
        xend = xend,
        y = y,
        yend = yend
      ),
      inherit.aes = FALSE,
      colour = "#FFFFFF",
      linewidth = 3.0,
      linetype = "solid",
      lineend = "round",
      show.legend = FALSE
    ) +
    ggiraph::geom_segment_interactive(
      data = marker_data,
      ggplot2::aes(
        x = x,
        xend = xend,
        y = y,
        yend = yend,
        colour = colour,
        tooltip = tooltip,
        data_id = data_id
      ),
      inherit.aes = FALSE,
      linewidth = 1.6,
      linetype = "solid",
      lineend = "round",
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(
      transform = scale_spec$transformation,
      limits = scale_spec$limits,
      breaks = scale_spec$breaks,
      labels = axis_formatter,
      expand = ggplot2::expansion(mult = c(0.01, 0.03))
    ) +
    ggplot2::scale_y_continuous(
      breaks = NULL,
      labels = NULL,
      expand = ggplot2::expansion(mult = c(0, 0.12))
    ) +
    ggplot2::labs(
      title = panel_title,
      subtitle = panel_subtitle,
      x = x_label,
      y = "Relative concentration of observations"
    ) +
    .distribution_theme(font_family, colours)

  stats_plot <- .summary_panel(
    stats_row = stats_row,
    value_formatter = value_formatter,
    colours = colours,
    font_family = font_family,
    count_label = count_label
  )

  density_plot + stats_plot +
    patchwork::plot_layout(
      widths = c(1 - stats_width, stats_width)
    )
}


#' Plot stacked density distributions
#'
#' The horizontal scale is selected directly with `x_scale`. The function
#' calculates its own readable limits and breaks, draws one constant fill and
#' outline per group, and places summary statistics in a dedicated panel.
#' Mean and median markers are solid, use equal white underlays, and expose
#' exact values through ggiraph tooltips.
#'
#' @param data A data frame.
#' @param value_column Name of the non-negative numeric column.
#' @param group_column Name of the grouping column.
#' @param group_levels Character vector fixing panel order.
#' @param group_colours Named character vector with one colour per group.
#' @param value_formatter Function used for summary and marker values.
#' @param colours Named list or vector containing surface, text, text_strong,
#'   text_muted, grid, mean, and median colours.
#' @param x_scale Either `"pseudo_log"` or `"linear"`.
#' @param axis_formatter Function used for axis labels.
#' @param group_labels Optional named character vector of display labels.
#' @param font_family Font family passed to ggplot2.
#' @param panel_titles Optional named character vector of complete panel titles.
#' @param title_suffix Suffix used when `panel_titles` is NULL.
#' @param panel_subtitle Scalar or named character vector of panel subtitles.
#' @param x_label X-axis title.
#' @param figure_title Overall figure title.
#' @param figure_subtitle Overall figure subtitle.
#' @param lower Lower axis limit.
#' @param padding_fraction Fractional padding beyond the observed maximum.
#' @param target_intervals Approximate number of intervals used for limits.
#' @param pseudo_log_sigma Linear region around zero for pseudo-log scaling.
#' @param pseudo_log_base Logarithm base for pseudo-log scaling.
#' @param minimum_positive Smallest positive pseudo-log break.
#' @param max_breaks Maximum number of pseudo-log breaks.
#' @param linear_breaks Approximate number of linear breaks.
#' @param density_points Number of points used for the density calculation.
#' @param density_adjust Bandwidth adjustment passed to `stats::density()`.
#' @param fill_alpha Constant density-fill alpha.
#' @param outline_alpha Density-outline alpha.
#' @param outline_width Density-outline width.
#' @param count_label Scalar or named label used for observation count.
#' @param stats_width Fraction of each row reserved for summary statistics.
#'
#' @return A patchwork plot object.
#' @export
plot_density_distributions <- function(
    data,
    value_column,
    group_column,
    group_levels,
    group_colours,
    value_formatter,
    colours,
    x_scale = c("pseudo_log", "linear"),
    axis_formatter = value_formatter,
    group_labels = NULL,
    font_family = "sans",
    panel_titles = NULL,
    title_suffix = "Distribution",
    panel_subtitle = "",
    x_label = NULL,
    figure_title = NULL,
    figure_subtitle = NULL,
    lower = 0,
    padding_fraction = 0.15,
    target_intervals = 12L,
    pseudo_log_sigma = 0.001,
    pseudo_log_base = 10,
    minimum_positive = 0.01,
    max_breaks = 12L,
    linear_breaks = 8L,
    density_points = 1024L,
    density_adjust = 0.80,
    fill_alpha = 0.32,
    outline_alpha = 0.95,
    outline_width = 1.1,
    count_label = "Observations",
    stats_width = 0.23) {
  x_scale <- match.arg(x_scale)

  metric_distribution_summary(
    data = data,
    value_column = value_column,
    group_column = group_column,
    group_levels = group_levels
  )

  .validate_plot_colours(group_colours, group_levels, colours)

  if (!is.function(value_formatter) || !is.function(axis_formatter)) {
    stop("Formatters must be functions.", call. = FALSE)
  }

  if (
    length(stats_width) != 1L ||
      !is.finite(stats_width) ||
      stats_width <= 0.15 ||
      stats_width >= 0.40
  ) {
    stop("`stats_width` must be between 0.15 and 0.40.", call. = FALSE)
  }

  if (is.null(group_labels)) {
    group_labels <- stats::setNames(group_levels, group_levels)
  }

  if (
    is.null(names(group_labels)) ||
      !all(group_levels %in% names(group_labels))
  ) {
    stop(
      "`group_labels` must be named for every group.",
      call. = FALSE
    )
  }

  scale_spec <- .distribution_scale(
    values = data[[value_column]],
    x_scale = x_scale,
    lower = lower,
    padding_fraction = padding_fraction,
    target_intervals = target_intervals,
    pseudo_log_sigma = pseudo_log_sigma,
    pseudo_log_base = pseudo_log_base,
    minimum_positive = minimum_positive,
    max_breaks = max_breaks,
    linear_breaks = linear_breaks
  )

  rows <- lapply(
    group_levels,
    function(group_name) {
      group_label <- unname(group_labels[[group_name]])
      panel_title <- if (is.null(panel_titles)) {
        paste(group_label, title_suffix)
      } else {
        .resolve_named_setting(
          panel_titles,
          group_name,
          "panel_titles"
        )
      }

      .distribution_row(
        data = data,
        value_column = value_column,
        group_column = group_column,
        group_name = group_name,
        group_levels = group_levels,
        group_colours = group_colours,
        scale_spec = scale_spec,
        value_formatter = value_formatter,
        axis_formatter = axis_formatter,
        colours = colours,
        font_family = font_family,
        panel_title = panel_title,
        panel_subtitle = .resolve_named_setting(
          panel_subtitle,
          group_name,
          "panel_subtitle"
        ),
        x_label = x_label,
        density_points = density_points,
        density_adjust = density_adjust,
        fill_alpha = fill_alpha,
        outline_alpha = outline_alpha,
        outline_width = outline_width,
        count_label = .resolve_named_setting(
          count_label,
          group_name,
          "count_label"
        ),
        stats_width = stats_width
      )
    }
  )

  patchwork::wrap_plots(rows, ncol = 1) +
    patchwork::plot_annotation(
      title = figure_title,
      subtitle = figure_subtitle,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          family = font_family,
          face = "bold",
          colour = unname(colours[["text_strong"]]),
          size = 19
        ),
        plot.subtitle = ggplot2::element_text(
          family = font_family,
          colour = unname(colours[["text_muted"]]),
          size = 12
        )
      )
    )
}
