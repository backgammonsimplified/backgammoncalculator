#' Plot an ER comparison
#'
#' Create a directly labelled horizontal dot plot comparing ER values across
#' engines. Lower values are better.
#'
#' @param data A data frame.
#' @param engine Name of the engine column.
#' @param er Name of the ER column.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param x_label X-axis label.
#' @param font_family Font family for plot text.
#'
#' @return A ggplot object.
#'
#' @examples
#' er <- data.frame(
#'   engine = c("Sage", "GNU"),
#'   er = c(0.29, 0.18)
#' )
#'
#' plot_er_comparison(er)
#'
#' @export
plot_er_comparison <- function(
    data,
    engine = "engine",
    er = "er",
    title = "Overall ER",
    subtitle = "Lower is better",
    x_label = "ER",
    font_family = unname(bs_typography()[["r_fallback"]])) {
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame.",
      call. = FALSE
    )
  }

  required_columns <- c(
    engine,
    er
  )

  missing_columns <- required_columns[
    !required_columns %in% names(data)
  ]

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Missing required column(s): ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  plot_data <- data[
    ,
    c(engine, er),
    drop = FALSE
  ]

  names(plot_data) <- c(
    "engine",
    "er"
  )

  if (
    anyNA(plot_data$engine) ||
      any(!nzchar(plot_data$engine))
  ) {
    stop(
      "Engine names must be non-missing and non-empty.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(plot_data$er) ||
      anyNA(plot_data$er) ||
      any(!is.finite(plot_data$er)) ||
      any(plot_data$er < 0)
  ) {
    stop(
      "ER values must be finite numeric values greater than or equal to zero.",
      call. = FALSE
    )
  }

  analysis_palette <- bs_analysis_palette()

  text_colour <- unname(
    analysis_palette[["text"]]
  )

  strong_text_colour <- unname(
    analysis_palette[["text_strong"]]
  )

  muted_text_colour <- unname(
    analysis_palette[["text_muted"]]
  )

  grid_colour <- unname(
    analysis_palette[["grid"]]
  )

  surface_colour <- unname(
    analysis_palette[["surface"]]
  )

  plot_data$engine <- as.character(
    plot_data$engine
  )

  engine_palette <- bs_engine_palette(
    unique(plot_data$engine)
  )

  plot_data$colour <- unname(
    engine_palette[plot_data$engine]
  )

  plot_data$label <- sprintf(
    "%.3f",
    plot_data$er
  )

  plot_data <- plot_data[
    order(plot_data$er, decreasing = TRUE),
    ,
    drop = FALSE
  ]

  plot_data$engine <- factor(
    plot_data$engine,
    levels = plot_data$engine
  )

  x_max <- max(plot_data$er)

  label_padding <- max(
    0.04 * x_max,
    0.03
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = er,
      y = engine
    )
  ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = er,
        y = engine,
        yend = engine
      ),
      linewidth = 1.2,
      colour = grid_colour
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        colour = engine
      ),
      size = 4.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = er + label_padding,
        label = label
      ),
      hjust = 0,
      family = font_family,
      fontface = "bold",
      size = 4.3,
      colour = strong_text_colour
    ) +
    ggplot2::scale_colour_manual(
      values = engine_palette,
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(
        0,
        x_max + 3 * label_padding
      ),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = NULL
    ) +
    ggplot2::theme_minimal(
      base_family = font_family,
      base_size = 13
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        colour = strong_text_colour,
        face = "bold",
        size = 16
      ),
      plot.subtitle = ggplot2::element_text(
        colour = muted_text_colour,
        size = 11,
        margin = ggplot2::margin(
          b = 12
        )
      ),
      axis.title.x = ggplot2::element_text(
        colour = text_colour,
        face = "bold",
        margin = ggplot2::margin(
          t = 10
        )
      ),
      axis.text.x = ggplot2::element_text(
        colour = text_colour
      ),
      axis.text.y = ggplot2::element_text(
        colour = strong_text_colour,
        face = "bold"
      ),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        colour = grid_colour,
        linewidth = 0.5
      ),
      panel.background = ggplot2::element_rect(
        fill = surface_colour,
        colour = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = surface_colour,
        colour = NA
      )
    )
}
