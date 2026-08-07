#' Plot match wins and mirrored-pair outcomes
#'
#' Create the two-bar results figure used for engine benchmark summaries.
#' The caller supplies already-calculated match and mirrored-pair counts.
#' Study-specific data loading and interpretation remain outside this
#' function.
#'
#' @param sage_match_wins Number of individual matches won by Sage.
#' @param gnu_match_wins Number of individual matches won by GNU.
#' @param sage_pair_sweeps Number of mirrored pairs swept by Sage.
#' @param tied_pairs Number of mirrored pairs split one match each.
#' @param gnu_pair_sweeps Number of mirrored pairs swept by GNU.
#' @param sage_logo A grid grob containing the Sage logo.
#' @param gnu_logo A grid grob containing the GNU logo.
#' @param match_length_text Text used to describe the match length.
#'   Defaults to `"Seven Point"`.
#' @param colours Named character vector with colours for `Sage`, `Tied`,
#'   and `GNU`. `NULL` uses the bundled BMS semantic palette.
#' @param text_colour Main text colour. `NULL` uses the BMS chart token.
#' @param muted_text_colour Secondary text colour. `NULL` uses the BMS chart
#'   token.
#' @param background_colour Plot background colour. `NULL` uses the BMS chart
#'   surface token.
#' @param font_family Font family used by the figure. Defaults to the portable
#'   R sans-serif fallback. Publication scripts may pass a registered BMS font.
#'
#' @return A `ggplot2` plot.
#'
#' @examples
#' sage_logo <- grid::circleGrob()
#' gnu_logo <- grid::circleGrob()
#'
#' plot_pair_outcomes(
#'   sage_match_wins = 7,
#'   gnu_match_wins = 13,
#'   sage_pair_sweeps = 1,
#'   tied_pairs = 5,
#'   gnu_pair_sweeps = 4,
#'   sage_logo = sage_logo,
#'   gnu_logo = gnu_logo
#' )
#'
#' @export
plot_pair_outcomes <- function(
    sage_match_wins,
    gnu_match_wins,
    sage_pair_sweeps,
    tied_pairs,
    gnu_pair_sweeps,
    sage_logo,
    gnu_logo,
    match_length_text = "Seven Point",
    colours = NULL,
    text_colour = NULL,
    muted_text_colour = NULL,
    background_colour = NULL,
    font_family = unname(bms_typography()[["r_fallback"]])) {
  pair_outcome_plot_data <- prepare_pair_outcomes(
    sage_match_wins = sage_match_wins,
    gnu_match_wins = gnu_match_wins,
    sage_pair_sweeps = sage_pair_sweeps,
    tied_pairs = tied_pairs,
    gnu_pair_sweeps = gnu_pair_sweeps
  )

  match_total <- unique(
    pair_outcome_plot_data$total[
      pair_outcome_plot_data$score_type == "matches"
    ]
  )

  pair_total <- unique(
    pair_outcome_plot_data$total[
      pair_outcome_plot_data$score_type == "pairs"
    ]
  )

  if (!grid::is.grob(sage_logo)) {
    stop(
      "`sage_logo` must be a grid grob.",
      call. = FALSE
    )
  }

  if (!grid::is.grob(gnu_logo)) {
    stop(
      "`gnu_logo` must be a grid grob.",
      call. = FALSE
    )
  }

  if (
    length(match_length_text) != 1L ||
      is.na(match_length_text) ||
      !nzchar(match_length_text)
  ) {
    stop(
      "`match_length_text` must be one non-empty string.",
      call. = FALSE
    )
  }

  if (
    !is.character(font_family) ||
      length(font_family) != 1L ||
      is.na(font_family) ||
      !nzchar(font_family)
  ) {
    stop(
      "`font_family` must be one non-empty string.",
      call. = FALSE
    )
  }

  palette <- bms_analysis_palette()

  if (is.null(colours)) {
    engine_colours <- bms_engine_palette(
      c("Sage", "GNU")
    )

    colours <- c(
      Sage = unname(engine_colours[["Sage"]]),
      Tied = unname(palette[["tied"]]),
      GNU = unname(engine_colours[["GNU"]])
    )
  }

  if (is.null(text_colour)) {
    text_colour <- unname(palette[["text"]])
  }

  if (is.null(muted_text_colour)) {
    muted_text_colour <- unname(palette[["text_muted"]])
  }

  if (is.null(background_colour)) {
    background_colour <- unname(palette[["surface"]])
  }

  required_colour_names <- c(
    "Sage",
    "Tied",
    "GNU"
  )

  if (
    is.null(names(colours)) ||
      !all(required_colour_names %in% names(colours))
  ) {
    stop(
      "`colours` must contain named values for Sage, Tied, and GNU.",
      call. = FALSE
    )
  }

  pair_outcome_plot_data$inside_colour <- ifelse(
    pair_outcome_plot_data$outcome_group %in% c("Sage", "GNU"),
    bms_brand$website$light$text$inverse,
    text_colour
  )

  match_labels <- pair_outcome_plot_data[
    pair_outcome_plot_data$score_type == "matches",
    ,
    drop = FALSE
  ]

  pair_labels <- pair_outcome_plot_data[
    pair_outcome_plot_data$score_type == "pairs",
    ,
    drop = FALSE
  ]

  plot <-
    ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = pair_outcome_plot_data,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = row_order - 0.17,
        ymax = row_order + 0.17,
        fill = outcome_group
      ),
      colour = background_colour,
      linewidth = 1.1
    ) +
    ggplot2::geom_text(
      data = pair_outcome_plot_data,
      ggplot2::aes(
        x = xmid,
        y = row_order + 0.035,
        label = count,
        colour = inside_colour
      ),
      family = font_family,
      fontface = "bold",
      size = 7.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = pair_outcome_plot_data,
      ggplot2::aes(
        x = xmid,
        y = row_order - 0.085,
        label = percentage_label,
        colour = inside_colour
      ),
      family = font_family,
      fontface = "bold",
      size = 3.7,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = match_labels,
      ggplot2::aes(
        x = xmid,
        y = row_order + 0.29,
        label = outcome
      ),
      family = font_family,
      colour = text_colour,
      fontface = "bold",
      size = 4.2
    ) +
    ggplot2::geom_text(
      data = pair_labels,
      ggplot2::aes(
        x = xmid,
        y = row_order + 0.35,
        label = outcome
      ),
      family = font_family,
      colour = text_colour,
      fontface = "bold",
      size = 4.1
    ) +
    ggplot2::geom_text(
      data = pair_labels,
      ggplot2::aes(
        x = xmid,
        y = row_order + 0.24,
        label = record
      ),
      family = font_family,
      colour = muted_text_colour,
      fontface = "bold",
      size = 3.2
    ) +
    ggplot2::annotation_custom(
      grob = sage_logo,
      xmin = 0.31,
      xmax = 0.46,
      ymin = 2.78,
      ymax = 3.08
    ) +
    ggplot2::annotate(
      geom = "text",
      x = 0.50,
      y = 2.93,
      label = "vs",
      family = font_family,
      colour = muted_text_colour,
      fontface = "bold",
      size = 4.3
    ) +
    ggplot2::annotation_custom(
      grob = gnu_logo,
      xmin = 0.54,
      xmax = 0.69,
      ymin = 2.78,
      ymax = 3.08
    ) +
    ggplot2::annotate(
      geom = "text",
      x = 0,
      y = 2.62,
      label = paste0(
        "Match Score after ",
        match_total,
        " ",
        match_length_text,
        " Matches"
      ),
      hjust = 0,
      family = font_family,
      colour = text_colour,
      fontface = "bold",
      size = 5.2
    ) +
    ggplot2::annotate(
      geom = "text",
      x = 0,
      y = 1.58,
      label = paste0(
        "Mirrored Dice Match Score after ",
        pair_total,
        " ",
        match_length_text,
        " Match Pairs"
      ),
      hjust = 0,
      family = font_family,
      colour = text_colour,
      fontface = "bold",
      size = 5
    ) +
    ggplot2::scale_fill_manual(
      values = colours[required_colour_names],
      guide = "none"
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_cartesian(
      xlim = c(0, 1),
      ylim = c(0.68, 3.10),
      clip = "off",
      expand = FALSE
    ) +
    ggplot2::theme_void(
      base_size = 13,
      base_family = font_family
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = background_colour,
        colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = background_colour,
        colour = NA
      ),
      plot.margin = ggplot2::margin(
        t = 8,
        r = 18,
        b = 8,
        l = 18
      )
    )

  plot
}

utils::globalVariables(
  c(
    "count",
    "inside_colour",
    "outcome",
    "outcome_group",
    "percentage_label",
    "record",
    "row_order",
    "xmax",
    "xmid",
    "xmin"
  )
)
