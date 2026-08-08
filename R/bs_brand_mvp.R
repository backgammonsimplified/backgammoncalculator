#' BS brand snapshot metadata
#'
#' Return the metadata bundled with the generated BS brand-token snapshot.
#' The snapshot is generated outside this package and vendored so analysis
#' code does not depend on a private repository at runtime.
#'
#' @return A named list containing the brand-kit version, source commit,
#'   registry checksum, generation date, and source path.
#' @export
bs_brand_info <- function() {
  bs_brand_metadata
}

#' BS typography tokens
#'
#' Return the semantic typography families bundled with the generated BS
#' brand snapshot. Consumers should use these values instead of defining
#' notebook-local or website-local font stacks.
#'
#' @return A named character vector containing the primary family, CSS stack,
#'   R fallback, and Python fallback.
#' @export
bs_typography <- function() {
  c(
    primary = bs_brand$typography$family$primary,
    css_stack = bs_brand$typography$family$fallback_css,
    r_fallback = bs_brand$typography$family$fallback_r,
    python_fallback = bs_brand$typography$family$fallback_python
  )
}

#' BS engine colours
#'
#' Return the permanent BS colour assignments for named engines. Engine
#' matching is case-insensitive, while returned names preserve the caller's
#' spelling so the vector can be passed directly to a manual ggplot2 scale.
#'
#' @param engines Character vector of engine names. The current accepted
#'   assignments are Sage and GNU.
#'
#' @return A named character vector of colours.
#' @export
bs_engine_palette <- function(
    engines = c("Sage", "GNU")) {
  if (
    !is.character(engines) ||
      length(engines) == 0L ||
      anyNA(engines) ||
      any(!nzchar(engines))
  ) {
    stop(
      "`engines` must be a non-empty character vector without missing values.",
      call. = FALSE
    )
  }

  available <- c(
    sage = bs_engine_colours$sage,
    gnu = bs_engine_colours$gnu
  )

  keys <- tolower(engines)
  unknown <- engines[!keys %in% names(available)]

  if (length(unknown) > 0L) {
    stop(
      "Unknown BS engine colour assignment: ",
      paste(unique(unknown), collapse = ", "),
      ". Accepted engines: Sage, GNU.",
      call. = FALSE
    )
  }

  stats::setNames(
    unname(available[keys]),
    engines
  )
}

#' BS analytical colours
#'
#' Return a compact set of semantic colours used by analytical figures.
#' This is the first-release palette contract, not the complete future BS
#' analytical design system.
#'
#' @return A named character vector.
#' @export
bs_analysis_palette <- function() {
  c(
    overall = bs_analysis_colours$overall,
    difference = bs_analysis_colours$difference,
    secondary = bs_analysis_colours$secondary,
    threshold = bs_analysis_colours$threshold,
    selected = bs_analysis_colours$selected,
    excluded = bs_analysis_colours$excluded,
    tied = bs_brand$status$neutral$surface,
    page = bs_chart_colours$page,
    surface = bs_chart_colours$surface,
    panel = bs_chart_colours$panel,
    text = bs_chart_colours$text$primary,
    text_strong = bs_chart_colours$text$strong,
    text_secondary = bs_chart_colours$text$secondary,
    text_muted = bs_chart_colours$text$muted,
    text_inverse = bs_brand$website$light$text$inverse,
    grid = bs_chart_colours$grid,
    reference = bs_chart_colours$reference
  )
}

#' BS theme for analytical figures
#'
#' Provide a restrained first-release ggplot2 theme using the generated BS
#' chart tokens. More specialized accessibility and publication validation
#' helpers are intentionally deferred until after the first article release.
#'
#' @param base_size Base text size in points.
#' @param base_family Font family. Defaults to the portable R fallback from
#'   the bundled BS brand tokens.
#' @param background Either `"surface"`, `"page"`, or `"transparent"`.
#'
#' @return A ggplot2 theme.
#' @export
theme_bs <- function(
    base_size = 11,
    base_family = unname(bs_typography()[["r_fallback"]]),
    background = c("surface", "page", "transparent")) {
  background <- match.arg(background)
  palette <- bs_analysis_palette()

  background_colour <- switch(
    background,
    surface = unname(palette[["surface"]]),
    page = unname(palette[["page"]]),
    transparent = NA_character_
  )

  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      text = ggplot2::element_text(
        colour = unname(palette[["text"]])
      ),
      plot.title = ggplot2::element_text(
        colour = unname(palette[["text_strong"]]),
        face = "bold",
        size = ggplot2::rel(1.36),
        margin = ggplot2::margin(b = 5)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = unname(palette[["text_secondary"]]),
        size = ggplot2::rel(1),
        margin = ggplot2::margin(b = 12)
      ),
      plot.caption = ggplot2::element_text(
        colour = unname(palette[["text_muted"]]),
        size = ggplot2::rel(0.82),
        hjust = 0
      ),
      axis.title = ggplot2::element_text(
        colour = unname(palette[["text"]]),
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        colour = unname(palette[["text_secondary"]])
      ),
      panel.grid.major = ggplot2::element_line(
        colour = unname(palette[["grid"]]),
        linewidth = 0.45
      ),
      panel.grid.minor = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(
        colour = unname(palette[["text_strong"]]),
        face = "bold"
      ),
      legend.text = ggplot2::element_text(
        colour = unname(palette[["text"]])
      ),
      plot.background = ggplot2::element_rect(
        fill = background_colour,
        colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = background_colour,
        colour = NA
      )
    )
}

#' BS engine colour scale
#'
#' @param ... Passed to [ggplot2::scale_colour_manual()].
#' @param engines Engine names to include in the scale.
#'
#' @return A ggplot2 discrete colour scale.
#' @export
scale_colour_bs_engine <- function(
    ...,
    engines = c("Sage", "GNU")) {
  ggplot2::scale_colour_manual(
    ...,
    values = bs_engine_palette(engines)
  )
}

#' BS engine fill scale
#'
#' @param ... Passed to [ggplot2::scale_fill_manual()].
#' @param engines Engine names to include in the scale.
#'
#' @return A ggplot2 discrete fill scale.
#' @export
scale_fill_bs_engine <- function(
    ...,
    engines = c("Sage", "GNU")) {
  ggplot2::scale_fill_manual(
    ...,
    values = bs_engine_palette(engines)
  )
}
