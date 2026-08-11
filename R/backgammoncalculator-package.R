#' backgammoncalculator: Reusable Backgammon Analysis Tools
#'
#' `backgammoncalculator` provides local GNUID/XGID conversion through a
#' canonical backgammon position state, reusable error-rate and runtime
#' summaries, outcome and distribution helpers, and Backgammon Simplified
#' plotting utilities.
#'
#' @section Start here:
#' For a runnable tour of the main workflows, open
#' `?backgammoncalculator-examples`. The examples are designed to be copied
#' directly into an R session.
#'
#' @section Main workflows:
#' * **Identifiers:** [position_from_gnuid()], [position_from_xgid()],
#'   [gnuid_from_position()], [xgid_from_position()], [gnuid_to_xgid()], and
#'   [xgid_to_gnuid()].
#' * **ER and runtime analysis:** [calculate_er()], [summarize_er()],
#'   [compare_er()], [leave_one_out_er()], and [summarize_runtime()].
#' * **Outcomes and distributions:** [prepare_pair_outcomes()],
#'   [metric_distribution_summary()], and the distribution-axis helpers.
#' * **Plots and styling:** [plot_er_comparison()], [plot_pair_outcomes()],
#'   [plot_density_distributions()], [theme_bs()], and the BS palette/scale
#'   helpers.
#'
#' @seealso [backgammoncalculator-examples]
#' @keywords internal
"_PACKAGE"
