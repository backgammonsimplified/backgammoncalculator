# backgammoncalculator

[![R-CMD-check](https://github.com/backgammonsimplified/backgammoncalculator/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master)](https://github.com/backgammonsimplified/backgammoncalculator/actions/workflows/R-CMD-check.yaml)
[![GitHub release](https://img.shields.io/github/v/release/backgammonsimplified/backgammoncalculator)](https://github.com/backgammonsimplified/backgammoncalculator/releases/latest)
[![GitHub downloads](https://img.shields.io/github/downloads/backgammonsimplified/backgammoncalculator/total)](https://github.com/backgammonsimplified/backgammoncalculator/releases)
[![License](https://img.shields.io/github/license/backgammonsimplified/backgammoncalculator)](https://github.com/backgammonsimplified/backgammoncalculator)

`backgammoncalculator` provides complete GNUID and XGID conversion through a
canonical backgammon position state, together with reusable calculations,
statistical summaries, visual themes, and plot templates for backgammon
analysis.

Tested on Linux, Windows, and macOS with current R, with additional compatibility
validation on R 4.2.2.

## Installation

The package can be installed from the public GitHub repository with:

```r
remotes::install_github(
  "backgammonsimplified/backgammoncalculator"
)
```

A local source checkout can instead be installed with `R CMD INSTALL` or an R
package installation tool of your choice.

## GNUID and XGID conversion

A complete GNUID has the format `<Position ID>:<Match ID>`. Every public
identifier function accepts one scalar position; it returns either one scalar
identifier or one canonical `backgammon_position_state` object.

GNUID to XGID:

```r
backgammoncalculator::gnuid_to_xgid(
  position_id = "4HPwATDgc/ABMA",
  match_id = "8IhuACAACAAE"
)
#> XGID=-b----E-C---eE---c-e----B-:0:0:1:53:1:2:1:3:10
```

XGID to GNUID:

```r
xgid <- "XGID=-b----E-C---eE---c-e----B-:0:0:1:53:1:2:1:3:10"

backgammoncalculator::xgid_to_gnuid(xgid)
#> 4HPwATDgc/ABMA:8IluACAACAAE
```

The GNU result uses canonical descending dice order, so an input roll encoded
as `26` may be emitted as `62` after conversion through XGID. The canonical
state APIs are:

```text
position_from_gnuid()
position_from_xgid()
gnuid_from_position()
xgid_from_position()
```

Canonical states preserve the exact source identifier, its source parts when
applicable, provenance, and normalization metadata. Encoding an unchanged
state back to its verified source format preserves the exact source string.

Some differences between the formats require explicit handling:

- XGID does not distinguish GNU's `not_started` state from `playing`, so a
  GNUID converted through XGID normalizes `not_started` to `playing`.
- GNU Match IDs do not store XGID maximum-cube information. Conversion from an
  XGID with a non-default maximum can therefore be lossy and requires
  `allow_lossy = TRUE`.
- Completed, dropped, resigned, and unsupported terminal states are rejected,
  as are malformed identifiers and structurally unsupported positions.
- GNU dice are emitted in canonical descending order.

Identifier conversion is local and deterministic. It does not run GNU
Backgammon, use `backgammonboard`, call Python or JavaScript, or access a
network service. The package does not evaluate position equity and does not
render backgammon boards.

## API examples

The package includes a runnable API tour covering identifier conversion, ER
analysis, runtime summaries, outcome preparation, distribution helpers, plots,
and Backgammon Simplified styling. After loading the package, open it with:

```r
?`backgammoncalculator-examples`
```

### Canonical position state

Decode an identifier once, inspect or transform the canonical state, and encode
it again through the public boundary:

```r
gnuid <- "4HPwATDgc/ABMA:8IhuACAACAAE"

position <- backgammoncalculator::position_from_gnuid(gnuid)
backgammoncalculator::xgid_from_position(position)
backgammoncalculator::gnuid_from_position(position)
```

### ER analysis

The calculation helpers work with ordinary data frames and explicit column
names:

```r
decisions <- data.frame(
  engine = c("Sage", "Sage", "GNU", "GNU"),
  equity_lost = c(0.40, 0.60, 0.20, 0.30),
  eligible_decisions = c(50, 50, 50, 50)
)

backgammoncalculator::summarize_er(
  data = decisions,
  group_by = "engine",
  equity_lost = "equity_lost",
  eligible_decisions = "eligible_decisions"
)
```

For a direct scalar or vector calculation:

```r
backgammoncalculator::calculate_er(
  equity_lost = c(0.856, 0.515),
  eligible_decisions = c(1471, 1457)
)
```

### Plotting and brand helpers

Plots use the package's public brand tokens rather than project-local colour or
font definitions:

```r
er <- data.frame(
  engine = c("Sage", "GNU"),
  er = c(0.29, 0.18)
)

backgammoncalculator::plot_er_comparison(er)
backgammoncalculator::bs_engine_palette(c("Sage", "GNU"))
backgammoncalculator::bs_typography()
```

The dedicated examples help topic contains complete calls for the remaining
summary, sensitivity, outcome, distribution, theme, and scale functions.

## License and attribution

`backgammoncalculator` is licensed under the GNU Affero General Public License,
version 3. Third-party components and adapted concepts retain their applicable
notices and licenses. See
[`GNUID_XGID_ATTRIBUTION.md`](inst/GNUID_XGID_ATTRIBUTION.md) and
[`THIRD_PARTY_NOTICES.md`](inst/THIRD_PARTY_NOTICES.md).

The license does not grant rights to use the Backgammon Simplified name, logo,
or branding in a way that suggests an unofficial fork is the official project.
