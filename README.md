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

## License and attribution

`backgammoncalculator` is licensed under the GNU Affero General Public License,
version 3. Third-party components and adapted concepts retain their applicable
notices and licenses. See
[`GNUID_XGID_ATTRIBUTION.md`](inst/GNUID_XGID_ATTRIBUTION.md) and
[`THIRD_PARTY_NOTICES.md`](inst/THIRD_PARTY_NOTICES.md).

The license does not grant rights to use the Backgammon Made Simple name, logo,
or branding in a way that suggests an unofficial fork is the official project.
