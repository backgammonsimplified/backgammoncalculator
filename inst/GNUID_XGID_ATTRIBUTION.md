# GNUID and XGID conversion attribution

## Upstream reference

The local identifier implementation was informed by the MIT-licensed `bglab` project maintained at `lassehjorthmadsen/bglab`.

Recovered upstream details:

- Package version: `0.0.0.9000`
- Source file: `R/posid2xgid.R`
- Documentation snapshot: built 2025-01-25
- License: MIT
- Exact upstream commit: not available in the recovered documentation snapshot

The recovered source contained the functions `chr2bin()`, `id2bin()`, `posid2xgid()`, `matchid2xgid()`, and `gnuid2xgid()`.

The upstream MIT license text is installed at `licenses/bglab-MIT.txt`.

## Material local changes

The package does not import or depend on `bglab`. Only the identifier concepts and bit-field layout required for this boundary were adapted. The local implementation is a base-R rewrite with the following material changes:

- introduced one canonical `backgammon_position_state` representation;
- implemented GNUID and XGID parsing into that canonical state;
- implemented encoding from canonical state to complete GNUID and XGID;
- preserved exact source identifiers, provenance, normalization metadata, and stable-player identity;
- centralized physical-point, checker-case, score, turn-owner, dice-owner, and cube-owner mappings;
- added strict scalar, alphabet, padding, reserved-bit, structural, and semantic validation;
- added explicit unsupported-state and lossy-conversion conditions;
- added pending-double support and documented maximum-cube normalization;
- removed upstream package, network, engine, renderer, JavaScript, and Python runtime requirements;
- added deterministic regression fixtures and package-local tests.

No upstream documentation is copied into the package help or README.
