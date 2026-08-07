# backgammoncalculator 0.1.0

## Identifier conversion

- Added complete, bidirectional GNUID and XGID conversion through a canonical
  `backgammon_position_state` representation.
- Added `gnuid_to_xgid()` and `xgid_to_gnuid()` convenience conversions and
  `position_from_gnuid()`, `position_from_xgid()`, `gnuid_from_position()`, and
  `xgid_from_position()` canonical-state APIs.
- Preserved exact verified source identifiers when an unchanged canonical
  state is encoded back to its source format.
- Documented normalization and loss handling, including GNU `not_started` to
  XGID `playing` normalization and XGID maximum-cube information that GNU Match
  IDs cannot represent.
- Canonicalized GNU dice in descending order and added pending-double support.
- Added strict validation for malformed identifiers, reserved encodings,
  unsupported actions, terminal states, and structurally invalid positions.
- Added a GNU-derived 3,802-position regression corpus and deterministic
  identifier regression fixtures.
- Added third-party attribution and the applicable MIT license notice for the
  bglab concepts that informed the local base-R implementation.

## Analysis and plotting

- Included error-rate and runtime calculation, comparison, and summary
  utilities.
- Included branded plotting themes, palettes, paired-outcome plots, error-rate
  comparison plots, and statistical distribution plots.
