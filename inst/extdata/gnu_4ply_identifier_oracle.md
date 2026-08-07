# GNU 4-ply identifier oracle fixture

This fixture contains 3,802 unique GNU Position ID and GNU Match ID pairs parsed
from 82 GNU Backgammon 4-ply per-game review reports in:

`sage-vs-gnu-stage1-match-review-artifacts-v1.tar.gz`

Source archive SHA-256:

`40125d667c2faaf0cf24af54078249b74cfc070e355b2a4ee49b78ab93b90f1d`

Repository fixture SHA-256 (LF line endings, exact bytes):

`958040fefd8aa3f923ca3a302c235a58a77fee01b56825d6fc5125f695c13f49`

Historical Windows/CRLF byte representation SHA-256:

`0c6479c0ed6656ca783fe508d6f8b2b18eb3ecb7a8a7dccc18f9891d92aac4d3`

The two hashes differ only because converting the 3,803 LF line endings in the
repository fixture to CRLF changes the raw bytes. Parsing either representation
produces the same 3,802 CSV records. Raw artifact hashes are recorded separately
rather than silently normalizing line endings.

## Source facts used

The expected facts come from GNU Backgammon's own review text:

- GNU Position ID and Match ID;
- printed checker board, bars, and borne-off totals;
- player on roll and rolled dice;
- cube decision and pending-double text;
- cube value and visible cube owner;
- player scores;
- match length;
- Crawford and post-Crawford labels.

GNU 4-ply equities and move rankings are not used by these conversion tests.

GNU's printed point labels are relative to the player on roll. The parser
mirrors printed points when O is on roll so the fixture uses the package's fixed
canonical physical points. O maps to `player_0`; X maps to `player_1`.

## Independent validation

A separate Python standard-library decoder parsed the GNU Position ID and Match
ID bit fields and compared them with the facts extracted from the GNU reports.
All 3,802 unique pairs agreed on:

- all 24 checker points for both players;
- both bars and both borne-off counts;
- dice owner and turn owner;
- dice and pending doubles;
- cube value and owner;
- both scores;
- match length;
- Crawford state.

There were 3,808 report occurrences. Duplicate identifier pairs had no factual
conflicts. The fixture stores one row per unique complete GNUID.

This corpus is an independent GNU-generated regression source for identifier
conversion. It does not replace live GNU CLI acceptance or rendering parity.

## Recomputed coverage

Coverage was recomputed from named CSV columns after the Phase 1 review:

- 3,802 unique complete GNUIDs;
- actor O: 1,889 rows; actor X: 1,913 rows;
- 3,606 rolls, 98 cube decisions, and 98 pending doubles;
- cube owners: 1,593 centered, 1,296 O, 913 X;
- cube values: 1, 2, 4, and 8;
- match length: 7 for all 3,802 rows;
- 474 Crawford rows and 345 post-Crawford-labelled rows;
- 2,586 asymmetric-score rows;
- 651 O-bar rows, 698 X-bar rows, and 6 rows with both bars occupied;
- 645 O-borne-off rows, 655 X-borne-off rows, and 321 rows with both players
  having borne off at least one checker;
- all 21 canonical backgammon rolls are represented: 15 non-double
  combinations and all 6 doubles;
- every non-double roll in the fixture is stored in descending die order.

The earlier phrase describing "21 ordered non-double dice combinations" was
incorrect. There are 15 canonical non-double combinations plus 6 doubles.
