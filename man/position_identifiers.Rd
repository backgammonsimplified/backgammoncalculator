% Generated for the direct implementation overlay.
\name{position_identifiers}
\alias{position_identifiers}
\alias{position_from_gnuid}
\alias{position_from_xgid}
\alias{gnuid_from_position}
\alias{xgid_from_position}
\alias{gnuid_to_xgid}
\alias{xgid_to_gnuid}
\title{Convert complete GNUID and XGID identifiers through a canonical position}
\usage{
position_from_gnuid(gnuid, match_id = NULL)

position_from_xgid(xgid)

gnuid_from_position(position, allow_lossy = FALSE)

xgid_from_position(position)

gnuid_to_xgid(position_id, match_id = NULL)

xgid_to_gnuid(xgid, allow_lossy = FALSE)
}
\arguments{
\item{gnuid}{One complete GNUID, or a GNU Position ID when \code{match_id} is supplied.}

\item{match_id}{Optional GNU Match ID used with a separate GNU Position ID.}

\item{xgid}{One complete XGID beginning with \code{XGID=}.}

\item{position}{A canonical \code{backgammon_position_state} object returned by \code{position_from_gnuid()} or \code{position_from_xgid()}.}

\item{position_id}{A complete GNUID, or a GNU Position ID when \code{match_id} is supplied. This name preserves the original \code{gnuid_to_xgid()} interface.}

\item{allow_lossy}{Logical. When false, conversion to GNUID fails if an XGID-only factual setting would be discarded. At present this applies when the XGID maximum-cube exponent differs from GNU-derived normalization.}
}
\value{
\code{position_from_gnuid()} and \code{position_from_xgid()} return a canonical \code{backgammon_position_state} list. Encoder and convenience functions return one scalar character identifier.
}
\description{
These functions provide the package boundary for complete GNU Backgammon identifiers and eXtreme Gammon identifiers. Both formats are decoded to the same canonical position representation before any re-encoding occurs.
}
\details{
A complete GNUID is represented as a 14-character GNU Position ID followed by a colon and a 12-character GNU Match ID. For compatibility with existing callers, \code{position_from_gnuid()} and \code{gnuid_to_xgid()} also accept the two GNU parts as separate arguments.

The canonical point indices are fixed physical locations matching XGID checker slots 2 through 25. \code{player_0} always owns lowercase XGID checkers and \code{player_1} always owns uppercase XGID checkers. Stable player identity never changes with source orientation or the player on roll.

Exact source identifiers, source parts, provenance, and normalization notes are retained in the returned canonical object. Re-encoding to the unchanged source format reuses the exact source identifier only after its format and decoded factual state are verified against the current canonical state.
}
\section{Canonical state}{
The canonical object contains stable-player checker locations, bars and borne-off counts; dice owner and turn owner; dice or pending-double action; cube value and owner; score, match length, Crawford and Jacoby state; exact source identifier; conversion provenance; and normalization metadata.
}
\section{Normalization and lossiness}{
GNU Match IDs do not encode the XGID maximum-cube setting. GNUID decoding uses exponent 10 unless the current cube is higher. Encoding an XGID-derived state with a different normalized maximum requires \code{allow_lossy = TRUE}.

XGID does not encode GNU's \code{not_started} versus \code{playing} game-state distinction, so \code{not_started} is factually normalized to \code{playing} after a GNUID to XGID to GNUID conversion. GNU dice are encoded in descending order; die order is normalized rather than treated as a factual change.

The canonical object's normalization metadata reports normalized and lossy fields using one shared internal assessment path. Completed or other terminal games, resignation states, reserved GNU patterns, unsupported XGID actions, and structurally invalid positions fail explicitly.
}
\examples{
state <- position_from_gnuid(
  "4HPwATDgc/ABMA:8IhuACAACAAE"
)
xgid_from_position(state)
gnuid_from_position(state)

gnuid_to_xgid(
  position_id = "4HPwATDgc/ABMA",
  match_id = "8IhuACAACAAE"
)
}
