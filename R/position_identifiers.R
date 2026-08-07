.gnu_base64_alphabet <- c(
  LETTERS,
  letters,
  as.character(0:9),
  "+",
  "/"
)

.position_state_version <- "1.0"
.xgid_default_max_cube_exponent <- 10L

.identifier_error_classes <- c(
  "backgammoncalculator_invalid_complete_gnuid",
  "backgammoncalculator_invalid_gnu_position_id",
  "backgammoncalculator_invalid_gnu_match_id",
  "backgammoncalculator_invalid_xgid",
  "backgammoncalculator_invalid_position_state",
  "backgammoncalculator_unsupported_identifier_state",
  "backgammoncalculator_identifier_conversion_error"
)

#' Convert complete GNUID and XGID identifiers through a canonical position
#'
#' These functions provide the package boundary for complete GNU Backgammon
#' identifiers and eXtreme Gammon identifiers. Both formats are decoded to the
#' same canonical position representation before any re-encoding occurs.
#'
#' A complete GNUID is represented as a 14-character GNU Position ID followed
#' by a colon and a 12-character GNU Match ID. For compatibility with existing
#' callers, [position_from_gnuid()] and [gnuid_to_xgid()] also accept the two
#' GNU parts as separate arguments.
#'
#' The canonical point indices are fixed physical locations matching XGID
#' checker slots 2 through 25. `player_0` always owns lowercase XGID checkers
#' and `player_1` always owns uppercase XGID checkers. Stable player identity
#' never changes with source orientation or the player on roll.
#'
#' Exact source identifiers, source parts, provenance, and normalization notes
#' are retained in the returned canonical object. Re-encoding to the unchanged
#' source format reuses the exact source identifier only after its format and
#' decoded factual state are verified against the current canonical state.
#'
#' @param gnuid One complete GNUID, or a GNU Position ID when `match_id` is
#'   supplied.
#' @param match_id Optional GNU Match ID used with a separate GNU Position ID.
#' @param xgid One complete XGID beginning with `XGID=`.
#' @param position A canonical `backgammon_position_state` object returned by
#'   [position_from_gnuid()] or [position_from_xgid()].
#' @param position_id A complete GNUID, or a GNU Position ID when `match_id` is
#'   supplied. This name preserves the original `gnuid_to_xgid()` interface.
#' @param allow_lossy Logical. When `FALSE`, conversion to GNUID fails if an
#'   XGID-only factual setting would be discarded. At present this applies when
#'   the XGID maximum-cube exponent differs from GNU-derived normalization.
#'
#' @return `position_from_gnuid()` and `position_from_xgid()` return a canonical
#'   `backgammon_position_state` list. Encoder and convenience functions return
#'   one scalar character identifier.
#'
#' @section Canonical state:
#' The canonical object contains stable-player checker locations, bars and
#' borne-off counts; dice owner and turn owner; dice or pending-double action;
#' cube value and owner; score, match length, Crawford and Jacoby state; exact
#' source identifier; conversion provenance; and normalization metadata.
#'
#' @section Normalization and lossiness:
#' GNU Match IDs do not encode the XGID maximum-cube setting. GNUID decoding
#' uses exponent 10 unless the current cube is higher. Encoding an XGID-derived
#' state with a different normalized maximum requires `allow_lossy = TRUE`.
#' XGID does not encode GNU's `not_started` versus `playing` game-state
#' distinction, so `not_started` is factually normalized to `playing` after a
#' GNUID -> XGID -> GNUID conversion. GNU dice are encoded in descending order;
#' die order is therefore normalized rather than treated as a factual change.
#' The canonical object's normalization metadata reports these fields using one
#' shared internal assessment path. Completed or other terminal games,
#' resignation states, reserved GNU patterns, unsupported XGID actions, and
#' structurally invalid positions fail explicitly.
#'
#' @examples
#' state <- position_from_gnuid(
#'   "4HPwATDgc/ABMA:8IhuACAACAAE"
#' )
#' xgid_from_position(state)
#' gnuid_from_position(state)
#'
#' gnuid_to_xgid(
#'   position_id = "4HPwATDgc/ABMA",
#'   match_id = "8IhuACAACAAE"
#' )
#'
#' @name position_identifiers
NULL

#' @rdname position_identifiers
#' @export
position_from_gnuid <- function(gnuid, match_id = NULL) {
  parts <- split_complete_gnuid(gnuid, match_id)
  match_state <- decode_gnu_match_id(parts$match_id)
  checker_state <- decode_gnu_position_id(
    parts$position_id,
    dice_owner = match_state$dice_owner
  )
  validate_supported_checker_state(checker_state$players, "position_id")

  new_position_state(
    players = checker_state$players,
    turn = list(
      dice_owner = match_state$dice_owner,
      turn_owner = match_state$turn_owner,
      action = if (match_state$double_offered) "double" else "roll",
      dice = if (
        match_state$double_offered || all(match_state$dice == 0L)
      ) {
        integer()
      } else {
        match_state$dice
      }
    ),
    cube = list(
      exponent = match_state$cube_exponent,
      value = 2^match_state$cube_exponent,
      owner = match_state$cube_owner,
      max_exponent = max(
        .xgid_default_max_cube_exponent,
        match_state$cube_exponent
      )
    ),
    score = c(
      player_0 = match_state$score_player_0,
      player_1 = match_state$score_player_1
    ),
    match = list(
      length = match_state$match_length,
      crawford = match_state$crawford,
      jacoby = match_state$jacoby,
      game_state = match_state$game_state,
      game_state_code = match_state$game_state_code
    ),
    source = list(
      format = "GNUID",
      identifier = parts$complete,
      position_id = parts$position_id,
      match_id = parts$match_id
    ),
    provenance = identifier_provenance("GNUID")
  )
}

#' @rdname position_identifiers
#' @export
position_from_xgid <- function(xgid) {
  decoded <- decode_xgid(xgid)

  new_position_state(
    players = decoded$players,
    turn = decoded$turn,
    cube = decoded$cube,
    score = decoded$score,
    match = decoded$match,
    source = list(
      format = "XGID",
      identifier = xgid
    ),
    provenance = identifier_provenance("XGID")
  )
}

#' @rdname position_identifiers
#' @export
gnuid_from_position <- function(position, allow_lossy = FALSE) {
  validate_position_state(position)
  validate_flag(allow_lossy, "allow_lossy")

  source_identifier <- reusable_source_identifier(position, "GNUID")

  if (!is.null(source_identifier)) {
    return(source_identifier)
  }

  assessment <- identifier_conversion_assessment(position, "GNUID")
  lossy_fields <- assessment$lossy_fields

  if (length(lossy_fields) > 0L && !allow_lossy) {
    abort_identifier(
      paste0(
        "The canonical position contains XGID-only state that GNUID cannot ",
        "preserve. Set `allow_lossy = TRUE` to accept the documented ",
        "normalization."
      ),
      class = "backgammoncalculator_unsupported_identifier_state",
      argument = "position",
      rule = "xgid_only_fields",
      fields = lossy_fields
    )
  }

  position_id <- encode_gnu_position_id(position)
  match_id <- encode_gnu_match_id(position)
  paste0(position_id, ":", match_id)
}

#' @rdname position_identifiers
#' @export
xgid_from_position <- function(position) {
  validate_position_state(position)

  source_identifier <- reusable_source_identifier(position, "XGID")

  if (!is.null(source_identifier)) {
    return(source_identifier)
  }

  assessment <- identifier_conversion_assessment(position, "XGID")
  unsupported_loss <- setdiff(assessment$lossy_fields, "match.game_state")

  if (length(unsupported_loss) > 0L) {
    abort_identifier(
      "The canonical position contains state that XGID cannot preserve.",
      class = "backgammoncalculator_unsupported_identifier_state",
      argument = "position",
      rule = "xgid_lossy_fields",
      fields = unsupported_loss
    )
  }

  checker_component <- encode_xgid_checker_component(position)
  match_component <- encode_xgid_match_component(position)

  paste0(
    "XGID=",
    checker_component,
    ":",
    paste0(match_component, collapse = ":")
  )
}

#' @rdname position_identifiers
#' @export
gnuid_to_xgid <- function(position_id, match_id = NULL) {
  xgid_from_position(position_from_gnuid(position_id, match_id))
}

#' @rdname position_identifiers
#' @export
xgid_to_gnuid <- function(xgid, allow_lossy = FALSE) {
  gnuid_from_position(
    position_from_xgid(xgid),
    allow_lossy = allow_lossy
  )
}

split_complete_gnuid <- function(gnuid, match_id = NULL) {
  if (missing(gnuid)) {
    abort_identifier(
      "`gnuid` must be one complete GNUID or one GNU Position ID.",
      class = "backgammoncalculator_invalid_complete_gnuid",
      argument = "gnuid",
      rule = "required_argument"
    )
  }

  if (is.null(match_id)) {
    validate_scalar_character(
      gnuid,
      argument = "gnuid",
      class = "backgammoncalculator_invalid_complete_gnuid"
    )

    matched <- regexec(
      "^([A-Za-z0-9+/]{14}):([A-Za-z0-9+/]{12})$",
      gnuid,
      perl = TRUE
    )
    pieces <- regmatches(gnuid, matched)[[1L]]

    if (length(pieces) != 3L) {
      abort_identifier(
        paste0(
          "`gnuid` must be one complete GNUID in ",
          "`<position_id>:<match_id>` form."
        ),
        class = "backgammoncalculator_invalid_complete_gnuid",
        argument = "gnuid",
        rule = "complete_gnuid_format"
      )
    }

    position_id <- pieces[[2L]]
    match_id <- pieces[[3L]]
    complete <- gnuid
  } else {
    position_id <- gnuid
    complete <- paste0(position_id, ":", match_id)
  }

  validate_gnu_position_id(position_id)
  validate_gnu_match_id(match_id)

  list(
    complete = complete,
    position_id = position_id,
    match_id = match_id
  )
}

validate_gnu_position_id <- function(position_id) {
  validate_gnu_id_scalar(
    position_id,
    argument = "position_id",
    expected_length = 14L,
    class = "backgammoncalculator_invalid_gnu_position_id"
  )

  values <- gnu_base64_values(position_id)

  if ((values[[14L]] %% 16L) != 0L) {
    abort_identifier(
      paste0(
        "`position_id` must be one valid 14-character GNU Position ID. ",
        "Its final Base64 character contains non-zero padding bits."
      ),
      class = "backgammoncalculator_invalid_gnu_position_id",
      argument = "position_id",
      rule = "canonical_padding"
    )
  }

  invisible(position_id)
}

validate_gnu_match_id <- function(match_id) {
  validate_gnu_id_scalar(
    match_id,
    argument = "match_id",
    expected_length = 12L,
    class = "backgammoncalculator_invalid_gnu_match_id"
  )

  invisible(match_id)
}

validate_gnu_id_scalar <- function(
    value,
    argument,
    expected_length,
    class) {
  validate_scalar_character(value, argument, class)
  characters <- strsplit(value, "", fixed = TRUE)[[1L]]

  if (
    length(characters) != expected_length ||
      any(!characters %in% .gnu_base64_alphabet)
  ) {
    abort_identifier(
      sprintf(
        "`%s` must be one valid %d-character GNU %s ID.",
        argument,
        expected_length,
        if (identical(argument, "position_id")) "Position" else "Match"
      ),
      class = class,
      argument = argument,
      rule = "length_and_base64_alphabet"
    )
  }

  invisible(value)
}

validate_scalar_character <- function(value, argument, class) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    abort_identifier(
      sprintf("`%s` must be one non-missing character value.", argument),
      class = class,
      argument = argument,
      rule = "scalar_character"
    )
  }

  invisible(value)
}

validate_flag <- function(value, argument) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    abort_identifier(
      sprintf("`%s` must be TRUE or FALSE.", argument),
      class = "backgammoncalculator_invalid_position_state",
      argument = argument,
      rule = "logical_flag"
    )
  }

  invisible(value)
}

gnu_base64_values <- function(identifier) {
  characters <- strsplit(identifier, "", fixed = TRUE)[[1L]]
  unname(match(characters, .gnu_base64_alphabet) - 1L)
}

gnu_base64_to_bits <- function(identifier) {
  values <- gnu_base64_values(identifier)
  complete_groups <- length(values) %/% 4L
  bytes <- integer()

  if (complete_groups > 0L) {
    for (group_index in seq_len(complete_groups)) {
      start <- (group_index - 1L) * 4L + 1L
      group <- values[start:(start + 3L)]

      bytes <- c(
        bytes,
        group[[1L]] * 4L + group[[2L]] %/% 16L,
        (group[[2L]] %% 16L) * 16L + group[[3L]] %/% 4L,
        (group[[3L]] %% 4L) * 64L + group[[4L]]
      )
    }
  }

  remainder <- length(values) %% 4L

  if (remainder == 2L) {
    final <- utils::tail(values, 2L)
    bytes <- c(
      bytes,
      final[[1L]] * 4L + final[[2L]] %/% 16L
    )
  } else if (remainder != 0L) {
    abort_identifier(
      "The GNU identifier has an unsupported Base64 length.",
      class = "backgammoncalculator_identifier_conversion_error",
      argument = NA_character_,
      rule = "base64_group_length"
    )
  }

  unlist(
    lapply(
      bytes,
      function(byte) {
        vapply(
          0:7,
          function(shift) {
            bitwAnd(bitwShiftR(as.integer(byte), shift), 1L)
          },
          integer(1)
        )
      }
    ),
    use.names = FALSE
  )
}

gnu_bits_to_base64 <- function(bits) {
  if (length(bits) %% 8L != 0L || any(!bits %in% c(0L, 1L))) {
    abort_identifier(
      "Internal GNU bit data could not be encoded.",
      class = "backgammoncalculator_identifier_conversion_error",
      argument = NA_character_,
      rule = "bit_encoding"
    )
  }

  starts <- seq.int(1L, length(bits), by = 8L)
  bytes <- vapply(
    starts,
    function(start) {
      chunk <- bits[start:(start + 7L)]
      as.integer(sum(chunk * 2^(0:7)))
    },
    integer(1)
  )

  values <- integer()
  complete_groups <- length(bytes) %/% 3L

  if (complete_groups > 0L) {
    for (group_index in seq_len(complete_groups)) {
      start <- (group_index - 1L) * 3L + 1L
      group <- bytes[start:(start + 2L)]

      values <- c(
        values,
        group[[1L]] %/% 4L,
        (group[[1L]] %% 4L) * 16L + group[[2L]] %/% 16L,
        (group[[2L]] %% 16L) * 4L + group[[3L]] %/% 64L,
        group[[3L]] %% 64L
      )
    }
  }

  remainder <- length(bytes) %% 3L

  if (remainder == 1L) {
    final <- utils::tail(bytes, 1L)
    values <- c(
      values,
      final[[1L]] %/% 4L,
      (final[[1L]] %% 4L) * 16L
    )
  } else if (remainder == 2L) {
    final <- utils::tail(bytes, 2L)
    values <- c(
      values,
      final[[1L]] %/% 4L,
      (final[[1L]] %% 4L) * 16L + final[[2L]] %/% 16L,
      (final[[2L]] %% 16L) * 4L
    )
  }

  paste0(.gnu_base64_alphabet[values + 1L], collapse = "")
}

gnu_bits_to_integer <- function(bits, start, width) {
  selected <- bits[start + seq_len(width)]
  as.integer(sum(selected * 2^(0:(width - 1L))))
}

write_gnu_integer_bits <- function(bits, start, width, value) {
  positions <- start + seq_len(width)
  bits[positions] <- vapply(
    0:(width - 1L),
    function(shift) {
      bitwAnd(bitwShiftR(as.integer(value), shift), 1L)
    },
    integer(1)
  )
  bits
}

decode_gnu_position_id <- function(position_id, dice_owner) {
  bits <- gnu_base64_to_bits(position_id)

  if (length(bits) != 80L) {
    abort_identifier(
      "The GNU Position ID does not decode to the required 80-bit key.",
      class = "backgammoncalculator_invalid_gnu_position_id",
      argument = "position_id",
      rule = "decoded_length"
    )
  }

  relative_board <- matrix(
    0L,
    nrow = 2L,
    ncol = 25L,
    dimnames = list(
      c("relative_opponent", "relative_dice_owner"),
      c(paste0("point_", 1:24), "bar")
    )
  )

  cursor <- 1L

  for (relative_player in seq_len(2L)) {
    for (point in seq_len(25L)) {
      checker_count <- 0L

      while (cursor <= length(bits) && bits[[cursor]] == 1L) {
        checker_count <- checker_count + 1L
        cursor <- cursor + 1L
      }

      if (cursor > length(bits)) {
        abort_identifier(
          "The GNU Position ID has a malformed unary checker sequence.",
          class = "backgammoncalculator_invalid_gnu_position_id",
          argument = "position_id",
          rule = "unary_separator"
        )
      }

      relative_board[relative_player, point] <- checker_count
      cursor <- cursor + 1L
    }
  }

  if (cursor <= length(bits) && any(bits[cursor:length(bits)] != 0L)) {
    abort_identifier(
      "The GNU Position ID has non-zero trailing position bits.",
      class = "backgammoncalculator_invalid_gnu_position_id",
      argument = "position_id",
      rule = "trailing_bits"
    )
  }

  checker_totals <- rowSums(relative_board)

  if (any(checker_totals > 15L)) {
    abort_identifier(
      "The GNU Position ID assigns more than 15 checkers to a player.",
      class = "backgammoncalculator_invalid_gnu_position_id",
      argument = "position_id",
      rule = "checker_total"
    )
  }

  stable_rows <- vector("list", 2L)
  names(stable_rows) <- c("player_0", "player_1")
  stable_rows[[dice_owner]] <- relative_board[2L, ]
  stable_rows[[other_player(dice_owner)]] <- relative_board[1L, ]

  players <- lapply(
    names(stable_rows),
    function(player) {
      own_points <- unname(stable_rows[[player]][1:24])
      physical_points <- if (identical(player, "player_0")) {
        rev(own_points)
      } else {
        own_points
      }
      bar <- unname(stable_rows[[player]][[25L]])

      list(
        points = stats::setNames(
          as.integer(physical_points),
          paste0("point_", 1:24)
        ),
        bar = as.integer(bar),
        off = as.integer(15L - sum(physical_points) - bar)
      )
    }
  )
  names(players) <- names(stable_rows)

  overlap <-
    players$player_0$points > 0L &
    players$player_1$points > 0L

  if (any(overlap)) {
    abort_identifier(
      "The GNU Position ID places both players on the same physical point.",
      class = "backgammoncalculator_invalid_gnu_position_id",
      argument = "position_id",
      rule = "physical_point_overlap"
    )
  }

  list(players = players)
}

decode_gnu_match_id <- function(match_id) {
  bits <- gnu_base64_to_bits(match_id)

  if (length(bits) != 72L) {
    abort_identifier(
      "The GNU Match ID does not decode to the required 72-bit key.",
      class = "backgammoncalculator_invalid_gnu_match_id",
      argument = "match_id",
      rule = "decoded_length"
    )
  }

  cube_owner_code <- gnu_bits_to_integer(bits, 4L, 2L)
  dice_owner_code <- gnu_bits_to_integer(bits, 6L, 1L)
  turn_owner_code <- gnu_bits_to_integer(bits, 11L, 1L)
  game_state_code <- gnu_bits_to_integer(bits, 8L, 3L)

  decoded <- list(
    cube_exponent = gnu_bits_to_integer(bits, 0L, 4L),
    cube_owner_code = cube_owner_code,
    cube_owner = switch(
      as.character(cube_owner_code),
      `0` = "player_0",
      `1` = "player_1",
      `3` = "centered",
      NA_character_
    ),
    dice_owner = if (dice_owner_code == 0L) "player_0" else "player_1",
    crawford = as.logical(gnu_bits_to_integer(bits, 7L, 1L)),
    game_state_code = game_state_code,
    game_state = switch(
      as.character(game_state_code),
      `0` = "not_started",
      `1` = "playing",
      `2` = "game_over",
      `3` = "resigned",
      `4` = "dropped",
      "reserved"
    ),
    turn_owner = if (turn_owner_code == 0L) "player_0" else "player_1",
    double_offered = as.logical(gnu_bits_to_integer(bits, 12L, 1L)),
    resignation_value = gnu_bits_to_integer(bits, 13L, 2L),
    dice = c(
      gnu_bits_to_integer(bits, 15L, 3L),
      gnu_bits_to_integer(bits, 18L, 3L)
    ),
    match_length = gnu_bits_to_integer(bits, 21L, 15L),
    score_player_0 = gnu_bits_to_integer(bits, 36L, 15L),
    score_player_1 = gnu_bits_to_integer(bits, 51L, 15L),
    jacoby = !as.logical(gnu_bits_to_integer(bits, 66L, 1L)),
    reserved_bits = bits[68:72]
  )

  validate_decoded_gnu_match(decoded)
  decoded
}

validate_decoded_gnu_match <- function(match_state) {
  if (any(match_state$reserved_bits != 0L)) {
    abort_identifier(
      "The GNU Match ID uses a reserved bit pattern.",
      class = "backgammoncalculator_invalid_gnu_match_id",
      argument = "match_id",
      rule = "reserved_bits"
    )
  }

  if (is.na(match_state$cube_owner)) {
    abort_identifier(
      "The GNU Match ID uses the reserved cube-owner code.",
      class = "backgammoncalculator_invalid_gnu_match_id",
      argument = "match_id",
      rule = "cube_owner_code"
    )
  }

  if (any(match_state$dice > 6L)) {
    abort_identifier(
      "The GNU Match ID contains an invalid die value.",
      class = "backgammoncalculator_invalid_gnu_match_id",
      argument = "match_id",
      rule = "die_range"
    )
  }

  if (xor(match_state$dice[[1L]] == 0L, match_state$dice[[2L]] == 0L)) {
    abort_identifier(
      "The GNU Match ID contains only one die value.",
      class = "backgammoncalculator_invalid_gnu_match_id",
      argument = "match_id",
      rule = "dice_pair"
    )
  }

  if (match_state$game_state_code %in% 5:7) {
    abort_identifier(
      "The GNU Match ID uses a reserved game-state code.",
      class = "backgammoncalculator_invalid_gnu_match_id",
      argument = "match_id",
      rule = "game_state_code"
    )
  }

  if (match_state$game_state_code %in% 2:4) {
    abort_identifier(
      paste0(
        "The GNU Match ID represents a completed, resigned, or dropped ",
        "game that is outside the supported canonical position contract."
      ),
      class = "backgammoncalculator_unsupported_identifier_state",
      argument = "match_id",
      rule = "completed_game_state"
    )
  }

  if (match_state$resignation_value != 0L) {
    abort_identifier(
      paste0(
        "The GNU Match ID contains a resignation offer that is outside ",
        "the supported canonical position contract."
      ),
      class = "backgammoncalculator_unsupported_identifier_state",
      argument = "match_id",
      rule = "resignation_offer"
    )
  }

  validate_match_rules(
    match_length = match_state$match_length,
    score = c(
      player_0 = match_state$score_player_0,
      player_1 = match_state$score_player_1
    ),
    crawford = match_state$crawford,
    cube_exponent = match_state$cube_exponent,
    cube_owner = match_state$cube_owner,
    invalid_class = "backgammoncalculator_invalid_gnu_match_id",
    argument = "match_id"
  )

  dice_present <- all(match_state$dice != 0L)

  if (match_state$double_offered) {
    if (match_state$game_state_code != 1L) {
      abort_identifier(
        "The GNU Match ID offers a double outside a game in progress.",
        class = "backgammoncalculator_invalid_gnu_match_id",
        argument = "match_id",
        rule = "double_game_state"
      )
    }

    if (dice_present) {
      abort_identifier(
        "The GNU Match ID contains dice while a double is pending.",
        class = "backgammoncalculator_invalid_gnu_match_id",
        argument = "match_id",
        rule = "double_with_dice"
      )
    }

    if (identical(match_state$turn_owner, match_state$dice_owner)) {
      abort_identifier(
        paste0(
          "The GNU Match ID does not assign a pending-double decision to ",
          "the opponent."
        ),
        class = "backgammoncalculator_invalid_gnu_match_id",
        argument = "match_id",
        rule = "double_turn_owner"
      )
    }
  } else if (!identical(match_state$turn_owner, match_state$dice_owner)) {
    abort_identifier(
      paste0(
        "The GNU Match ID has inconsistent DiceOwner and TurnOwner fields."
      ),
      class = "backgammoncalculator_invalid_gnu_match_id",
      argument = "match_id",
      rule = "turn_owner"
    )
  }

  invisible(match_state)
}

encode_gnu_position_id <- function(position) {
  own_rows <- list(
    player_0 = c(
      rev(unname(position$players$player_0$points)),
      position$players$player_0$bar
    ),
    player_1 = c(
      unname(position$players$player_1$points),
      position$players$player_1$bar
    )
  )

  relative_rows <- list(
    own_rows[[other_player(position$turn$dice_owner)]],
    own_rows[[position$turn$dice_owner]]
  )

  bits <- integer()

  for (row in relative_rows) {
    for (count in row) {
      bits <- c(bits, rep.int(1L, count), 0L)
    }
  }

  if (length(bits) > 80L) {
    abort_identifier(
      "The canonical checker state cannot be represented by a GNU Position ID.",
      class = "backgammoncalculator_unsupported_identifier_state",
      argument = "position",
      rule = "gnu_position_capacity"
    )
  }

  bits <- c(bits, rep.int(0L, 80L - length(bits)))
  gnu_bits_to_base64(bits)
}

encode_gnu_match_id <- function(position) {
  bits <- integer(72L)
  cube_owner_code <- switch(
    position$cube$owner,
    player_0 = 0L,
    player_1 = 1L,
    centered = 3L
  )
  dice_owner_code <- if (position$turn$dice_owner == "player_0") 0L else 1L
  turn_owner_code <- if (position$turn$turn_owner == "player_0") 0L else 1L
  dice <- if (identical(position$turn$action, "double")) {
    c(0L, 0L)
  } else if (length(position$turn$dice) == 0L) {
    c(0L, 0L)
  } else {
    sort(as.integer(position$turn$dice), decreasing = TRUE)
  }

  fields <- list(
    c(0L, 4L, position$cube$exponent),
    c(4L, 2L, cube_owner_code),
    c(6L, 1L, dice_owner_code),
    c(7L, 1L, as.integer(position$match$crawford)),
    c(8L, 3L, position$match$game_state_code),
    c(11L, 1L, turn_owner_code),
    c(12L, 1L, as.integer(position$turn$action == "double")),
    c(13L, 2L, 0L),
    c(15L, 3L, dice[[1L]]),
    c(18L, 3L, dice[[2L]]),
    c(21L, 15L, position$match$length),
    c(36L, 15L, position$score[["player_0"]]),
    c(51L, 15L, position$score[["player_1"]]),
    c(66L, 1L, as.integer(!position$match$jacoby)),
    c(67L, 5L, 0L)
  )

  for (field in fields) {
    bits <- write_gnu_integer_bits(
      bits,
      start = field[[1L]],
      width = field[[2L]],
      value = field[[3L]]
    )
  }

  gnu_bits_to_base64(bits)
}

decode_xgid <- function(xgid) {
  validate_scalar_character(
    xgid,
    argument = "xgid",
    class = "backgammoncalculator_invalid_xgid"
  )

  if (!grepl("^XGID=", xgid)) {
    abort_identifier(
      "`xgid` must begin with `XGID=`.",
      class = "backgammoncalculator_invalid_xgid",
      argument = "xgid",
      rule = "prefix"
    )
  }

  fields <- strsplit(sub("^XGID=", "", xgid), ":", fixed = TRUE)[[1L]]

  if (length(fields) != 10L) {
    abort_identifier(
      "`xgid` must contain one checker component and nine match fields.",
      class = "backgammoncalculator_invalid_xgid",
      argument = "xgid",
      rule = "field_count"
    )
  }

  checker_component <- fields[[1L]]
  cube_exponent <- parse_xgid_integer(fields[[2L]], "cube exponent", 0L, 15L)
  cube_owner_code <- parse_xgid_integer(fields[[3L]], "cube owner", -1L, 1L)
  turn_code <- parse_xgid_integer(fields[[4L]], "turn", -1L, 1L)
  dice_or_action <- fields[[5L]]
  score_player_1 <- parse_xgid_integer(fields[[6L]], "player_1 score", 0L, 32767L)
  score_player_0 <- parse_xgid_integer(fields[[7L]], "player_0 score", 0L, 32767L)
  rule_field <- parse_xgid_integer(fields[[8L]], "rule field", 0L, 1L)
  match_length <- parse_xgid_integer(fields[[9L]], "match length", 0L, 32767L)
  max_cube_exponent <- parse_xgid_integer(
    fields[[10L]],
    "maximum cube exponent",
    0L,
    15L
  )

  if (!cube_owner_code %in% c(-1L, 0L, 1L)) {
    abort_invalid_xgid("The XGID cube owner must be -1, 0, or 1.", "cube_owner")
  }

  if (!turn_code %in% c(-1L, 1L)) {
    abort_invalid_xgid("The XGID turn field must be -1 or 1.", "turn")
  }

  if (cube_exponent > max_cube_exponent) {
    abort_invalid_xgid(
      "The XGID current cube exponent exceeds its maximum cube exponent.",
      "cube_maximum"
    )
  }

  if (!grepl("^(00|[1-6][1-6]|D)$", dice_or_action)) {
    abort_identifier(
      paste0(
        "The XGID dice or action field must be `00`, two dice from 1 to 6, ",
        "or `D`."
      ),
      class = "backgammoncalculator_unsupported_identifier_state",
      argument = "xgid",
      rule = "dice_or_action"
    )
  }

  players <- decode_xgid_checker_component(checker_component)
  validate_supported_checker_state(players, "xgid")
  dice_owner <- if (turn_code == -1L) "player_0" else "player_1"
  action <- if (identical(dice_or_action, "D")) "double" else "roll"
  turn_owner <- if (identical(action, "double")) {
    other_player(dice_owner)
  } else {
    dice_owner
  }
  dice <- if (identical(action, "double") || identical(dice_or_action, "00")) {
    integer()
  } else {
    as.integer(strsplit(dice_or_action, "", fixed = TRUE)[[1L]])
  }
  cube_owner <- switch(
    as.character(cube_owner_code),
    `-1` = "player_0",
    `0` = "centered",
    `1` = "player_1"
  )
  score <- c(
    player_0 = score_player_0,
    player_1 = score_player_1
  )
  crawford <- match_length > 0L && rule_field == 1L
  jacoby <- match_length == 0L && rule_field == 1L

  validate_match_rules(
    match_length = match_length,
    score = score,
    crawford = crawford,
    cube_exponent = cube_exponent,
    cube_owner = cube_owner,
    invalid_class = "backgammoncalculator_invalid_xgid",
    argument = "xgid"
  )

  list(
    players = players,
    turn = list(
      dice_owner = dice_owner,
      turn_owner = turn_owner,
      action = action,
      dice = dice
    ),
    cube = list(
      exponent = cube_exponent,
      value = 2^cube_exponent,
      owner = cube_owner,
      max_exponent = max_cube_exponent
    ),
    score = score,
    match = list(
      length = match_length,
      crawford = crawford,
      jacoby = jacoby,
      game_state = "playing",
      game_state_code = 1L
    )
  )
}

parse_xgid_integer <- function(value, label, minimum, maximum) {
  if (!grepl("^-?[0-9]+$", value)) {
    abort_invalid_xgid(
      sprintf("The XGID %s must be an integer.", label),
      paste0("integer_", gsub(" ", "_", label, fixed = TRUE))
    )
  }

  parsed <- suppressWarnings(as.integer(value))

  if (is.na(parsed) || parsed < minimum || parsed > maximum) {
    abort_invalid_xgid(
      sprintf(
        "The XGID %s must be between %d and %d.",
        label,
        minimum,
        maximum
      ),
      paste0("range_", gsub(" ", "_", label, fixed = TRUE))
    )
  }

  parsed
}

decode_xgid_checker_component <- function(component) {
  characters <- strsplit(component, "", fixed = TRUE)[[1L]]

  if (length(characters) != 26L) {
    abort_invalid_xgid(
      "The XGID checker component must contain exactly 26 characters.",
      "checker_length"
    )
  }

  if (!all(characters %in% c("-", letters[1:15], LETTERS[1:15]))) {
    abort_invalid_xgid(
      "The XGID checker component contains an unsupported checker character.",
      "checker_alphabet"
    )
  }

  if (!characters[[1L]] %in% c("-", letters[1:15])) {
    abort_invalid_xgid(
      "The first XGID checker slot may contain only the player_0 bar.",
      "player_0_bar"
    )
  }

  if (!characters[[26L]] %in% c("-", LETTERS[1:15])) {
    abort_invalid_xgid(
      "The final XGID checker slot may contain only the player_1 bar.",
      "player_1_bar"
    )
  }

  player_0_points <- integer(24L)
  player_1_points <- integer(24L)

  for (physical_point in seq_len(24L)) {
    character <- characters[[physical_point + 1L]]

    if (character %in% letters[1:15]) {
      player_0_points[[physical_point]] <- match(character, letters)
    } else if (character %in% LETTERS[1:15]) {
      player_1_points[[physical_point]] <- match(character, LETTERS)
    }
  }

  player_0_bar <- if (characters[[1L]] == "-") {
    0L
  } else {
    match(characters[[1L]], letters)
  }
  player_1_bar <- if (characters[[26L]] == "-") {
    0L
  } else {
    match(characters[[26L]], LETTERS)
  }

  player_0_total <- sum(player_0_points) + player_0_bar
  player_1_total <- sum(player_1_points) + player_1_bar

  if (player_0_total > 15L || player_1_total > 15L) {
    abort_invalid_xgid(
      "The XGID checker component assigns more than 15 checkers to a player.",
      "checker_total"
    )
  }

  list(
    player_0 = list(
      points = stats::setNames(player_0_points, paste0("point_", 1:24)),
      bar = as.integer(player_0_bar),
      off = as.integer(15L - player_0_total)
    ),
    player_1 = list(
      points = stats::setNames(player_1_points, paste0("point_", 1:24)),
      bar = as.integer(player_1_bar),
      off = as.integer(15L - player_1_total)
    )
  )
}

encode_xgid_checker_component <- function(position) {
  component <- rep("-", 26L)

  if (position$players$player_0$bar > 0L) {
    component[[1L]] <- checker_letter(
      position$players$player_0$bar,
      "player_0"
    )
  }

  for (physical_point in seq_len(24L)) {
    player_0_count <- position$players$player_0$points[[physical_point]]
    player_1_count <- position$players$player_1$points[[physical_point]]

    if (player_0_count > 0L) {
      component[[physical_point + 1L]] <- checker_letter(
        player_0_count,
        "player_0"
      )
    } else if (player_1_count > 0L) {
      component[[physical_point + 1L]] <- checker_letter(
        player_1_count,
        "player_1"
      )
    }
  }

  if (position$players$player_1$bar > 0L) {
    component[[26L]] <- checker_letter(
      position$players$player_1$bar,
      "player_1"
    )
  }

  paste0(component, collapse = "")
}

checker_letter <- function(count, player) {
  if (count < 1L || count > 15L) {
    abort_identifier(
      "A checker count is outside the supported identifier range.",
      class = "backgammoncalculator_invalid_position_state",
      argument = "position",
      rule = "checker_count"
    )
  }

  if (identical(player, "player_0")) {
    letters[[count]]
  } else {
    LETTERS[[count]]
  }
}

encode_xgid_match_component <- function(position) {
  cube_owner <- switch(
    position$cube$owner,
    player_0 = -1L,
    centered = 0L,
    player_1 = 1L
  )
  turn <- if (position$turn$dice_owner == "player_0") -1L else 1L
  dice_or_action <- if (position$turn$action == "double") {
    "D"
  } else if (length(position$turn$dice) == 0L) {
    "00"
  } else {
    paste0(position$turn$dice, collapse = "")
  }
  rule_field <- if (position$match$length == 0L) {
    as.integer(position$match$jacoby)
  } else {
    as.integer(position$match$crawford)
  }

  c(
    as.character(position$cube$exponent),
    as.character(cube_owner),
    as.character(turn),
    dice_or_action,
    as.character(position$score[["player_1"]]),
    as.character(position$score[["player_0"]]),
    as.character(rule_field),
    as.character(position$match$length),
    as.character(position$cube$max_exponent)
  )
}

new_position_state <- function(
    players,
    turn,
    cube,
    score,
    match,
    source,
    provenance) {
  position <- structure(
    list(
      version = .position_state_version,
      players = players,
      turn = turn,
      cube = cube,
      score = score,
      match = match,
      source = source,
      provenance = provenance,
      normalization = list()
    ),
    class = c("backgammon_position_state", "list")
  )

  position$source$fingerprint <- position_state_fingerprint(position)
  position$normalization <- build_normalization_metadata(position)
  validate_position_state(position)
  position
}

validate_position_state <- function(position) {
  if (!inherits(position, "backgammon_position_state") || !is.list(position)) {
    abort_identifier(
      paste0(
        "`position` must be a canonical backgammon position returned by ",
        "`position_from_gnuid()` or `position_from_xgid()`."
      ),
      class = "backgammoncalculator_invalid_position_state",
      argument = "position",
      rule = "class"
    )
  }

  required <- c(
    "version",
    "players",
    "turn",
    "cube",
    "score",
    "match",
    "source",
    "provenance",
    "normalization"
  )

  if (!all(required %in% names(position))) {
    abort_invalid_position_state(
      "The canonical position is missing required fields.",
      "fields"
    )
  }

  if (!identical(position$version, .position_state_version)) {
    abort_invalid_position_state(
      "The canonical position version is unsupported.",
      "version"
    )
  }

  if (
    !is.list(position$players) ||
      !identical(names(position$players), c("player_0", "player_1"))
  ) {
    abort_invalid_position_state(
      "The canonical position must contain stable player_0 and player_1 records.",
      "players"
    )
  }

  for (player in c("player_0", "player_1")) {
    record <- position$players[[player]]

    if (!is_named_record(record, c("points", "bar", "off"))) {
      abort_invalid_position_state(
        "A canonical player record is malformed.",
        "player_record"
      )
    }

    if (!is_integer_vector_in_range(record$points, 24L, 0L, 15L)) {
      abort_invalid_position_state(
        "Canonical point counts must be 24 non-negative integers no greater than 15.",
        "point_counts"
      )
    }

    if (
      !is_single_integer_in_range(record$bar, 0L, 15L) ||
        !is_single_integer_in_range(record$off, 0L, 15L)
    ) {
      abort_invalid_position_state(
        "Canonical bar and off counts must be integers from 0 to 15.",
        "bar_off_counts"
      )
    }

    if (sum(record$points) + record$bar + record$off != 15L) {
      abort_invalid_position_state(
        "Each canonical player must have exactly 15 checkers.",
        "checker_total"
      )
    }
  }

  if (
    any(
      position$players$player_0$points > 0L &
        position$players$player_1$points > 0L
    )
  ) {
    abort_invalid_position_state(
      "Both stable players cannot occupy the same physical point.",
      "point_overlap"
    )
  }

  validate_supported_checker_state(position$players, "position")

  if (!is_named_record(position$turn, c("dice_owner", "turn_owner", "action", "dice"))) {
    abort_invalid_position_state("The canonical turn record is malformed.", "turn_record")
  }

  stable_players <- c("player_0", "player_1")

  if (!is_single_character_choice(position$turn$dice_owner, stable_players)) {
    abort_invalid_position_state("The dice owner is invalid.", "dice_owner")
  }

  if (!is_single_character_choice(position$turn$turn_owner, stable_players)) {
    abort_invalid_position_state("The turn owner is invalid.", "turn_owner")
  }

  if (!is_single_character_choice(position$turn$action, c("roll", "double"))) {
    abort_invalid_position_state("The turn action is unsupported.", "turn_action")
  }

  if (identical(position$turn$action, "double")) {
    if (!is.numeric(position$turn$dice) || length(position$turn$dice) != 0L) {
      abort_invalid_position_state(
        "A pending double cannot contain dice.",
        "double_dice"
      )
    }

    if (
      !identical(
        position$turn$turn_owner,
        other_player(position$turn$dice_owner)
      )
    ) {
      abort_invalid_position_state(
        "A pending double must assign the decision to the opponent.",
        "double_turn_owner"
      )
    }
  } else {
    dice <- position$turn$dice

    if (
      !is.numeric(dice) ||
        !length(dice) %in% c(0L, 2L) ||
        (length(dice) == 2L && !is_integer_vector_in_range(dice, 2L, 1L, 6L))
    ) {
      abort_invalid_position_state(
        "Canonical dice must be absent or contain two integer values from 1 to 6.",
        "dice"
      )
    }

    if (!identical(position$turn$turn_owner, position$turn$dice_owner)) {
      abort_invalid_position_state(
        "Roll states must have the same dice owner and turn owner.",
        "roll_turn_owner"
      )
    }
  }

  if (!is_named_record(position$cube, c("exponent", "value", "owner", "max_exponent"))) {
    abort_invalid_position_state("The canonical cube record is malformed.", "cube_record")
  }

  if (
    !is_single_integer_in_range(position$cube$exponent, 0L, 15L) ||
      !is_single_finite_number(position$cube$value) ||
      as.numeric(position$cube$value) != 2^position$cube$exponent ||
      !is_single_character_choice(
        position$cube$owner,
        c("player_0", "centered", "player_1")
      ) ||
      !is_single_integer_in_range(position$cube$max_exponent, 0L, 15L) ||
      position$cube$exponent > position$cube$max_exponent
  ) {
    abort_invalid_position_state("The canonical cube state is invalid.", "cube")
  }

  if (
    !is_integer_vector_in_range(position$score, 2L, 0L, 32767L) ||
      !identical(names(position$score), c("player_0", "player_1"))
  ) {
    abort_invalid_position_state("The canonical score is invalid.", "score")
  }

  if (!is_named_record(
    position$match,
    c("length", "crawford", "jacoby", "game_state", "game_state_code")
  )) {
    abort_invalid_position_state("The canonical match record is malformed.", "match_record")
  }

  if (
    !is_single_integer_in_range(position$match$length, 0L, 32767L) ||
      !is_single_logical(position$match$crawford) ||
      !is_single_logical(position$match$jacoby) ||
      !is_single_character_choice(
        position$match$game_state,
        c("not_started", "playing")
      ) ||
      !is_single_integer_in_range(position$match$game_state_code, 0L, 1L)
  ) {
    abort_invalid_position_state("The canonical match state is invalid.", "match")
  }

  if (
    (identical(position$match$game_state, "not_started") &&
      position$match$game_state_code != 0L) ||
      (identical(position$match$game_state, "playing") &&
        position$match$game_state_code != 1L)
  ) {
    abort_invalid_position_state(
      "The canonical game-state name and code are inconsistent.",
      "game_state"
    )
  }

  validate_match_rules(
    match_length = position$match$length,
    score = position$score,
    crawford = position$match$crawford,
    cube_exponent = position$cube$exponent,
    cube_owner = position$cube$owner,
    invalid_class = "backgammoncalculator_invalid_position_state",
    argument = "position"
  )

  if (position$match$length > 0L && position$match$jacoby) {
    abort_invalid_position_state(
      "Jacoby cannot be enabled in match play.",
      "match_jacoby"
    )
  }

  if (position$match$length == 0L && position$match$crawford) {
    abort_invalid_position_state(
      "Crawford cannot be enabled in a money game.",
      "money_crawford"
    )
  }

  if (!is_named_record(position$source, c("format", "identifier", "fingerprint"))) {
    abort_invalid_position_state("The canonical source record is malformed.", "source_record")
  }

  if (
    !is_single_character_choice(position$source$format, c("GNUID", "XGID")) ||
      !is_single_nonempty_character(position$source$identifier) ||
      !is_single_nonempty_character(position$source$fingerprint)
  ) {
    abort_invalid_position_state("The canonical source metadata is invalid.", "source")
  }

  if (identical(position$source$format, "GNUID")) {
    if (!all(c("position_id", "match_id") %in% names(position$source))) {
      abort_invalid_position_state(
        "A GNUID source record must preserve its Position ID and Match ID parts.",
        "source_parts"
      )
    }

    if (
      !is_single_nonempty_character(position$source$position_id) ||
        !is_single_nonempty_character(position$source$match_id)
    ) {
      abort_invalid_position_state(
        "The preserved GNU source parts are malformed.",
        "source_parts"
      )
    }
  }

  provenance_fields <- c(
    "source_format",
    "canonical_state_version",
    "implementation",
    "upstream_reference",
    "attribution_file"
  )

  if (
    !is_named_record(position$provenance, provenance_fields) ||
      !is_single_character_choice(
        position$provenance$source_format,
        c("GNUID", "XGID")
      ) ||
      !identical(
        position$provenance$canonical_state_version,
        .position_state_version
      ) ||
      !all(vapply(
        position$provenance[c(
          "implementation",
          "upstream_reference",
          "attribution_file"
        )],
        is_single_nonempty_character,
        logical(1)
      ))
  ) {
    abort_invalid_position_state(
      "The canonical provenance record is malformed.",
      "provenance"
    )
  }

  normalization_fields <- c(
    "canonical_state_version",
    "stable_player_mapping",
    "physical_point_mapping",
    "source_orientation",
    "xgid_max_cube_exponent",
    "source_normalized_fields",
    "normalized_fields",
    "lossy_fields",
    "lossy_fields_by_target",
    "normalization_notes"
  )
  normalization <- position$normalization

  if (!is_named_record(normalization, normalization_fields)) {
    abort_invalid_position_state(
      "The canonical normalization record is malformed.",
      "normalization"
    )
  }

  target_loss <- normalization$lossy_fields_by_target

  if (
    !identical(normalization$canonical_state_version, .position_state_version) ||
      !all(vapply(
        normalization[c(
          "stable_player_mapping",
          "physical_point_mapping",
          "source_orientation"
        )],
        is_single_nonempty_character,
        logical(1)
      )) ||
      !is_single_integer_in_range(
        normalization$xgid_max_cube_exponent,
        0L,
        15L
      ) ||
      !all(vapply(
        normalization[c(
          "source_normalized_fields",
          "normalized_fields",
          "lossy_fields",
          "normalization_notes"
        )],
        is_character_vector_without_na,
        logical(1)
      )) ||
      !is.list(target_loss) ||
      !identical(names(target_loss), c("GNUID", "XGID")) ||
      !all(vapply(
        target_loss,
        is_character_vector_without_na,
        logical(1)
      ))
  ) {
    abort_invalid_position_state(
      "The canonical normalization record is malformed.",
      "normalization"
    )
  }

  invisible(position)
}

validate_match_rules <- function(
    match_length,
    score,
    crawford,
    cube_exponent,
    cube_owner,
    invalid_class,
    argument) {
  if (match_length == 0L) {
    if (crawford) {
      abort_identifier(
        "Crawford cannot be enabled in a money game.",
        class = invalid_class,
        argument = argument,
        rule = "money_game_crawford"
      )
    }
    return(invisible(TRUE))
  }

  if (any(score >= match_length)) {
    abort_identifier(
      "An active-match score must be below match length.",
      class = invalid_class,
      argument = argument,
      rule = "active_match_score"
    )
  }

  if (crawford) {
    one_away <- score == match_length - 1L

    if (sum(one_away) != 1L) {
      abort_identifier(
        paste0(
          "Crawford requires exactly one stable player to be one point ",
          "from the match."
        ),
        class = invalid_class,
        argument = argument,
        rule = "crawford_score"
      )
    }

    if (cube_exponent != 0L || !identical(cube_owner, "centered")) {
      abort_identifier(
        "A Crawford game must use a centered 1-cube.",
        class = invalid_class,
        argument = argument,
        rule = "crawford_cube"
      )
    }
  }

  invisible(TRUE)
}

is_named_record <- function(value, required_names) {
  is.list(value) && all(required_names %in% names(value))
}

is_single_nonempty_character <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

is_single_character_choice <- function(value, choices) {
  is_single_nonempty_character(value) && value %in% choices
}

is_character_vector_without_na <- function(value) {
  is.character(value) && all(!is.na(value))
}

is_single_logical <- function(value) {
  is.logical(value) && length(value) == 1L && !is.na(value)
}

is_single_finite_number <- function(value) {
  is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value)
}

is_single_nonnegative_integer <- function(value) {
  is_single_finite_number(value) &&
    value >= 0L &&
    value == floor(value)
}

is_single_integer_in_range <- function(value, minimum, maximum) {
  is_single_nonnegative_integer(value) &&
    value >= minimum &&
    value <= maximum
}

is_integer_vector_in_range <- function(value, expected_length, minimum, maximum) {
  is.numeric(value) &&
    length(value) == expected_length &&
    all(!is.na(value)) &&
    all(is.finite(value)) &&
    all(value == floor(value)) &&
    all(value >= minimum) &&
    all(value <= maximum)
}

validate_supported_checker_state <- function(players, argument) {
  off_counts <- vapply(
    c("player_0", "player_1"),
    function(player) as.integer(players[[player]]$off),
    integer(1)
  )

  if (any(off_counts == 15L)) {
    abort_identifier(
      paste0(
        "The checker state represents a completed game, which is outside ",
        "the supported v1 canonical position contract."
      ),
      class = "backgammoncalculator_unsupported_identifier_state",
      argument = argument,
      rule = "terminal_checker_state"
    )
  }

  invisible(TRUE)
}

position_state_fingerprint <- function(position) {
  paste0(
    c(
      unname(position$players$player_0$points),
      position$players$player_0$bar,
      position$players$player_0$off,
      unname(position$players$player_1$points),
      position$players$player_1$bar,
      position$players$player_1$off,
      position$turn$dice_owner,
      position$turn$turn_owner,
      position$turn$action,
      if (length(position$turn$dice) == 0L) "none" else position$turn$dice,
      position$cube$exponent,
      position$cube$owner,
      position$cube$max_exponent,
      unname(position$score),
      position$match$length,
      position$match$crawford,
      position$match$jacoby,
      position$match$game_state,
      position$match$game_state_code
    ),
    collapse = "|"
  )
}

identifier_conversion_assessment <- function(position, target_format) {
  if (!target_format %in% c("GNUID", "XGID")) {
    abort_identifier(
      "Internal identifier target format is unsupported.",
      class = "backgammoncalculator_identifier_conversion_error",
      argument = NA_character_,
      rule = "target_format"
    )
  }

  normalized_fields <- character()
  lossy_fields <- character()
  notes <- character()

  if (identical(target_format, "GNUID")) {
    normalized_max_exponent <- max(
      .xgid_default_max_cube_exponent,
      position$cube$exponent
    )

    if (position$cube$max_exponent != normalized_max_exponent) {
      normalized_fields <- c(normalized_fields, "cube.max_exponent")
      lossy_fields <- c(lossy_fields, "cube.max_exponent")
      notes <- c(
        notes,
        paste0(
          "GNUID does not store the XGID maximum-cube exponent; it is ",
          "normalized from the current cube exponent."
        )
      )
    }

    if (
      identical(position$turn$action, "roll") &&
        length(position$turn$dice) == 2L &&
        position$turn$dice[[1L]] < position$turn$dice[[2L]]
    ) {
      normalized_fields <- c(normalized_fields, "turn.dice_order")
      notes <- c(
        notes,
        "GNU Match IDs canonicalize a non-double roll to descending die order."
      )
    }
  } else if (position$match$game_state_code == 0L) {
    normalized_fields <- c(normalized_fields, "match.game_state")
    lossy_fields <- c(lossy_fields, "match.game_state")
    notes <- c(
      notes,
      paste0(
        "XGID does not encode GNU's not_started versus playing distinction; ",
        "not_started is normalized to playing through XGID."
      )
    )
  }

  list(
    target_format = target_format,
    normalized_fields = unique(normalized_fields),
    lossy_fields = unique(lossy_fields),
    notes = unique(notes)
  )
}

build_normalization_metadata <- function(position) {
  source_format <- position$source$format
  to_gnu <- identifier_conversion_assessment(position, "GNUID")
  to_xgid <- identifier_conversion_assessment(position, "XGID")
  source_normalized_fields <- if (identical(source_format, "GNUID")) {
    "cube.max_exponent"
  } else {
    "match.game_state"
  }
  source_notes <- if (identical(source_format, "GNUID")) {
    paste0(
      "GNUID does not store the XGID maximum-cube exponent; the canonical ",
      "state uses max(10, current cube exponent)."
    )
  } else {
    paste0(
      "XGID does not store a GNU game-state code; the canonical state uses ",
      "playing."
    )
  }

  list(
    canonical_state_version = .position_state_version,
    stable_player_mapping = paste0(
      "player_0=lowercase XGID; player_1=uppercase XGID"
    ),
    physical_point_mapping = "points 1:24 are XGID checker slots 2:25",
    source_orientation = if (identical(source_format, "GNUID")) {
      "GNU rows relative to dice owner"
    } else {
      "XGID fixed top and bottom player mapping"
    },
    xgid_max_cube_exponent = position$cube$max_exponent,
    source_normalized_fields = source_normalized_fields,
    normalized_fields = unique(c(
      source_normalized_fields,
      to_gnu$normalized_fields,
      to_xgid$normalized_fields
    )),
    lossy_fields = unique(c(to_gnu$lossy_fields, to_xgid$lossy_fields)),
    lossy_fields_by_target = list(
      GNUID = to_gnu$lossy_fields,
      XGID = to_xgid$lossy_fields
    ),
    normalization_notes = unique(c(source_notes, to_gnu$notes, to_xgid$notes))
  )
}

reusable_source_identifier <- function(position, target_format) {
  source <- position$source
  current_fingerprint <- position_state_fingerprint(position)

  if (
    !identical(source$format, target_format) ||
      !identical(source$fingerprint, current_fingerprint) ||
      !is_single_nonempty_character(source$identifier)
  ) {
    return(NULL)
  }

  decoded <- tryCatch(
    if (identical(target_format, "GNUID")) {
      position_from_gnuid(source$identifier)
    } else {
      position_from_xgid(source$identifier)
    },
    error = function(error) NULL
  )

  if (
    is.null(decoded) ||
      !identical(position_state_fingerprint(decoded), current_fingerprint)
  ) {
    return(NULL)
  }

  source$identifier
}

gnuid_lossy_fields <- function(position) {
  identifier_conversion_assessment(position, "GNUID")$lossy_fields
}

xgid_lossy_fields <- function(position) {
  identifier_conversion_assessment(position, "XGID")$lossy_fields
}

identifier_provenance <- function(source_format) {
  list(
    source_format = source_format,
    canonical_state_version = .position_state_version,
    implementation = "backgammoncalculator local base-R conversion",
    upstream_reference = paste0(
      "lassehjorthmadsen/bglab 0.0.0.9000, R/posid2xgid.R, ",
      "documentation snapshot built 2025-01-25"
    ),
    attribution_file = "GNUID_XGID_ATTRIBUTION.md"
  )
}

other_player <- function(player) {
  if (identical(player, "player_0")) "player_1" else "player_0"
}

abort_invalid_xgid <- function(message, rule) {
  abort_identifier(
    message,
    class = "backgammoncalculator_invalid_xgid",
    argument = "xgid",
    rule = rule
  )
}

abort_invalid_position_state <- function(message, rule) {
  abort_identifier(
    message,
    class = "backgammoncalculator_invalid_position_state",
    argument = "position",
    rule = rule
  )
}

abort_identifier <- function(
    message,
    class,
    argument,
    rule,
    fields = character()) {
  condition <- structure(
    list(
      message = message,
      call = NULL,
      argument = argument,
      rule = rule,
      fields = fields
    ),
    class = c(
      class,
      "backgammoncalculator_identifier_error",
      "backgammoncalculator_error",
      "error",
      "condition"
    )
  )

  stop(condition)
}
