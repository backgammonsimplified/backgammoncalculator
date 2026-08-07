test_gnu_base64_alphabet <- c(
  LETTERS,
  letters,
  as.character(0:9),
  "+",
  "/"
)

test_make_xgid <- function(
    checker_component = "-b----E-C---eE---c-e----B-",
    cube_exponent = 0L,
    cube_owner = 0L,
    turn = 1L,
    dice_or_action = "00",
    score = c(player_0 = 0L, player_1 = 0L),
    rule = 0L,
    match_length = 0L,
    max_cube_exponent = 10L) {
  paste0(
    "XGID=",
    checker_component,
    ":",
    paste0(
      c(
        cube_exponent,
        cube_owner,
        turn,
        dice_or_action,
        score[["player_1"]],
        score[["player_0"]],
        rule,
        match_length,
        max_cube_exponent
      ),
      collapse = ":"
    )
  )
}

test_semantic_state <- function(position) {
  list(
    players = position$players,
    turn = position$turn,
    cube = position$cube,
    score = position$score,
    match = position$match
  )
}

test_bits_to_bytes <- function(bits) {
  starts <- seq.int(1L, length(bits), by = 8L)

  vapply(
    starts,
    function(start) {
      chunk <- bits[start:(start + 7L)]
      as.integer(sum(chunk * 2^(0:7)))
    },
    integer(1)
  )
}

test_base64_encode_bytes <- function(bytes) {
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
    final <- tail(bytes, 1L)
    values <- c(
      values,
      final[[1L]] %/% 4L,
      (final[[1L]] %% 4L) * 16L
    )
  } else if (remainder == 2L) {
    final <- tail(bytes, 2L)
    values <- c(
      values,
      final[[1L]] %/% 4L,
      (final[[1L]] %% 4L) * 16L + final[[2L]] %/% 16L,
      (final[[2L]] %% 16L) * 4L
    )
  }

  paste0(test_gnu_base64_alphabet[values + 1L], collapse = "")
}

test_write_unsigned_bits <- function(bits, start, width, value) {
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

test_encode_match_id <- function(
    cube_exponent = 0L,
    cube_owner_code = 3L,
    dice_owner = 1L,
    crawford = FALSE,
    game_state = 1L,
    turn_owner = NULL,
    double_offered = FALSE,
    resignation_value = 0L,
    dice = c(0L, 0L),
    match_length = 0L,
    scores = c(player_0 = 0L, player_1 = 0L),
    jacoby = FALSE,
    reserved_bits = 0L) {
  if (is.null(turn_owner)) {
    turn_owner <- if (double_offered) 1L - dice_owner else dice_owner
  }

  bits <- integer(72L)
  fields <- list(
    c(0L, 4L, cube_exponent),
    c(4L, 2L, cube_owner_code),
    c(6L, 1L, dice_owner),
    c(7L, 1L, as.integer(crawford)),
    c(8L, 3L, game_state),
    c(11L, 1L, turn_owner),
    c(12L, 1L, as.integer(double_offered)),
    c(13L, 2L, resignation_value),
    c(15L, 3L, dice[[1L]]),
    c(18L, 3L, dice[[2L]]),
    c(21L, 15L, match_length),
    c(36L, 15L, scores[["player_0"]]),
    c(51L, 15L, scores[["player_1"]]),
    c(66L, 1L, as.integer(!jacoby)),
    c(67L, 5L, reserved_bits)
  )

  for (field in fields) {
    bits <- test_write_unsigned_bits(
      bits,
      start = field[[1L]],
      width = field[[2L]],
      value = field[[3L]]
    )
  }

  test_base64_encode_bytes(test_bits_to_bytes(bits))
}

test_package_file <- function(...) {
  parts <- list(...)
  installed <- do.call(
    system.file,
    c(parts, list(package = "backgammoncalculator"))
  )

  if (nzchar(installed)) {
    installed
  } else {
    do.call(
      testthat::test_path,
      c(list("..", "..", "inst"), parts)
    )
  }
}
