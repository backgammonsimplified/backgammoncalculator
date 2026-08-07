find_gnu_4ply_oracle <- function() {
  installed <- system.file(
    "extdata",
    "gnu_4ply_identifier_oracle.csv",
    package = "backgammoncalculator"
  )

  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }

  source_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "extdata",
    "gnu_4ply_identifier_oracle.csv"
  )

  if (!file.exists(source_path)) {
    stop("GNU 4-ply identifier oracle fixture was not found.", call. = FALSE)
  }

  source_path
}

parse_oracle_points <- function(value) {
  as.integer(strsplit(value, " ", fixed = TRUE)[[1L]])
}

oracle_player <- function(colour) {
  if (identical(colour, "O")) "player_0" else "player_1"
}

oracle_other_player <- function(player) {
  if (identical(player, "player_0")) "player_1" else "player_0"
}

oracle_cube_owner <- function(owner) {
  switch(
    owner,
    O = "player_0",
    X = "player_1",
    centered = "centered"
  )
}

oracle_semantic_state <- function(position) {
  list(
    players = list(
      player_0 = list(
        points = unname(position$players$player_0$points),
        bar = unname(position$players$player_0$bar),
        off = unname(position$players$player_0$off)
      ),
      player_1 = list(
        points = unname(position$players$player_1$points),
        bar = unname(position$players$player_1$bar),
        off = unname(position$players$player_1$off)
      )
    ),
    turn = list(
      dice_owner = position$turn$dice_owner,
      turn_owner = position$turn$turn_owner,
      action = position$turn$action,
      dice = unname(position$turn$dice)
    ),
    cube = list(
      exponent = unname(position$cube$exponent),
      value = unname(position$cube$value),
      owner = position$cube$owner,
      max_exponent = unname(position$cube$max_exponent)
    ),
    score = unname(position$score),
    match = list(
      length = unname(position$match$length),
      crawford = unname(position$match$crawford),
      jacoby = unname(position$match$jacoby)
    )
  )
}

test_that("3,802 GNU 4-ply review identifiers match the canonical model", {
  oracle <- utils::read.csv(
    find_gnu_4ply_oracle(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_identical(nrow(oracle), 3802L)
  expect_identical(length(unique(oracle$position_id)), 3485L)
  expect_identical(length(unique(oracle$match_id)), 1857L)
  expect_setequal(unique(oracle$action), c("roll", "cube_decision", "double"))
  expect_setequal(unique(oracle$cube_value), c(1L, 2L, 4L, 8L))
  expect_true(any(oracle$O_bar > 0L))
  expect_true(any(oracle$X_bar > 0L))
  expect_true(any(oracle$O_off > 0L))
  expect_true(any(oracle$X_off > 0L))
  expect_true(any(oracle$crawford == 1L))
  expect_true(any(oracle$score_o != oracle$score_x))

  expect_identical(sum(oracle$O_bar > 0L & oracle$X_bar > 0L), 6L)
  expect_identical(sum(oracle$crawford == 1L), 474L)
  expect_identical(sum(oracle$post_crawford == 1L), 345L)
  expect_identical(sum(oracle$O_off > 0L), 645L)
  expect_identical(sum(oracle$X_off > 0L), 655L)
  expect_identical(sum(oracle$O_off > 0L & oracle$X_off > 0L), 321L)
  expect_identical(sum(oracle$score_o != oracle$score_x), 2586L)
  expect_identical(unique(oracle$match_length), 7L)

  rolls <- oracle[oracle$action == "roll", , drop = FALSE]
  expect_identical(nrow(rolls), 3606L)
  expect_true(all(rolls$die1 >= rolls$die2))

  canonical_roll <- paste(
    pmax(rolls$die1, rolls$die2),
    pmin(rolls$die1, rolls$die2),
    sep = "-"
  )
  non_double <- rolls$die1 != rolls$die2
  expect_identical(length(unique(canonical_roll)), 21L)
  expect_identical(length(unique(canonical_roll[non_double])), 15L)
  expect_identical(length(unique(canonical_roll[!non_double])), 6L)

  failure_count <- 0L
  failure_examples <- character()

  record_failure <- function(index, field, actual, expected) {
    failure_count <<- failure_count + 1L

    if (length(failure_examples) < 20L) {
      actual_text <- paste(capture.output(dput(actual)), collapse = "")
      expected_text <- paste(capture.output(dput(expected)), collapse = "")
      failure_examples <<- c(
        failure_examples,
        sprintf(
          "row %d [%s:%s] %s: actual=%s expected=%s",
          index,
          oracle$position_id[[index]],
          oracle$match_id[[index]],
          field,
          actual_text,
          expected_text
        )
      )
    }

    invisible(FALSE)
  }

  check_identical <- function(index, field, actual, expected) {
    if (!identical(actual, expected)) {
      record_failure(index, field, actual, expected)
    }
  }

  for (index in seq_len(nrow(oracle))) {
    row <- oracle[index, , drop = FALSE]
    complete_gnuid <- paste0(row$position_id, ":", row$match_id)

    state <- tryCatch(
      position_from_gnuid(row$position_id, row$match_id),
      error = function(error) {
        record_failure(
          index,
          "position_from_gnuid error",
          conditionMessage(error),
          "successful conversion"
        )
        NULL
      }
    )

    if (is.null(state)) {
      next
    }

    expected_dice_owner <- oracle_player(row$actor_color)
    expected_turn_owner <- if (identical(row$action, "double")) {
      oracle_other_player(expected_dice_owner)
    } else {
      expected_dice_owner
    }
    expected_action <- if (identical(row$action, "double")) {
      "double"
    } else {
      "roll"
    }
    expected_dice <- if (identical(row$action, "roll")) {
      c(as.integer(row$die1), as.integer(row$die2))
    } else {
      integer()
    }

    check_identical(
      index,
      "player_0 points",
      unname(state$players$player_0$points),
      parse_oracle_points(row$O_points)
    )
    check_identical(
      index,
      "player_1 points",
      unname(state$players$player_1$points),
      parse_oracle_points(row$X_points)
    )
    check_identical(
      index,
      "player_0 bar",
      unname(state$players$player_0$bar),
      as.integer(row$O_bar)
    )
    check_identical(
      index,
      "player_1 bar",
      unname(state$players$player_1$bar),
      as.integer(row$X_bar)
    )
    check_identical(
      index,
      "player_0 off",
      unname(state$players$player_0$off),
      as.integer(row$O_off)
    )
    check_identical(
      index,
      "player_1 off",
      unname(state$players$player_1$off),
      as.integer(row$X_off)
    )
    check_identical(
      index,
      "dice owner",
      state$turn$dice_owner,
      expected_dice_owner
    )
    check_identical(
      index,
      "turn owner",
      state$turn$turn_owner,
      expected_turn_owner
    )
    check_identical(index, "action", state$turn$action, expected_action)
    check_identical(index, "dice", unname(state$turn$dice), expected_dice)
    check_identical(
      index,
      "cube value",
      unname(state$cube$value),
      as.numeric(row$cube_value)
    )
    check_identical(
      index,
      "cube owner",
      state$cube$owner,
      oracle_cube_owner(row$cube_owner)
    )
    check_identical(
      index,
      "score",
      unname(state$score),
      c(as.integer(row$score_o), as.integer(row$score_x))
    )
    check_identical(
      index,
      "match length",
      unname(state$match$length),
      as.integer(row$match_length)
    )
    check_identical(
      index,
      "Crawford",
      unname(state$match$crawford),
      as.logical(row$crawford)
    )
    check_identical(
      index,
      "exact source GNUID",
      state$source$identifier,
      complete_gnuid
    )
    check_identical(
      index,
      "exact GNUID re-encoding",
      gnuid_from_position(state),
      complete_gnuid
    )
    check_identical(
      index,
      "GNUID to XGID",
      xgid_from_position(state),
      row$expected_xgid
    )

    xgid_state <- tryCatch(
      position_from_xgid(row$expected_xgid),
      error = function(error) {
        record_failure(
          index,
          "position_from_xgid error",
          conditionMessage(error),
          "successful conversion"
        )
        NULL
      }
    )

    if (!is.null(xgid_state)) {
      check_identical(
        index,
        "XGID canonical state",
        oracle_semantic_state(xgid_state),
        oracle_semantic_state(state)
      )
    }
  }

  expect_identical(
    failure_count,
    0L,
    info = paste(failure_examples, collapse = "\n")
  )
})

test_that("the convenience and reverse paths cover a deterministic corpus sample", {
  oracle <- utils::read.csv(
    find_gnu_4ply_oracle(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  sample_indices <- sort(unique(c(
    seq.int(1L, nrow(oracle), by = 17L),
    which(oracle$action != "roll"),
    which(oracle$crawford == 1L),
    which(oracle$O_bar > 0L & oracle$X_bar > 0L),
    which(oracle$O_off > 0L & oracle$X_off > 0L)
  )))

  failures <- character()

  for (index in sample_indices) {
    row <- oracle[index, , drop = FALSE]

    actual_xgid <- gnuid_to_xgid(row$position_id, row$match_id)

    if (!identical(actual_xgid, row$expected_xgid)) {
      failures <- c(
        failures,
        sprintf("row %d forward conversion mismatch", index)
      )
      next
    }

    regenerated_gnuid <- xgid_to_gnuid(row$expected_xgid)
    regenerated_xgid <- gnuid_to_xgid(regenerated_gnuid)

    if (!identical(regenerated_xgid, row$expected_xgid)) {
      failures <- c(
        failures,
        sprintf("row %d reverse conversion mismatch", index)
      )
    }
  }

  expect_length(failures, 0L)
})
