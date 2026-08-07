test_that("complete GNUID parses to the accepted canonical state", {
  state <- position_from_gnuid(
    "4HPwATDgc/ABMA:8IhuACAACAAE"
  )

  expect_s3_class(state, "backgammon_position_state")
  expect_identical(state$turn$dice_owner, "player_1")
  expect_identical(state$turn$turn_owner, "player_1")
  expect_identical(state$turn$action, "roll")
  expect_identical(state$turn$dice, c(5L, 3L))
  expect_identical(state$cube$exponent, 0L)
  expect_identical(state$cube$value, 1)
  expect_identical(state$cube$owner, "centered")
  expect_identical(state$score, c(player_0 = 2L, player_1 = 1L))
  expect_identical(state$match$length, 3L)
  expect_true(state$match$crawford)
  expect_false(state$match$jacoby)
  expect_identical(state$match$game_state, "not_started")

  expect_identical(state$players$player_0$points[[1L]], 2L)
  expect_identical(state$players$player_0$points[[12L]], 5L)
  expect_identical(state$players$player_0$points[[17L]], 3L)
  expect_identical(state$players$player_0$points[[19L]], 5L)
  expect_identical(state$players$player_1$points[[6L]], 5L)
  expect_identical(state$players$player_1$points[[8L]], 3L)
  expect_identical(state$players$player_1$points[[13L]], 5L)
  expect_identical(state$players$player_1$points[[24L]], 2L)
  expect_identical(state$players$player_0$bar, 0L)
  expect_identical(state$players$player_1$bar, 0L)
  expect_identical(state$players$player_0$off, 0L)
  expect_identical(state$players$player_1$off, 0L)
})

test_that("XGID parses to the same stable-player model", {
  xgid <- paste0(
    "XGID=-b----E-C---eE---c-e----B-",
    ":0:0:1:53:1:2:1:3:10"
  )
  state <- position_from_xgid(xgid)

  expect_identical(state$source$identifier, xgid)
  expect_identical(state$turn$dice_owner, "player_1")
  expect_identical(state$score, c(player_0 = 2L, player_1 = 1L))
  expect_identical(state$players$player_0$points[[1L]], 2L)
  expect_identical(state$players$player_1$points[[24L]], 2L)
  expect_identical(state$match$game_state, "playing")
})

test_that("canonical encoders and convenience conversions use canonical state", {
  complete <- "4HPwATDgc/ABMA:8IhuACAACAAE"
  expected_xgid <- paste0(
    "XGID=-b----E-C---eE---c-e----B-",
    ":0:0:1:53:1:2:1:3:10"
  )

  state <- position_from_gnuid(complete)
  expect_identical(gnuid_from_position(state), complete)
  expect_identical(xgid_from_position(state), expected_xgid)
  expect_identical(gnuid_to_xgid(complete), expected_xgid)
  expect_identical(
    gnuid_to_xgid("4HPwATDgc/ABMA", "8IhuACAACAAE"),
    expected_xgid
  )

  encoded <- xgid_to_gnuid(expected_xgid)
  expect_identical(encoded, "4HPwATDgc/ABMA:8IluACAACAAE")
  expect_identical(gnuid_to_xgid(encoded), expected_xgid)
})

test_that("exact source identifiers, provenance, and normalization are retained", {
  complete <- "4HPwATDgc/ABMA:8IhuACAACAAE"
  state <- position_from_gnuid(complete)

  expect_identical(state$source$format, "GNUID")
  expect_identical(state$source$identifier, complete)
  expect_identical(state$source$position_id, "4HPwATDgc/ABMA")
  expect_identical(state$source$match_id, "8IhuACAACAAE")
  expect_true(nzchar(state$source$fingerprint))
  expect_identical(state$provenance$source_format, "GNUID")
  expect_match(state$provenance$upstream_reference, "bglab")
  expect_identical(state$normalization$canonical_state_version, "1.0")
  expect_match(state$normalization$stable_player_mapping, "player_0")
  expect_identical(gnuid_from_position(state), complete)

  xgid <- test_make_xgid(max_cube_exponent = 8L)
  xstate <- position_from_xgid(xgid)
  expect_identical(xgid_from_position(xstate), xgid)
  expect_identical(xstate$source$identifier, xgid)
  expect_identical(xstate$normalization$xgid_max_cube_exponent, 8L)
})

test_that("regression family proves DiceOwner, TurnOwner, and cube-owner mapping", {
  position_id <- "ewMAAD4gAAAAAA"
  aq <- "AQGqAAAAAAAE"
  eq <- "EQGqAAAAAAAE"
  uq <- "UQmqAAAAAAAE"

  expect_identical(
    gnuid_to_xgid(position_id, aq),
    paste0(
      "XGID=-BDB-------------a------e-",
      ":1:-1:-1:42:0:0:0:5:10"
    )
  )
  expect_identical(
    gnuid_to_xgid(position_id, eq),
    paste0(
      "XGID=-BDB-------------a------e-",
      ":1:1:-1:42:0:0:0:5:10"
    )
  )
  expect_identical(
    gnuid_to_xgid(position_id, uq),
    paste0(
      "XGID=-E------A-------------bdb-",
      ":1:1:1:42:0:0:0:5:10"
    )
  )

  aq_state <- position_from_gnuid(paste0(position_id, ":", aq))
  eq_state <- position_from_gnuid(paste0(position_id, ":", eq))
  uq_state <- position_from_gnuid(paste0(position_id, ":", uq))

  expect_identical(aq_state$turn$dice_owner, "player_0")
  expect_identical(eq_state$turn$dice_owner, "player_0")
  expect_identical(uq_state$turn$dice_owner, "player_1")
  expect_identical(aq_state$cube$owner, "player_0")
  expect_identical(eq_state$cube$owner, "player_1")
  expect_identical(uq_state$cube$owner, "player_1")
  expect_identical(aq_state$turn$turn_owner, "player_0")
  expect_identical(uq_state$turn$turn_owner, "player_1")
})

test_that("accepted XGID regression converts to GNU only with explicit lossiness", {
  xgid <- paste0(
    "XGID=-BDB-------------a------e-",
    ":1:-1:-1:42:0:0:0:5:8"
  )

  expect_error(
    xgid_to_gnuid(xgid),
    class = "backgammoncalculator_unsupported_identifier_state"
  )
  expect_identical(
    xgid_to_gnuid(xgid, allow_lossy = TRUE),
    "ewMAAD4gAAAAAA:AQGqAAAAAAAE"
  )

  high_cube <- test_make_xgid(
    cube_exponent = 12L,
    max_cube_exponent = 12L
  )
  expect_silent(xgid_to_gnuid(high_cube))
})

test_that("bars and borne-off counts use fixed physical points", {
  checker_component <- paste0("a", strrep("-", 24L), "B")
  state <- position_from_xgid(
    test_make_xgid(checker_component = checker_component)
  )

  expect_identical(state$players$player_0$bar, 1L)
  expect_identical(state$players$player_1$bar, 2L)
  expect_identical(state$players$player_0$off, 14L)
  expect_identical(state$players$player_1$off, 13L)
  expect_true(all(state$players$player_0$points == 0L))
  expect_true(all(state$players$player_1$points == 0L))

  encoded <- xgid_to_gnuid(xgid_from_position(state))
  reparsed <- position_from_gnuid(encoded)
  expect_equal(test_semantic_state(reparsed), test_semantic_state(state))
})

test_that("asymmetric score, match length, Crawford, and dice are preserved", {
  xgid <- test_make_xgid(
    turn = -1L,
    dice_or_action = "62",
    score = c(player_0 = 8L, player_1 = 2L),
    rule = 1L,
    match_length = 9L
  )
  state <- position_from_xgid(xgid)

  expect_identical(state$score, c(player_0 = 8L, player_1 = 2L))
  expect_identical(state$match$length, 9L)
  expect_true(state$match$crawford)
  expect_identical(state$turn$dice_owner, "player_0")
  expect_identical(state$turn$dice, c(6L, 2L))

  complete <- xgid_to_gnuid(xgid)
  expect_identical(gnuid_to_xgid(complete), xgid)
})

test_that("pending doubles preserve offerer and decision owner", {
  xgid <- test_make_xgid(
    cube_exponent = 1L,
    cube_owner = -1L,
    turn = -1L,
    dice_or_action = "D",
    score = c(player_0 = 3L, player_1 = 1L),
    match_length = 7L
  )
  state <- position_from_xgid(xgid)

  expect_identical(state$turn$action, "double")
  expect_identical(state$turn$dice_owner, "player_0")
  expect_identical(state$turn$turn_owner, "player_1")
  expect_length(state$turn$dice, 0L)

  complete <- xgid_to_gnuid(xgid)
  reparsed <- position_from_gnuid(complete)
  expect_identical(reparsed$turn$action, "double")
  expect_identical(reparsed$turn$dice_owner, "player_0")
  expect_identical(reparsed$turn$turn_owner, "player_1")
  expect_identical(gnuid_to_xgid(complete), xgid)
})

test_that("cube values 1 through 64 and all owners survive conversion", {
  for (cube_exponent in 0:6) {
    for (cube_owner in c(-1L, 0L, 1L)) {
      xgid <- test_make_xgid(
        cube_exponent = cube_exponent,
        cube_owner = cube_owner,
        max_cube_exponent = 10L
      )
      complete <- xgid_to_gnuid(xgid)
      state <- position_from_gnuid(complete)

      expect_identical(state$cube$exponent, as.integer(cube_exponent))
      expect_identical(state$cube$value, 2^cube_exponent)
      expect_identical(
        state$cube$owner,
        switch(
          as.character(cube_owner),
          `-1` = "player_0",
          `0` = "centered",
          `1` = "player_1"
        )
      )
      expect_identical(gnuid_to_xgid(complete), xgid)
    }
  }
})

test_that("same GNU Position ID with different Match IDs changes only factual match mapping", {
  position_id <- "ewMAAD4gAAAAAA"
  player_0 <- position_from_gnuid(
    position_id,
    "AQGqAAAAAAAE"
  )
  player_1 <- position_from_gnuid(
    position_id,
    "UQmqAAAAAAAE"
  )

  expect_false(identical(player_0$players, player_1$players))
  expect_identical(player_0$turn$dice_owner, "player_0")
  expect_identical(player_1$turn$dice_owner, "player_1")
  expect_identical(player_0$turn$dice, player_1$turn$dice)
  expect_identical(player_0$score, player_1$score)
  expect_identical(player_0$match$length, player_1$match$length)
})

test_that("invalid complete GNUID and XGID inputs fail with stable classes", {
  invalid_gnuid <- list(
    NA_character_,
    "",
    "4HPwATDgc/ABMA",
    "4HPwATDgc/ABMA 8IhuACAACAAE",
    "Position ID: 4HPwATDgc/ABMA:8IhuACAACAAE",
    c(
      "4HPwATDgc/ABMA:8IhuACAACAAE",
      "4HPwATDgc/ABMA:8IhuACAACAAE"
    )
  )

  for (value in invalid_gnuid) {
    expect_error(
      position_from_gnuid(value),
      class = "backgammoncalculator_invalid_complete_gnuid"
    )
  }

  expect_error(
    position_from_gnuid("4HPwATDgc/ABMB:8IhuACAACAAE"),
    class = "backgammoncalculator_invalid_gnu_position_id"
  )
  expect_error(
    position_from_gnuid("4HPwATDgc/ABMA:8IhuACAACAA!"),
    class = "backgammoncalculator_invalid_complete_gnuid"
  )

  invalid_xgid <- c(
    "",
    "-b----E-C---eE---c-e----B-:0:0:1:53:1:2:1:3:10",
    "XGID=short:0:0:1:00:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----B-:0:0:0:00:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----B-:0:0:1:70:0:0:0:0:10",
    "XGID=-b----E-C---eE---c-e----B-:5:0:1:00:0:0:0:0:4"
  )

  for (value in invalid_xgid) {
    expect_error(
      position_from_xgid(value),
      class = "backgammoncalculator_identifier_error"
    )
  }
})

test_that("unsupported GNU and XGID states fail explicitly", {
  unsupported_matches <- c(
    test_encode_match_id(game_state = 2L),
    test_encode_match_id(game_state = 3L),
    test_encode_match_id(game_state = 4L),
    test_encode_match_id(resignation_value = 1L)
  )

  for (match_id in unsupported_matches) {
    expect_error(
      position_from_gnuid("4HPwATDgc/ABMA", match_id),
      class = "backgammoncalculator_unsupported_identifier_state"
    )
  }

  expect_error(
    position_from_gnuid(
      "4HPwATDgc/ABMA",
      test_encode_match_id(reserved_bits = 1L)
    ),
    class = "backgammoncalculator_invalid_gnu_match_id"
  )

  expect_error(
    position_from_xgid(
      test_make_xgid(dice_or_action = "R")
    ),
    class = "backgammoncalculator_unsupported_identifier_state"
  )
})

test_that("regression records and attribution metadata are installed", {
  fixture_path <- test_package_file(
    "extdata",
    "identifier_regressions.csv"
  )
  attribution_path <- test_package_file(
    "GNUID_XGID_ATTRIBUTION.md"
  )
  license_path <- test_package_file(
    "licenses",
    "bglab-MIT.txt"
  )

  expect_true(nzchar(fixture_path))
  expect_true(nzchar(attribution_path))
  expect_true(nzchar(license_path))

  records <- utils::read.csv(fixture_path, stringsAsFactors = FALSE)
  expect_true(all(c("UQmqAAAAAAAE", "EQGqAAAAAAAE", "AQGqAAAAAAAE") %in% records$match_id))
  expect_true(all(nzchar(records$source_of_truth)))
  expect_true(all(nzchar(records$relationship)))

  attribution <- paste(readLines(attribution_path, warn = FALSE), collapse = "\n")
  license <- paste(readLines(license_path, warn = FALSE), collapse = "\n")
  expect_match(attribution, "R/posid2xgid.R", fixed = TRUE)
  expect_match(attribution, "0.0.0.9000", fixed = TRUE)
  expect_match(attribution, "material local changes", ignore.case = TRUE)
  expect_match(license, "MIT License", fixed = TRUE)
})
