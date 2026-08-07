test_that("exact source reuse verifies format and factual identity", {
  complete <- "4HPwATDgc/ABMA:8IhuACAACAAE"
  expected_xgid <- paste0(
    "XGID=-b----E-C---eE---c-e----B-",
    ":0:0:1:53:1:2:1:3:10"
  )

  bogus <- position_from_gnuid(complete)
  bogus$source$identifier <- "bogus"
  expect_identical(gnuid_from_position(bogus), complete)

  wrong_valid_source <- position_from_gnuid(complete)
  wrong_valid_source$source$identifier <- paste0(
    "ewMAAD4gAAAAAA:AQGqAAAAAAAE"
  )
  expect_identical(gnuid_from_position(wrong_valid_source), complete)

  wrong_format <- position_from_gnuid(complete)
  wrong_format$source$format <- "XGID"
  expect_identical(xgid_from_position(wrong_format), expected_xgid)

  stale_fingerprint <- position_from_gnuid(complete)
  stale_fingerprint$source$fingerprint <- "stale"
  expect_identical(gnuid_from_position(stale_fingerprint), complete)
})

test_that("normalization metadata uses the same loss assessment as conversion", {
  complete <- "4HPwATDgc/ABMA:8IhuACAACAAE"
  state <- position_from_gnuid(complete)

  expect_true("cube.max_exponent" %in% state$normalization$normalized_fields)
  expect_true("match.game_state" %in% state$normalization$normalized_fields)
  expect_true("match.game_state" %in% state$normalization$lossy_fields)
  expect_identical(
    state$normalization$lossy_fields_by_target$GNUID,
    character()
  )
  expect_identical(
    state$normalization$lossy_fields_by_target$XGID,
    "match.game_state"
  )
  expect_identical(gnuid_from_position(state), complete)

  xgid <- xgid_from_position(state)
  normalized_gnuid <- xgid_to_gnuid(xgid)
  normalized_state <- position_from_gnuid(normalized_gnuid)
  expect_identical(normalized_state$match$game_state, "playing")
  expect_identical(normalized_state$match$game_state_code, 1L)

  max8 <- position_from_xgid(test_make_xgid(max_cube_exponent = 8L))
  expect_true("cube.max_exponent" %in% max8$normalization$lossy_fields)
  expect_identical(
    max8$normalization$lossy_fields_by_target$GNUID,
    "cube.max_exponent"
  )
  max8_error <- tryCatch(
    gnuid_from_position(max8),
    error = identity
  )
  expect_s3_class(
    max8_error,
    "backgammoncalculator_unsupported_identifier_state"
  )
  expect_identical(
    max8_error$fields,
    max8$normalization$lossy_fields_by_target$GNUID
  )
})

test_that("GNU Match ID encoding canonicalizes dice to descending order", {
  ascending <- test_make_xgid(dice_or_action = "26")
  descending <- test_make_xgid(dice_or_action = "62")
  expected <- "4HPwATDgc/ABMA:cAkLAAAAAAAE"

  expect_identical(xgid_to_gnuid(ascending), expected)
  expect_identical(xgid_to_gnuid(descending), expected)
  expect_identical(
    test_encode_match_id(dice = c(6L, 2L)),
    "cAkLAAAAAAAE"
  )

  ascending_state <- position_from_xgid(ascending)
  expect_true(
    "turn.dice_order" %in% ascending_state$normalization$normalized_fields
  )
  expect_false(
    "turn.dice_order" %in% ascending_state$normalization$lossy_fields
  )
})

test_that("malformed canonical state fails with stable package conditions", {
  base <- position_from_gnuid("4HPwATDgc/ABMA:8IhuACAACAAE")

  mutations <- list(
    missing_turn = function(position) {
      position$turn <- NULL
      position
    },
    missing_dice_owner = function(position) {
      position$turn$dice_owner <- NULL
      position
    },
    vector_dice_owner = function(position) {
      position$turn$dice_owner <- c("player_0", "player_1")
      position
    },
    nan_die = function(position) {
      position$turn$dice <- c(5, NaN)
      position
    },
    infinite_bar = function(position) {
      position$players$player_0$bar <- Inf
      position
    },
    missing_points = function(position) {
      position$players$player_0$points <- NULL
      position
    },
    infinite_cube = function(position) {
      position$cube$value <- Inf
      position
    },
    vector_cube_owner = function(position) {
      position$cube$owner <- c("centered", "player_0")
      position
    },
    nan_score = function(position) {
      position$score[[1L]] <- NaN
      position
    },
    missing_match_length = function(position) {
      position$match$length <- NULL
      position
    },
    invalid_game_state = function(position) {
      position$match$game_state <- c("playing", "not_started")
      position
    },
    missing_source_fingerprint = function(position) {
      position$source$fingerprint <- NULL
      position
    },
    empty_provenance = function(position) {
      position$provenance <- list()
      position
    },
    malformed_provenance_field = function(position) {
      position$provenance$source_format <- c("GNUID", "XGID")
      position
    },
    missing_normalization = function(position) {
      position$normalization <- NULL
      position
    },
    empty_normalization = function(position) {
      position$normalization <- list()
      position
    },
    malformed_normalization_field = function(position) {
      position$normalization$lossy_fields <- 1L
      position
    },
    malformed_target_loss = function(position) {
      position$normalization$lossy_fields_by_target <- list(GNUID = character())
      position
    }
  )

  encoders <- list(
    gnuid = gnuid_from_position,
    xgid = xgid_from_position
  )

  for (mutation_name in names(mutations)) {
    malformed <- mutations[[mutation_name]](base)

    for (encoder_name in names(encoders)) {
      error <- tryCatch(
        encoders[[encoder_name]](malformed),
        error = identity
      )

      expect_s3_class(
        error,
        "backgammoncalculator_invalid_position_state"
      )
      expect_false(inherits(error, "simpleError"))
    }
  }
})

test_that("terminal checker states are unsupported in the v1 canonical model", {
  terminal_xgid <- test_make_xgid(checker_component = strrep("-", 26L))

  expect_error(
    position_from_xgid(terminal_xgid),
    class = "backgammoncalculator_unsupported_identifier_state"
  )

  terminal_match <- test_encode_match_id(game_state = 1L)
  expect_error(
    position_from_gnuid("AAAAAAAAAAAAAA", terminal_match),
    class = "backgammoncalculator_unsupported_identifier_state"
  )

  terminal_state <- position_from_gnuid(
    "4HPwATDgc/ABMA:8IhuACAACAAE"
  )
  terminal_state$players$player_0$points[] <- 0L
  terminal_state$players$player_0$bar <- 0L
  terminal_state$players$player_0$off <- 15L

  expect_error(
    gnuid_from_position(terminal_state),
    class = "backgammoncalculator_unsupported_identifier_state"
  )
  expect_error(
    xgid_from_position(terminal_state),
    class = "backgammoncalculator_unsupported_identifier_state"
  )
})
