test_that("leave_one_out_er recalculates each omission", {
  input <- data.frame(
    pair_id = rep(
      c("pair_01", "pair_02", "pair_03"),
      each = 2
    ),
    engine = rep(
      c("Sage", "GNU"),
      3
    ),
    loss = c(
      1.0,
      0.5,
      2.0,
      1.0,
      3.0,
      1.5
    ),
    decisions = rep(
      100,
      6
    )
  )

  observed <- leave_one_out_er(
    data = input,
    unit = "pair_id",
    engine = "engine",
    equity_lost = "loss",
    eligible_decisions = "decisions"
  )

  expect_identical(
    observed$omitted_value,
    c("pair_01", "pair_02", "pair_03")
  )

  expect_identical(
    observed$remaining_unit_count,
    c(2L, 2L, 2L)
  )

  expect_equal(
    observed$sage_equity_lost,
    c(5, 4, 3)
  )

  expect_equal(
    observed$gnu_equity_lost,
    c(2.5, 2, 1.5)
  )

  expect_equal(
    observed$sage_eligible_decisions,
    c(200, 200, 200)
  )

  expect_equal(
    observed$gnu_eligible_decisions,
    c(200, 200, 200)
  )

  expect_equal(
    observed$sage_er,
    c(12.5, 10, 7.5)
  )

  expect_equal(
    observed$gnu_er,
    c(6.25, 5, 3.75)
  )

  expect_equal(
    observed$er_difference_sage_minus_gnu,
    c(6.25, 5, 3.75)
  )

  expect_identical(
    observed$lower_er_engine,
    c("GNU", "GNU", "GNU")
  )
})

test_that("leave_one_out_er pools multiple rows per unit", {
  input <- data.frame(
    pair_id = rep(
      c("pair_01", "pair_02"),
      each = 4
    ),
    engine = rep(
      c("Sage", "Sage", "GNU", "GNU"),
      2
    ),
    loss = c(
      0.4,
      0.6,
      0.2,
      0.3,
      0.8,
      1.2,
      0.4,
      0.6
    ),
    decisions = rep(
      50,
      8
    )
  )

  observed <- leave_one_out_er(
    input,
    "pair_id",
    "engine",
    "loss",
    "decisions"
  )

  expect_equal(
    observed$sage_equity_lost,
    c(2, 1)
  )

  expect_equal(
    observed$gnu_equity_lost,
    c(1, 0.5)
  )

  expect_equal(
    observed$sage_er,
    c(10, 5)
  )

  expect_equal(
    observed$gnu_er,
    c(5, 2.5)
  )
})

test_that("leave_one_out_er reports ties", {
  input <- data.frame(
    pair_id = rep(
      c("pair_01", "pair_02"),
      each = 2
    ),
    engine = rep(
      c("Sage", "GNU"),
      2
    ),
    loss = c(
      1,
      1,
      2,
      2
    ),
    decisions = rep(
      100,
      4
    )
  )

  observed <- leave_one_out_er(
    input,
    "pair_id",
    "engine",
    "loss",
    "decisions"
  )

  expect_identical(
    observed$lower_er_engine,
    c("Tie", "Tie")
  )

  expect_identical(
    observed$er_difference_sage_minus_gnu,
    c(0, 0)
  )
})

test_that("leave_one_out_er retains missing engine results", {
  input <- data.frame(
    pair_id = c(
      "pair_01",
      "pair_02",
      "pair_02"
    ),
    engine = c(
      "Sage",
      "Sage",
      "GNU"
    ),
    loss = c(
      1,
      2,
      1
    ),
    decisions = c(
      100,
      100,
      100
    )
  )

  observed <- leave_one_out_er(
    input,
    "pair_id",
    "engine",
    "loss",
    "decisions"
  )

  pair_02_omitted <- observed$omitted_value ==
    "pair_02"

  expect_true(
    is.na(
      observed$gnu_er[
        pair_02_omitted
      ]
    )
  )

  expect_true(
    is.na(
      observed$er_difference_sage_minus_gnu[
        pair_02_omitted
      ]
    )
  )

  expect_true(
    is.na(
      observed$lower_er_engine[
        pair_02_omitted
      ]
    )
  )
})

test_that("leave_one_out_er does not discard missing metrics", {
  input <- data.frame(
    pair_id = rep(
      c("pair_01", "pair_02", "pair_03"),
      each = 2
    ),
    engine = rep(
      c("Sage", "GNU"),
      3
    ),
    loss = c(
      1,
      0.5,
      NA_real_,
      1,
      3,
      1.5
    ),
    decisions = rep(
      100,
      6
    )
  )

  observed <- leave_one_out_er(
    input,
    "pair_id",
    "engine",
    "loss",
    "decisions"
  )

  pair_01_omitted <- observed$omitted_value ==
    "pair_01"

  expect_true(
    is.na(
      observed$sage_equity_lost[
        pair_01_omitted
      ]
    )
  )

  expect_true(
    is.na(
      observed$sage_er[
        pair_01_omitted
      ]
    )
  )
})

test_that("leave_one_out_er ignores unrelated engines", {
  input <- data.frame(
    pair_id = c(
      "pair_01",
      "pair_01",
      "pair_01",
      "pair_02",
      "pair_02",
      "pair_02"
    ),
    engine = c(
      "Sage",
      "GNU",
      "Other",
      "Sage",
      "GNU",
      "Other"
    ),
    loss = c(
      1,
      0.5,
      100,
      2,
      1,
      100
    ),
    decisions = rep(
      100,
      6
    )
  )

  observed <- leave_one_out_er(
    input,
    "pair_id",
    "engine",
    "loss",
    "decisions"
  )

  expect_equal(
    observed$sage_equity_lost,
    c(2, 1)
  )

  expect_equal(
    observed$gnu_equity_lost,
    c(1, 0.5)
  )
})

test_that("leave_one_out_er supports custom engine labels", {
  input <- data.frame(
    unit = rep(
      c("one", "two"),
      each = 2
    ),
    engine = rep(
      c("Engine One", "Engine Two"),
      2
    ),
    loss = c(
      1,
      2,
      3,
      4
    ),
    decisions = rep(
      100,
      4
    )
  )

  observed <- leave_one_out_er(
    data = input,
    unit = "unit",
    engine = "engine",
    equity_lost = "loss",
    eligible_decisions = "decisions",
    left_engine = "Engine One",
    right_engine = "Engine Two"
  )

  expect_named(
    observed,
    c(
      "omitted_value",
      "remaining_unit_count",
      "engine_one_equity_lost",
      "engine_one_eligible_decisions",
      "engine_one_er",
      "engine_two_equity_lost",
      "engine_two_eligible_decisions",
      "engine_two_er",
      "er_difference_engine_one_minus_engine_two",
      "lower_er_engine"
    )
  )
})

test_that("leave_one_out_er validates inputs", {
  input <- data.frame(
    pair_id = c(
      "pair_01",
      "pair_02"
    ),
    engine = c(
      "Sage",
      "GNU"
    ),
    loss = c(
      1,
      1
    ),
    decisions = c(
      100,
      100
    )
  )

  expect_error(
    leave_one_out_er(
      list(),
      "pair_id",
      "engine",
      "loss",
      "decisions"
    ),
    "`data` must be a data frame.",
    fixed = TRUE
  )

  expect_error(
    leave_one_out_er(
      input,
      "missing",
      "engine",
      "loss",
      "decisions"
    ),
    "Missing column(s) in `data`: missing.",
    fixed = TRUE
  )

  expect_error(
    leave_one_out_er(
      transform(
        input,
        pair_id = NA_character_
      ),
      "pair_id",
      "engine",
      "loss",
      "decisions"
    ),
    "`pair_id` must not contain missing values.",
    fixed = TRUE
  )

  expect_error(
    leave_one_out_er(
      input[
        input$pair_id == "pair_01",
        ,
        drop = FALSE
      ],
      "pair_id",
      "engine",
      "loss",
      "decisions"
    ),
    "At least two unique omission units are required.",
    fixed = TRUE
  )

  expect_error(
    leave_one_out_er(
      input,
      "pair_id",
      "engine",
      "loss",
      "decisions",
      left_engine = "Sage",
      right_engine = "Sage"
    ),
    "`left_engine` and `right_engine` must be different.",
    fixed = TRUE
  )
})
