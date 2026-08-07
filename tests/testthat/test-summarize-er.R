test_that("summarize_er pools before calculating ER", {
  input <- data.frame(
    engine = c(
      "Sage",
      "Sage",
      "GNU",
      "GNU"
    ),
    loss = c(
      0.400,
      0.456,
      0.200,
      0.315
    ),
    decisions = c(
      700,
      771,
      700,
      757
    )
  )

  observed <- summarize_er(
    data = input,
    group_by = "engine",
    equity_lost = "loss",
    eligible_decisions = "decisions"
  )

  expected <- data.frame(
    engine = c("Sage", "GNU"),
    equity_lost = c(0.856, 0.515),
    eligible_decisions = c(1471, 1457),
    er = c(
      0.290958531611148874,
      0.176733013040494158
    )
  )

  expect_equal(
    observed,
    expected,
    tolerance = 1e-12
  )
})

test_that("summarize_er supports multiple grouping columns", {
  input <- data.frame(
    engine = c(
      "Sage",
      "Sage",
      "Sage",
      "Sage"
    ),
    component = c(
      "checker",
      "checker",
      "cube",
      "cube"
    ),
    loss = c(
      0.400,
      0.456,
      1.800,
      2.080
    ),
    decisions = c(
      700,
      771,
      100,
      101
    )
  )

  observed <- summarize_er(
    data = input,
    group_by = c(
      "engine",
      "component"
    ),
    equity_lost = "loss",
    eligible_decisions = "decisions"
  )

  expect_equal(
    observed$equity_lost,
    c(0.856, 3.880),
    tolerance = 1e-12
  )

  expect_equal(
    observed$eligible_decisions,
    c(1471, 201)
  )

  expect_equal(
    observed$er,
    c(
      0.290958531611148874,
      9.651741293532337451
    ),
    tolerance = 1e-12
  )
})

test_that("summarize_er calculates an ungrouped summary", {
  input <- data.frame(
    loss = c(0.2, 0.3),
    decisions = c(40, 60)
  )

  expect_equal(
    summarize_er(
      data = input,
      group_by = character(),
      equity_lost = "loss",
      eligible_decisions = "decisions"
    ),
    data.frame(
      equity_lost = 0.5,
      eligible_decisions = 100,
      er = 2.5
    )
  )
})

test_that("summarize_er returns NA ER for zero pooled decisions", {
  input <- data.frame(
    engine = c("Sage", "Sage"),
    loss = c(0, 0),
    decisions = c(0, 0)
  )

  observed <- summarize_er(
    data = input,
    group_by = "engine",
    equity_lost = "loss",
    eligible_decisions = "decisions"
  )

  expect_identical(
    observed$er,
    NA_real_
  )
})

test_that("summarize_er does not silently discard missing metrics", {
  input <- data.frame(
    engine = c(
      "Sage",
      "Sage",
      "GNU"
    ),
    loss = c(
      0.2,
      NA_real_,
      0.3
    ),
    decisions = c(
      50,
      50,
      NA_real_
    )
  )

  observed <- summarize_er(
    data = input,
    group_by = "engine",
    equity_lost = "loss",
    eligible_decisions = "decisions"
  )

  expect_true(
    is.na(observed$equity_lost[observed$engine == "Sage"])
  )

  expect_true(
    is.na(observed$eligible_decisions[observed$engine == "GNU"])
  )

  expect_true(
    all(is.na(observed$er))
  )
})

test_that("summarize_er preserves first-observed group order", {
  input <- data.frame(
    engine = c(
      "GNU",
      "Sage",
      "GNU",
      "Sage"
    ),
    loss = c(
      0.1,
      0.2,
      0.3,
      0.4
    ),
    decisions = c(
      10,
      20,
      30,
      40
    )
  )

  observed <- summarize_er(
    data = input,
    group_by = "engine",
    equity_lost = "loss",
    eligible_decisions = "decisions"
  )

  expect_identical(
    observed$engine,
    c("GNU", "Sage")
  )
})

test_that("summarize_er groups missing grouping values", {
  input <- data.frame(
    category = c(
      "known",
      NA_character_,
      NA_character_
    ),
    loss = c(
      0.2,
      0.3,
      0.2
    ),
    decisions = c(
      20,
      30,
      20
    )
  )

  observed <- summarize_er(
    data = input,
    group_by = "category",
    equity_lost = "loss",
    eligible_decisions = "decisions"
  )

  expect_equal(
    nrow(observed),
    2
  )

  expect_equal(
    observed$equity_lost[is.na(observed$category)],
    0.5
  )

  expect_equal(
    observed$eligible_decisions[is.na(observed$category)],
    50
  )
})

test_that("summarize_er validates data and column arguments", {
  input <- data.frame(
    group = "A",
    loss = 1,
    decisions = 100
  )

  expect_error(
    summarize_er(
      data = list(),
      group_by = "group",
      equity_lost = "loss",
      eligible_decisions = "decisions"
    ),
    "`data` must be a data frame.",
    fixed = TRUE
  )

  expect_error(
    summarize_er(
      data = input,
      group_by = "missing",
      equity_lost = "loss",
      eligible_decisions = "decisions"
    ),
    "Missing column(s) in `data`: missing.",
    fixed = TRUE
  )

  expect_error(
    summarize_er(
      data = input,
      group_by = c("group", "group"),
      equity_lost = "loss",
      eligible_decisions = "decisions"
    ),
    "`group_by` must not contain duplicate column names.",
    fixed = TRUE
  )

  expect_error(
    summarize_er(
      data = input,
      group_by = "loss",
      equity_lost = "loss",
      eligible_decisions = "decisions"
    ),
    "Metric columns must not also be grouping columns: loss.",
    fixed = TRUE
  )
})

test_that("summarize_er rejects invalid metric values", {
  expect_error(
    summarize_er(
      data = data.frame(
        group = "A",
        loss = -1,
        decisions = 100
      ),
      group_by = "group",
      equity_lost = "loss",
      eligible_decisions = "decisions"
    ),
    "`loss` must not contain negative values.",
    fixed = TRUE
  )

  expect_error(
    summarize_er(
      data = data.frame(
        group = "A",
        loss = 1,
        decisions = Inf
      ),
      group_by = "group",
      equity_lost = "loss",
      eligible_decisions = "decisions"
    ),
    "`decisions` must contain only finite values or NA.",
    fixed = TRUE
  )
})
