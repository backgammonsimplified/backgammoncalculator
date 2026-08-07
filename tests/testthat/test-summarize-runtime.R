test_that("summarize_runtime calculates descriptive statistics", {
  input <- data.frame(
    runtime_type = rep("live", 4),
    seconds = c(10, 20, 30, 40)
  )

  observed <- summarize_runtime(
    data = input,
    runtime = "seconds",
    group_by = "runtime_type"
  )

  expected <- data.frame(
    runtime_type = "live",
    n = 4L,
    total_seconds = 100,
    mean_seconds = 25,
    median_seconds = 25,
    q1_seconds = 17.5,
    q3_seconds = 32.5,
    iqr_seconds = 15,
    minimum_seconds = 10,
    maximum_seconds = 40
  )

  expect_equal(observed, expected)
})

test_that("summarize_runtime supports multiple groups", {
  input <- data.frame(
    category = rep("A", 4),
    runtime_type = c("live", "live", "review", "review"),
    seconds = c(10, 20, 30, 50)
  )

  observed <- summarize_runtime(
    data = input,
    runtime = "seconds",
    group_by = c("category", "runtime_type")
  )

  expect_equal(observed$n, c(2L, 2L))
  expect_equal(observed$total_seconds, c(30, 80))
  expect_equal(observed$median_seconds, c(15, 40))
})

test_that("summarize_runtime supports an ungrouped summary", {
  input <- data.frame(seconds = c(5, 15))

  observed <- summarize_runtime(
    data = input,
    runtime = "seconds"
  )

  expect_equal(observed$n, 2L)
  expect_equal(observed$total_seconds, 20)
  expect_equal(observed$mean_seconds, 10)
})

test_that("summarize_runtime supports one observation", {
  input <- data.frame(
    runtime_type = "live",
    seconds = 12
  )

  observed <- summarize_runtime(
    data = input,
    runtime = "seconds",
    group_by = "runtime_type"
  )

  expect_equal(observed$q1_seconds, 12)
  expect_equal(observed$q3_seconds, 12)
  expect_equal(observed$iqr_seconds, 0)
})

test_that("summarize_runtime preserves first-observed group order", {
  input <- data.frame(
    runtime_type = c("review", "live", "review", "live"),
    seconds = c(30, 10, 50, 20)
  )

  observed <- summarize_runtime(
    data = input,
    runtime = "seconds",
    group_by = "runtime_type"
  )

  expect_identical(observed$runtime_type, c("review", "live"))
})

test_that("summarize_runtime groups missing grouping values", {
  input <- data.frame(
    runtime_type = c("live", NA_character_, NA_character_),
    seconds = c(10, 20, 30)
  )

  observed <- summarize_runtime(
    data = input,
    runtime = "seconds",
    group_by = "runtime_type"
  )

  expect_equal(nrow(observed), 2)
  expect_equal(
    observed$total_seconds[is.na(observed$runtime_type)],
    50
  )
})

test_that("summarize_runtime rejects missing runtime values", {
  input <- data.frame(seconds = c(10, NA_real_))

  expect_error(
    summarize_runtime(input, "seconds"),
    "`seconds` must not contain missing values.",
    fixed = TRUE
  )
})

test_that("summarize_runtime rejects invalid runtime values", {
  expect_error(
    summarize_runtime(
      data.frame(seconds = -1),
      "seconds"
    ),
    "`seconds` must not contain negative values.",
    fixed = TRUE
  )

  expect_error(
    summarize_runtime(
      data.frame(seconds = Inf),
      "seconds"
    ),
    "`seconds` must contain only finite values.",
    fixed = TRUE
  )

  expect_error(
    summarize_runtime(
      data.frame(seconds = "10"),
      "seconds"
    ),
    "`seconds` must be numeric.",
    fixed = TRUE
  )
})

test_that("summarize_runtime validates data and columns", {
  input <- data.frame(
    group = "A",
    seconds = 10
  )

  expect_error(
    summarize_runtime(list(), "seconds"),
    "`data` must be a data frame.",
    fixed = TRUE
  )

  expect_error(
    summarize_runtime(input[FALSE, ], "seconds"),
    "`data` must contain at least one row.",
    fixed = TRUE
  )

  expect_error(
    summarize_runtime(input, "missing"),
    "Missing column(s) in `data`: missing.",
    fixed = TRUE
  )

  expect_error(
    summarize_runtime(
      input,
      "seconds",
      c("group", "group")
    ),
    "`group_by` must not contain duplicate column names.",
    fixed = TRUE
  )

  expect_error(
    summarize_runtime(
      input,
      "seconds",
      "seconds"
    ),
    "`runtime` must not also be a grouping column.",
    fixed = TRUE
  )
})
