test_that("calculate_er reproduces the six accepted Stage 1 ER values", {
  accepted <- data.frame(
    equity_lost = c(
      0.85599999999999998,
      0.51500000000000001,
      3.87999999999999989,
      0.01000000000000000,
      4.73500000000000032,
      0.52300000000000002
    ),
    eligible_decisions = c(
      1471,
      1457,
      201,
      242,
      1672,
      1699
    ),
    expected_er = c(
      0.290958531611148874,
      0.176733013040494158,
      9.651741293532337451,
      0.020661157024793389,
      1.415968899521531155,
      0.153914067098293111
    )
  )

  expect_equal(
    calculate_er(
      accepted$equity_lost,
      accepted$eligible_decisions
    ),
    accepted$expected_er,
    tolerance = 1e-12
  )
})

test_that("calculate_er is vectorized and recycles a scalar", {
  expect_equal(
    calculate_er(
      equity_lost = c(1, 2, 3),
      eligible_decisions = 100
    ),
    c(5, 10, 15)
  )

  expect_equal(
    calculate_er(
      equity_lost = 1,
      eligible_decisions = c(100, 200, 250)
    ),
    c(5, 2.5, 2)
  )
})

test_that("calculate_er returns NA for zero eligible decisions", {
  expect_identical(
    calculate_er(
      equity_lost = c(0, 1),
      eligible_decisions = c(0, 0)
    ),
    c(NA_real_, NA_real_)
  )
})

test_that("calculate_er propagates missing values", {
  expect_identical(
    calculate_er(
      equity_lost = c(NA_real_, 1, 1),
      eligible_decisions = c(100, NA_real_, 100)
    ),
    c(NA_real_, NA_real_, 5)
  )
})

test_that("calculate_er rejects negative inputs", {
  expect_error(
    calculate_er(-1, 100),
    "`equity_lost` must not contain negative values.",
    fixed = TRUE
  )

  expect_error(
    calculate_er(1, -100),
    "`eligible_decisions` must not contain negative values.",
    fixed = TRUE
  )
})

test_that("calculate_er rejects non-numeric and non-finite inputs", {
  expect_error(
    calculate_er("1", 100),
    "`equity_lost` must be numeric.",
    fixed = TRUE
  )

  expect_error(
    calculate_er(1, Inf),
    "`eligible_decisions` must contain only finite values or NA.",
    fixed = TRUE
  )

  expect_error(
    calculate_er(NaN, 100),
    "`equity_lost` must not contain NaN.",
    fixed = TRUE
  )
})

test_that("calculate_er rejects incompatible vector lengths", {
  expect_error(
    calculate_er(
      equity_lost = c(1, 2),
      eligible_decisions = c(100, 200, 300)
    ),
    paste0(
      "`equity_lost` and `eligible_decisions` must have equal lengths, ",
      "or one argument must have length one."
    ),
    fixed = TRUE
  )
})

test_that("calculate_er supports empty numeric inputs", {
  expect_identical(
    calculate_er(
      numeric(),
      numeric()
    ),
    numeric()
  )
})
