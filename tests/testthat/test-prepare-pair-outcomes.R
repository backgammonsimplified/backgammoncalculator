test_that("prepare_pair_outcomes calculates accepted Stage 1 values", {
  data <- prepare_pair_outcomes(
    sage_match_wins = 7,
    gnu_match_wins = 13,
    sage_pair_sweeps = 1,
    tied_pairs = 5,
    gnu_pair_sweeps = 4
  )

  expect_equal(nrow(data), 5L)
  expect_equal(data$count, c(7, 13, 1, 5, 4))
  expect_equal(data$total, c(20, 20, 10, 10, 10))
  expect_equal(data$share, c(0.35, 0.65, 0.10, 0.50, 0.40))
  expect_equal(data$xmax, c(0.35, 1.00, 0.10, 0.60, 1.00))
  expect_equal(data$percentage_label, c("35%", "65%", "10%", "50%", "40%"))
})

test_that("prepare_pair_outcomes validates counts", {
  expect_error(
    prepare_pair_outcomes(
      sage_match_wins = -1,
      gnu_match_wins = 13,
      sage_pair_sweeps = 1,
      tied_pairs = 5,
      gnu_pair_sweeps = 4
    ),
    "non-negative whole numbers",
    fixed = TRUE
  )
})
