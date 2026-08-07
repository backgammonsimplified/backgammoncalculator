test_that("compare_er creates default comparison columns", {
  input <- data.frame(
    component = c("checker", "checker", "cube", "cube"),
    engine = c("Sage", "GNU", "Sage", "GNU"),
    er = c(0.29, 0.18, 9.65, 0.02)
  )

  observed <- compare_er(
    data = input,
    group_by = "component",
    engine = "engine",
    er = "er"
  )

  expect_named(
    observed,
    c(
      "component",
      "sage_er",
      "gnu_er",
      "er_difference_sage_minus_gnu",
      "lower_er_engine"
    )
  )
  expect_equal(observed$sage_er, c(0.29, 9.65))
  expect_equal(observed$gnu_er, c(0.18, 0.02))
  expect_equal(
    observed$er_difference_sage_minus_gnu,
    c(0.11, 9.63)
  )
  expect_identical(observed$lower_er_engine, c("GNU", "GNU"))
})

test_that("compare_er preserves first-observed group order", {
  input <- data.frame(
    component = c("cube", "cube", "checker", "checker"),
    engine = c("GNU", "Sage", "Sage", "GNU"),
    er = c(0.02, 9.65, 0.29, 0.18)
  )

  observed <- compare_er(
    input,
    "component",
    "engine",
    "er"
  )

  expect_identical(observed$component, c("cube", "checker"))
})

test_that("compare_er reports ties", {
  input <- data.frame(
    group = c("A", "A"),
    engine = c("Sage", "GNU"),
    er = c(1.5, 1.5)
  )

  observed <- compare_er(input, "group", "engine", "er")

  expect_identical(observed$lower_er_engine, "Tie")
  expect_identical(observed$er_difference_sage_minus_gnu, 0)
})

test_that("compare_er returns NA when ER is missing", {
  input <- data.frame(
    group = c("A", "A"),
    engine = c("Sage", "GNU"),
    er = c(NA_real_, 1)
  )

  observed <- compare_er(input, "group", "engine", "er")

  expect_true(is.na(observed$er_difference_sage_minus_gnu))
  expect_true(is.na(observed$lower_er_engine))
})

test_that("compare_er retains groups with an absent engine", {
  input <- data.frame(
    group = c("A", "A", "B"),
    engine = c("Sage", "GNU", "Sage"),
    er = c(1, 2, 3)
  )

  observed <- compare_er(input, "group", "engine", "er")

  expect_equal(nrow(observed), 2)
  expect_true(is.na(observed$gnu_er[observed$group == "B"]))
  expect_true(
    is.na(
      observed$er_difference_sage_minus_gnu[
        observed$group == "B"
      ]
    )
  )
  expect_true(
    is.na(observed$lower_er_engine[observed$group == "B"])
  )
})

test_that("compare_er rejects duplicate engine rows within a group", {
  input <- data.frame(
    group = c("A", "A", "A"),
    engine = c("Sage", "Sage", "GNU"),
    er = c(1, 2, 3)
  )

  expect_error(
    compare_er(input, "group", "engine", "er"),
    "Duplicate `Sage` rows for group = A.",
    fixed = TRUE
  )
})

test_that("compare_er ignores unrelated engines", {
  input <- data.frame(
    group = c("A", "A", "A"),
    engine = c("Sage", "GNU", "Other"),
    er = c(1, 2, 0)
  )

  observed <- compare_er(input, "group", "engine", "er")

  expect_equal(nrow(observed), 1)
  expect_identical(observed$lower_er_engine, "Sage")
})

test_that("compare_er supports one ungrouped comparison", {
  input <- data.frame(
    engine = c("Sage", "GNU"),
    er = c(1, 2)
  )

  observed <- compare_er(
    input,
    character(),
    "engine",
    "er"
  )

  expect_equal(nrow(observed), 1)
  expect_equal(observed$sage_er, 1)
  expect_equal(observed$gnu_er, 2)
})

test_that("compare_er supports custom engine labels", {
  input <- data.frame(
    group = c("A", "A"),
    engine = c("Engine One", "Engine Two"),
    er = c(1, 2)
  )

  observed <- compare_er(
    data = input,
    group_by = "group",
    engine = "engine",
    er = "er",
    left_engine = "Engine One",
    right_engine = "Engine Two"
  )

  expect_named(
    observed,
    c(
      "group",
      "engine_one_er",
      "engine_two_er",
      "er_difference_engine_one_minus_engine_two",
      "lower_er_engine"
    )
  )
  expect_identical(observed$lower_er_engine, "Engine One")
})

test_that("compare_er validates inputs", {
  input <- data.frame(
    group = "A",
    engine = "Sage",
    er = 1
  )

  expect_error(
    compare_er(list(), "group", "engine", "er"),
    "`data` must be a data frame.",
    fixed = TRUE
  )

  expect_error(
    compare_er(input, "missing", "engine", "er"),
    "Missing column(s) in `data`: missing.",
    fixed = TRUE
  )

  expect_error(
    compare_er(
      input,
      "group",
      "engine",
      "er",
      left_engine = "Sage",
      right_engine = "Sage"
    ),
    "`left_engine` and `right_engine` must be different.",
    fixed = TRUE
  )
})
