test_that("BS typography tokens come from the generated registry", {
  typography <- bs_typography()
  registry <- getFromNamespace(
    "bs_brand",
    "backgammoncalculator"
  )

  expect_named(
    typography,
    c(
      "primary",
      "css_stack",
      "r_fallback",
      "python_fallback"
    )
  )

  expect_identical(
    unname(typography[["primary"]]),
    registry$typography$family$primary
  )

  expect_identical(
    unname(typography[["css_stack"]]),
    registry$typography$family$fallback_css
  )

  expect_identical(
    unname(typography[["r_fallback"]]),
    registry$typography$family$fallback_r
  )
})
