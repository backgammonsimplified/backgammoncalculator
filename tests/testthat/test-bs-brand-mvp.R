test_that("bundled BS metadata matches the generated snapshot", {
  info <- bs_brand_info()

  expect_identical(
    info$brand_kit_version,
    "1.0.0"
  )
  expect_identical(
    info$source_registry_checksum_sha256,
    "45a1938655558fc85cd22defd84292e65675a1549d97a5f1a171a3ed020b8956"
  )
  expect_identical(
    info$source_registry_checksum_scope,
    "original pre-rename registry"
  )
  expect_identical(
    info$source_commit,
    "UNCOMMITTED-DRAFT"
  )
})

test_that("Sage and GNU mappings remain fixed", {
  palette <- bs_engine_palette(
    c("Sage", "GNU")
  )

  expect_identical(
    unname(palette[["Sage"]]),
    "#915634"
  )
  expect_identical(
    unname(palette[["GNU"]]),
    "#3D5F87"
  )
})

test_that("engine matching is case-insensitive and preserves labels", {
  palette <- bs_engine_palette(
    c("sage", "GNU")
  )

  expect_identical(
    names(palette),
    c("sage", "GNU")
  )
  expect_identical(
    unname(palette),
    c("#915634", "#3D5F87")
  )
})

test_that("unknown engines fail explicitly", {
  expect_error(
    bs_engine_palette(
      c("Sage", "Unknown engine")
    ),
    "Unknown BS engine colour assignment"
  )
})

test_that("minimal BS theme and scales are ggplot objects", {
  expect_s3_class(
    theme_bs(),
    "theme"
  )

  expect_s3_class(
    scale_colour_bs_engine(),
    "ScaleDiscrete"
  )

  expect_s3_class(
    scale_fill_bs_engine(),
    "ScaleDiscrete"
  )
})

test_that("pair outcomes use bundled BS colours by default", {
  plot <- plot_pair_outcomes(
    sage_match_wins = 7,
    gnu_match_wins = 13,
    sage_pair_sweeps = 1,
    tied_pairs = 5,
    gnu_pair_sweeps = 4,
    sage_logo = grid::circleGrob(),
    gnu_logo = grid::circleGrob()
  )

  built <- ggplot2::ggplot_build(plot)
  rectangle_fills <- unique(built$data[[1L]]$fill)

  expect_true("#915634" %in% rectangle_fills)
  expect_true("#3D5F87" %in% rectangle_fills)
  expect_true("#ECEEF0" %in% rectangle_fills)
})
