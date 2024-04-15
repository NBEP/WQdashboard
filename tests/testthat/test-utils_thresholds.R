df <- data.frame(
  Parameter = c("Nitrogen", "Nitrogen", "Nitrogen"),
  Depth_Category = c("Shallow", "Deep", NA)
)

test_that("filter_threshold_depth works", {
  chk <- filter_threshold_depth(df, "Shallow")
  expect_equal(chk$Depth_Category, "Shallow")

  chk <- filter_threshold_depth(df, "Foo")
  expect_equal(chk$Depth_Category, NA_character_)
})
