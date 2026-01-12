test_that("sort_months works", {
  # Test - chronological list?
  expect_equal(
    sort_months(c("April", "January", "March", "January")),
    c("January", "February", "March", "April")
  )

  # Test - early return
  expect_equal(sort_months(c("April", "April")), "April")
})


test_that("sort_depth works", {
  expect_equal(
    sort_depth(c("Midwater", "Bottom", "Surface", "Midwater")),
    c("Surface", "Midwater", "Bottom")
  )

  # Edge case - weird depth values
  expect_equal(
    sort_depth(c("foo", NA, "Bottom", "Surface", "bar")),
    c("Surface", "Bottom", "bar", "foo", NA)
  )
})
