test_that("sort_months sorts months chronologically", {
  chk <- c("December", "January", "March", "January")
  expect_equal(sort_months(chk), c("January", "March", "December"))
})
