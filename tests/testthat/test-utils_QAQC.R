test_that("check_column_name renames columns", {
  df <- data.frame(Col1 = c("A", "B", "C"), Col2 = c(1, 2, 3))
  df_new <- data.frame(Col1 = c("A", "B", "C"), Col3 = c(1, 2, 3))

  expect_error(check_column_name(df, c("Col1", "Col2"), "Col3"))
  expect_equal(check_column_name(df, "Col2", "Col3"), df_new)
})

test_that("check_column_missing produces error if any columns missing", {
  df <- data.frame(Col1 = c("A", "B", "C"),
                   Col2 = c(1, 2, 3))

  expect_no_error(check_column_missing(df, c("Col1", "Col2")))
  expect_error(check_column_missing(df, c("Col1", "Col3")))
})

test_that("check_val_missing produces error or warning if NA or blank cell", {
  df <- data.frame(Col1 = c("A", "B", "C"),
                   Col2 = c("D", "E", ""),
                   Col3 = c("G", NA, "I"))

  expect_error(check_val_missing(df, "Col2"))
  expect_error(check_val_missing(df, "Col3"))
  expect_warning(check_val_missing(df, "Col2", is_stop = FALSE))
  expect_warning(check_val_missing(df, "Col3", is_stop = FALSE))
  expect_no_error(check_val_missing(df, "Col1"))
})

test_that("check_val_duplicate produces error or warning if duplicate values in
          column", {
  df <- data.frame(Col1 = c("A", "B", "C"),
                   Col2 = c("D", "E", "E"),
                   Col3 = c("G", "G", "I"))

  expect_error(check_val_duplicate(df, "Col2"))
  expect_error(check_val_duplicate(df, c("Col2, Col3")))
  expect_warning(check_val_duplicate(df, "Col2", is_stop = FALSE))
  expect_no_error(check_val_duplicate(df, "Col1"))
  expect_no_error(check_val_duplicate(df, c("Col1", "Col2")))
})

test_that("Drops column if less than two unique values", {
  df <- data.frame(Col1 = c("A", "A", "A"),
                   Col2 = c(NA, NA, NA),
                   Col3 = c("A", "B", "C"))
  chk1 <- check_val_count(df, "Col1")
  chk2 <- check_val_count(df, "Col2")
  chk3 <- check_val_count(df, "Col3")
  chk4 <- check_val_count(df, "Col4")
  expect_equal(colnames(chk1), c("Col2", "Col3"))
  expect_equal(colnames(chk2), c("Col1", "Col3"))
  expect_equal(colnames(chk3), c("Col1", "Col2", "Col3"))
  expect_equal(colnames(chk4), c("Col1", "Col2", "Col3"))
})

test_that("check_val_numeric produces error if non-numeric value in column", {
  df <- data.frame(Col1 = c("A", "B", "C"),
                   Col2 = c(1, 2, 3))

  expect_no_error(check_val_numeric(df, "Col2"))
  expect_error(check_val_numeric(df, "Col1"))
})
