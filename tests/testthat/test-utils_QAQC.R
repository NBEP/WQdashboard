# Define vars ------------------------------------------------------------------
df_wqdashboard <- data.frame(
  Site_ID = c("001", "002"),
  Site_Name = c("Site1", "Site2"),
  Latitude = c(41.83, 42.28),
  Longitude = c(-71.41, -71.77),
  Town = c("Providence", "Worcester"),
  State = c("Rhode Island", "MA"))
df_wqx <- data.frame(
  "Monitoring Location ID" = c("001", "002"),
  "Monitoring Location Name" = c("Site1", "Site2"),
  "Monitoring Location Latitude (DD.DDDD)" = c(41.83, 42.28),
  "Monitoring Location Longitude (-DDD.DDDD)" = c(-71.41, -71.77),
  "State Code" = c("RI", "MA"),
  check.names = FALSE)
df_par <- data.frame(
  Site_ID = c("001", "002"),
  Date = c("240325", "240401")
)

# Run tests --------------------------------------------------------------------
test_that("detect_column_format detects format", {
  df_error <- dplyr::select(df_wqx, !"State Code")

  expect_equal(detect_column_format(df_wqdashboard, colnames_sites), "WQdashboard_short")
  expect_equal(detect_column_format(df_wqx, colnames_sites), "WQX")
  expect_error(detect_column_format(df_error, colnames_sites))
  expect_error(detect_column_format(df_wqdashboard, colnames_results))
  expect_error(detect_column_format(df_wqdashboard, "hello world"))
})

test_that("update_column_format renames columns", {
  chk <- update_column_format(df_wqx, colnames_sites)
  expect_equal(
    colnames(chk),
    c("Site_ID", "Site_Name", "Latitude", "Longitude", "State"))
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

test_that("format_date_col formats dates", {
  chk <- format_date_col(df_par, "ymd")

  expect_equal(chk$Date[1], as.Date("2024-03-25"))
  expect_error(format_date_col(df_par, "foobar"))
  expect_error(format_date_col(df_par, "mdy"))
})

test_that("rename_param renames parameters", {
  expect_equal(rename_param("foobar"), "foobar")
  expect_equal(rename_param("Air Temperature"), "Temperature, air")
})
