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
  "Monitoring Location County Name" = c("Providence", "Worcester"),
  "State Code" = c("RI", "MA"),
  "Monitoring Location Type" = c("River/Stream", "River/Stream"),
  check.names = FALSE)
df_par <- data.frame(
  Site_ID = c("001", "002"),
  Date = c("240325", "240401"),
  Activity_Type = c("Quality Control Sample-Field Blank", "Field Msr/Obs"),
  Qualifier = c(NA, "Q"))
df_depth <- data.frame(
  Site_ID = c("001", "002", "003"),
  Parameter = c("Temperature, Air", "Temperature, Water", "Depth"),
  Depth = c(1, 150, 10),
  Depth_Unit = c("m", "cm", "foo"))

# Run tests --------------------------------------------------------------------
test_that("detect_column_format detects format", {
  df_error <- dplyr::select(df_wqx, !"State Code")

  expect_equal(detect_column_format(df_wqdashboard, colnames_sites),
               "WQdashboard_short")
  expect_equal(detect_column_format(df_wqx, colnames_sites), "WQX")
  expect_error(detect_column_format(df_error, colnames_sites))
  expect_error(detect_column_format(df_wqdashboard, colnames_results))
  expect_error(detect_column_format(df_wqdashboard, "hello world"))
})

test_that("update_column_names renames columns", {
  chk <- update_column_names(df_wqx, colnames_sites)
  expect_equal(
    colnames(chk),
    c("Site_ID", "Site_Name", "Latitude", "Longitude", "County", "State",
      "Location_Type"))
})

test_that("check_column_missing produces error if any columns missing", {
  df <- data.frame(Col1 = c("A", "B", "C"),
                   Col2 = c(1, 2, 3))

  expect_no_error(check_column_missing(df, c("Col1", "Col2")))
  expect_error(check_column_missing(df, c("Col1", "Col3")))
})

test_that("skip_dq_rows ignores rows where Qualifier listed in qaqc_fail", {
  chk <- df_par$Site_ID == "001"
  expect_false(all(chk))
  chk <- skip_dq_rows(df_par, chk)
  expect_true(all(chk))
})

test_that("skip_qc_rows ignores rows where Activity_Type is Quality Control", {
  chk <- df_par$Site_ID == "002"
  expect_false(all(chk))
  chk <- skip_qc_rows(df_par, chk)
  expect_true(all(chk))
})

test_that("check_val_missing produces error or warning if NA value in column", {
  df <- data.frame(
    Col1 = c("A", "B", "C"),
    Col2 = c("D", NA, "F"),
    Col3 = c(as.Date("2024/01/01"), as.Date("2024/01/02"),
             as.Date("2024/01/03")))

  expect_error(check_val_missing(df, "Col2"))
  expect_warning(check_val_missing(df, "Col2", is_stop = FALSE))
  expect_no_error(check_val_missing(df, "Col1"))
  expect_no_error(check_val_missing(df, "Col3"))
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
  chk <- format_date_col(df_par,
    date_format = "ymd",
    ignore_dq = FALSE)

  expect_equal(chk$Date[1], as.Date("2024-03-25"))
  expect_error(format_date_col(df_par, date_format = "foobar"))
  expect_error(format_date_col(df_par, date_format = "mdy"))
})

test_that("rename_param renames parameters", {
  expect_equal(rename_param("foobar"), "foobar")
  expect_equal(rename_param("Air Temperature"), "Temperature, air")
})

test_that("rename_unit renames units", {
  expect_equal(rename_unit("foobar"), "foobar")
  expect_equal(rename_unit("blank"), "None")
})

test_that("convert_unit accurately converts units", {
  expect_equal(convert_unit(32, "deg F", "deg C"), 0)
  expect_equal(convert_unit(0, "deg C", "deg F"), 32)
  expect_error(convert_unit(0,"foo", "bar"))
})

test_that("check_units flags multiple units per parameter", {
  df <- data.frame(
    Parameter = c("Temperature, Air", "Temperature, Air", "Temperature, Water"),
    Result_Unit = c("C", "F", "C"),
    Qualifier = c(NA, "Q", NA)
  )
  expect_no_error(check_units(df))
  expect_error(check_units(df, ignore_dq = FALSE))
})

test_that("depth_to_m converts depth to meters", {
  chk <- depth_to_m(df_depth)
  expect_equal(chk$Depth, c(1,1.5,10))
  expect_equal(chk$Depth_Unit, c("m", "m", "foo"))
})

test_that("assign_depth_category assigns depth category", {
  df <- depth_to_m(df_depth)
  chk <- assign_depth_category(df, sites=df_wqdashboard)
  expect_equal(chk$Depth_Category, c("Surface", "Midwater", NA))
})

test_that("list_sites lists unique sites", {
  df_blank <- df_par
  df_blank$Site_ID[1] <- NA
  expect_equal(list_sites(df_wqdashboard), c("001", "002"))
  expect_equal(list_sites(df_blank), "002")
})
