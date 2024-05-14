df <- data.frame(Site_ID = c("001", "002"),
                 Site_Name = c("Site1", "Site2"),
                 Latitude = c(41.83, 42.28),
                 Longitude = c(-71.41, -71.77),
                 Town = c("Providence", "Worcester"),
                 State = c("Rhode Island", "MA"))
df_test <- QAQC_sites(df)

test_that("Error message if missing columns", {
  df_fail <- dplyr::select(df, c("Site_ID", "Site_Name"))

  expect_no_error(df_test)
  expect_error(QAQC_sites(df_fail))
})

test_that("Error message if duplicate Site_ID", {
  df_fail <- dplyr::mutate(df, Site_ID = "foo")

  expect_error(QAQC_sites(df_fail))
})

test_that("Error message if non-numeric Latitude, Longitude", {
  df_fail <- dplyr::mutate(df, Latitude = "foo")
  df_fail2 <- dplyr::mutate(df, Longitude = "foo")

  expect_error(QAQC_sites(df_fail))
  expect_error(QAQC_sites(df_fail2))
})

test_that("State names are converted to abbreviations", {
  df_fail <- dplyr::mutate(df, State = "Road Island")

  expect_error(QAQC_sites(df_fail))
  expect_equal(df_test$State, c("RI", "MA"))
})

test_that("Ocean samples marked as saltwater", {
  df_ocean <- dplyr::mutate(df, Location_Type = "Ocean")
  chk <- QAQC_sites(df_ocean)

  expect_equal(chk$Group[1], "Saltwater")
})
