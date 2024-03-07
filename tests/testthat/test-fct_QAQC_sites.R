df <- data.frame(Site_ID = c("001", "002"),
                 Site_Name = c("Site1", "Site2"),
                 Latitude = c(41.83, 42.28),
                 Longitude = c(-71.41, -71.77),
                 Town = c("Providence", "Worcester"),
                 State = c("Rhode Island", "MA"),
                 extraCol = c("Red", "Herring"))
df_test <- QAQC_sites(df)

test_that("Error message if missing columns", {
  df_fail <- dplyr::select(df, c("Site_ID", "Site_Name"))

  expect_no_error(df_test)
  expect_error(QAQC_sites(df_fail))
})

test_that("Extra columns are dropped", {
  expect_false("extraCol" %in% colnames(df_test))
})

test_that("State names are converted to abbreviations", {
  df_fail <- dplyr::mutate(df, State = "Road Island")

  expect_error(QAQC_sites(df_fail))
  expect_equal(df_test$State, c("RI", "MA"))
  expect_equal(df_test$Town_Code, c("Providence, RI", "Worcester, MA"))
})

test_that("State column dropped if only one state provided", {
  df_fail <- dplyr::filter(df, State == "Rhode Island")
  df_fail <- QAQC_sites(df_fail)

  expect_true("State" %in% colnames(df))
  expect_false("State" %in% colnames(df_fail))
})

test_that("Town_Code is added if more than one Town provided", {
  df_onestate <- dplyr::mutate(df, State = "Rhode Island")
  df_onestate <- QAQC_sites(df_onestate)
  df_nostate <- dplyr::select(df, !"State")
  df_nostate <- QAQC_sites(df_nostate)
  df_onetown <- dplyr::filter(df, Town == "Providence")
  df_onetown <- QAQC_sites(df_onetown)
  df_notown <- dplyr::select(df, !"Town")
  df_notown <- QAQC_sites(df_notown)

  expect_true("Town_Code" %in% colnames(df_test))
  expect_equal(df_test$Town_Code, c("Providence, RI", "Worcester, MA"))
  expect_true("Town_Code" %in% colnames(df_onestate))
  expect_equal(df_onestate$Town_Code, c("Providence", "Worcester"))
  expect_true("Town_Code" %in% colnames(df_nostate))
  expect_equal(df_nostate$Town_Code, c("Providence", "Worcester"))
  expect_false("Town_Code" %in% colnames(df_onetown))
  expect_false("Town_Code" %in% colnames(df_notown))
})
