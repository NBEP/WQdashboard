df <- data.frame(
  State = c("MA", "MA"),
  Depth_Category = c("Surface", "Surface"),
  Parameter = c("Nitrate", "Orthophosphate"),
  Unit = c("mg/L", "mg/L"),
  Min_Max_Mean = c("mean", "mean"),
  Threshold_Min = c(NA, NA),
  Threshold_Max = c(0.6, 0.05),
  Excellent = c(0.3, 0.025),
  Good = c(0.6, 0.05),
  Fair = c(0.9, 0.1),
  extra_col = c("Red", "Herring"))

df_test <- QAQC_thresholds(df)
df_t2 <- dplyr::select(df, !c(Excellent, Good, Fair))

test_that("Error message if missing columns", {
  df_fail <- dplyr::select(df, !Unit)

  expect_no_error(df_test)
  expect_error(QAQC_thresholds(df_fail))

  # Need at least one threshold column...
  df_t1 <- dplyr::select(df_t2, !Threshold_Max)
  df_t0 <- dplyr::select(df_t1, !Threshold_Min)
  expect_warning(QAQC_thresholds(df_t2))
  expect_error(suppressWarnings(QAQC_thresholds(df_t1)))
  expect_error(QAQC_thresholds(df_t0))
})

test_that("Replaces missing columns", {
  chk <- suppressWarnings(QAQC_thresholds(df_t2))
  expect_true(all(c("Excellent", "Good", "Fair") %in% colnames(chk)))
})

test_that("Drops extra columns", {
  expect_false("extra_col" %in% colnames(df_test))
  expect_false("Group" %in% colnames(df_test))
})

test_that("Can't list site and group on same row", {
  df_fail <- df %>%
    dplyr::mutate(Group = "foo") %>%
    dplyr::mutate(Site_ID = "bar")

  expect_error(QAQC_thresholds(df_fail))
})

test_that("Provides error message if invalid depth category", {
  df_fail <- dplyr::mutate(df, Depth_Category = "foo")
  expect_error(QAQC_thresholds(df_fail))
})

test_that("Provides error message if threshold is not numeric", {
  df_fail <- dplyr::mutate(df, Excellent = "foo")
  expect_error(QAQC_thresholds(df_fail))
})

