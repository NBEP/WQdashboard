test_that("caption_graph works", {
  df <- data.frame(
    Site_Name = c("foo", "bar", "foofy"),
    Parameter = c("Nitrate", "Orthophosphate", "Temperature, Water"),
    Unit = c("mg/L", "mg/L", "deg C"),
    Depth = c("Surface", "Midwater", "Bottom")
  )

  df_thresh <- data.frame(
    State = "MA",
    Group = NA,
    Parameter = "Nitrate",
    Unit = "mg/L",
    Min_Max_Mean = "mean",
    Threshold_Min = NA,
    Threshold_Max = 0.6,
    Excellent = 0.3,
    Good = 0.6,
    Fair = 0.9)
  df_thresh_null <- df_thresh %>%
    dplyr::mutate(Threshold_Max = NA) %>%
    dplyr::mutate(Good = NA)

  # Run tests - no thresholds
  expect_equal(
    caption_graph(df, "Site_Name"),
    "Nitrate, Orthophosphate, and Temperature, Water for foo, bar, and foofy.")
  expect_equal(
    caption_graph(df, "Depth"),
    "Nitrate, Orthophosphate, and Temperature, Water for foo, bar, and foofy at surface, midwater, and bottom depths.")
  expect_equal(
    caption_graph(head(df, 2), "Depth"),
    "Nitrate and Orthophosphate for foo and bar at surface and midwater depths.")
  expect_equal(
    caption_graph(head(df, 1), "Depth"),
    "Nitrate for foo at surface depth.")

  # Run tests - thresholds
  expect_equal(
    caption_graph(head(df, 1), "Site_Name", df_thresh),
    "Nitrate for foo. The maximum acceptable value is 0.6 mg/L and the maximum excellent value is 0.3 mg/L."
  )
  expect_equal(
    caption_graph(head(df, 1), "Site_Name", df_thresh_null),
    "Nitrate for foo."
  )

})
