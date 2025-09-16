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
    Fair = 0.9
  )
  df_thresh_null <- df_thresh %>%
    dplyr::mutate(Threshold_Max = NA) %>%
    dplyr::mutate(Good = NA)

  # Run tests
  expect_equal(
    caption_graph(df[1, ], df_thresh),
    "<hr><h3>Thresholds</h3><b>Acceptable:</b> &lt; 0.6 mg/L<br><b>Excellent:</b> &lt; 0.3 mg/L"
  )
  expect_equal(caption_graph(df, NULL), "")
  expect_equal(caption_graph(df, df_thresh_null), "")
  expect_equal(caption_graph(df[3, ], df_thresh), "")
})
