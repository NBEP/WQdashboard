test_that("caption_graph works", {
  df <- data.frame(
    Site_Name = c("foo", "bar", "foofy"),
    Parameter = c("Nitrate", "Orthophosphate", "Temperature, Water"),
    Unit = c("mg/L", "mg/L", "deg C"),
    Depth = c("Surface", "Midwater", "Bottom"),
    Min = NA,
    Max = 0.6,
    Excellent = 0.3,
    Best = "low"
  )

  # Run tests
  expect_equal(
    caption_graph(df[1, ]),
    "<hr><h3>Thresholds</h3><b>Acceptable:</b> &lt; 0.6 mg/L<br><b>Excellent:</b> &lt; 0.3 mg/L"
  )
})
