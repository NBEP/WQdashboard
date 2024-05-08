df <- data.frame(
  Site_Name = "NBEP",
  Town = "Providence, RI",
  County = "Providence County, RI",
  State = "RI",
  Watershed = "Woonasquatucket River",
  Depth = "Shallow",
  Unit = "mg/L",
  score_typ = "Average",
  score_num = 42,
  score_str = "Excellent"
)

test_that("add_popup_text works", {
  chk <- add_popup_text(df)

  expect_true("popup_loc" %in% colnames(chk))
  expect_true("popup_score" %in% colnames(chk))
  expect_true("alt" %in% colnames(chk))
  expect_equal(chk$popup_loc[1],
    "<b>NBEP</b> <br/>Town: Providence, RI <br/>Watershed: Woonasquatucket River")
  expect_equal(chk$popup_score[1],
    " <br/>Depth: Shallow<br/>Average: 42 mg/L <br/>Score: Excellent")
  expect_equal(chk$alt[1], "NBEP, Excellent")

  df_county <- dplyr::select(df, !Town)
  chk_county <- add_popup_text(df_county)
  df_state <- dplyr::select(df, !c(Town, County))
  chk_state <- add_popup_text(df_state)
  df_nowater <- dplyr::select(df, !Watershed)
  chk_nowater <- add_popup_text(df_nowater)
  df_nodepth <- dplyr::select(df, !Depth)
  chk_nodepth <- add_popup_text(df_nodepth)

  expect_equal(chk_county$popup_loc[1],
    "<b>NBEP</b> <br/>County: Providence County, RI <br/>Watershed: Woonasquatucket River")
  expect_equal(chk_state$popup_loc[1],
    "<b>NBEP</b> <br/>State: RI <br/>Watershed: Woonasquatucket River")
  expect_equal(chk_nowater$popup_loc[1],
    "<b>NBEP</b> <br/>Town: Providence, RI")
  expect_equal(chk_nodepth$popup_score[1],
    "<br/>Average: 42 mg/L <br/>Score: Excellent")

  df_nodata <- dplyr::mutate(df, score_num = NA)
  chk_nodata <- add_popup_text(df_nodata)
  df_noscore <- dplyr::mutate(df, score_str = "No Threshold Established")
  chk_noscore <- add_popup_text(df_noscore)

  expect_equal(chk_nodata$popup_score, " <br/>Depth: Shallow <br/><i>No data</i>")
  expect_equal(chk_nodata$alt, "NBEP, No data")
  expect_equal(chk_noscore$popup_score, " <br/>Depth: Shallow<br/>Average: 42 mg/L")
  expect_equal(chk_noscore$alt, "NBEP, 42 mg/L")
})
