# Define variables -------------------------------------------------------------
df <- data.frame(Site_ID = c("001", "002"),
                 Site_Name = c("NBEP", "Blackstone"),
                 Town_Code = c("Providence, RI", "Worcester, MA"),
                 State = c("RI", "MA"),
                 Watershed = c("Woonasquatucker River-Moshassuck River",
                               "Upper Blackstone River"))
df_notown <- dplyr::select(df, !Town_Code)
df_nostate <- dplyr::select(df, !State)
df_nowatershed <- dplyr::select(df, !Watershed)
df_watershed <- dplyr::select(df, Watershed)
df_noloc <- dplyr::select(df, Site_ID)

# Run tests --------------------------------------------------------------------
test_that("list_loc_choices generates tab list", {
  chk_town <- c("town", "watershed")
  names(chk_town) <- c("By Town", "By Watershed")
  chk_state <- chk_town
  names(chk_state) <- c("By State", "By Watershed")

  expect_equal(list_loc_choices(df), chk_town)
  expect_equal(list_loc_choices(df_notown), chk_state)
  expect_equal(list_loc_choices(df_nostate), chk_town)
  expect_equal(list_loc_choices(df_nowatershed), chk_town[1])
  expect_equal(list_loc_choices(df_watershed), chk_town[2])
  expect_equal(list_loc_choices(df_noloc), "blank")
})

test_that("set_loc_tab outputs correct tab name", {
  expect_equal(set_loc_tab(c("A", "B")), "toggle")
  expect_equal(set_loc_tab("A"), "notoggle")
  expect_equal(set_loc_tab("blank"), "blank")
})

test_that("list_loc_choices and set_loc_tab interact correctly", {
  chk <- set_loc_tab(list_loc_choices(df))
  chk_watershed <- set_loc_tab(list_loc_choices(df_watershed))
  chk_noloc <- set_loc_tab(list_loc_choices(df_noloc))

  expect_equal(chk, "toggle")
  expect_equal(chk_watershed, "notoggle")
  expect_equal(chk_noloc, "blank")
})

test_that("update_town_list filters town by state", {
  expect_equal(update_town_list(df=df, state_list = "RI"), "Providence, RI")
  expect_null(update_town_list(df=df_notown, state_list = "RI"))
})

test_that("create_site_list generates alphabetical site list", {
  chk <- c("002", "001")
  names(chk) <- c("Blackstone", "NBEP")

  expect_equal(create_site_list(df), chk)
  expect_error(create_site_list(df_noloc))
})

test_that("update_site_list filters sites by selected col, variables", {
  chk <- c("002")
  names(chk) <- c("Blackstone")
  expect_equal(
    update_site_list(
      df = df,
      filter_col = "State",
      filter_list = "MA"),
    chk)
  expect_equal(
    update_site_list(
      df = df,
      filter_col = "Town_Code",
      filter_list = "Worcester, MA"),
    chk)
  expect_equal(
    update_site_list(
      df = df,
      filter_col = "Watershed",
      filter_list = "Upper Blackstone River"),
    chk)
  expect_error(
    update_site_list(
      df = df,
      filter_col = "Red_Herring",
      filter_list = "MA"))
})

test_that("sort_months sorts months chronologically", {
  chk <- c("December", "January", "March", "January")
  expect_equal(sort_months(chk), c("January", "March", "December"))
})
