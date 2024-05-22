# Check qaqc_sites ------------------------------------------------------------
df <- data.frame(Site_ID = c("001", "002"),
                 Site_Name = c("Site1", "Site2"),
                 Latitude = c(41.83, 42.28),
                 Longitude = c(-71.41, -71.77),
                 Town = c("Providence", "Worcester"),
                 County = c("Providence", "Worcester"),
                 State = c("Rhode Island", "MA"),
                 Watershed = c(1, 2),
                 Red_Herring = c("Foo", "Bar"))
df_qaqc <- qaqc_sites(df)
df_format <- format_sites(df_qaqc)

test_that("Error message if missing columns", {
  df_fail <- dplyr::select(df, c("Site_ID", "Site_Name"))

  expect_no_error(df_qaqc)
  expect_error(qaqc_sites(df_fail))
})

test_that("Error message if duplicate Site_ID", {
  df_fail <- dplyr::mutate(df, Site_ID = "foo")

  expect_error(qaqc_sites(df_fail))
})

test_that("Error message if non-numeric Latitude, Longitude", {
  df_fail <- dplyr::mutate(df, Latitude = "foo")
  df_fail2 <- dplyr::mutate(df, Longitude = "foo")

  expect_error(qaqc_sites(df_fail))
  expect_error(qaqc_sites(df_fail2))
})

test_that("State names are converted to abbreviations", {
  df_fail <- dplyr::mutate(df, State = "Road Island")

  expect_error(qaqc_sites(df_fail))
  expect_equal(df_qaqc$State, c("RI", "MA"))
})

test_that("Ocean samples marked as saltwater", {
  df_ocean <- dplyr::mutate(df, Location_Type = "Ocean")
  chk <- qaqc_sites(df_ocean)

  expect_equal(chk$Group[1], "Saltwater")
})

# Check format_sites -----------------------------------------------------------
test_that("Drops extra columns", {
  expect_false("Red_Herring" %in% colnames(df_format))
})

test_that("Drops state/county/town/watershed if only one variable", {
  chk <- c("Town", "County", "State", "Watershed") %in% colnames(df_format)
  expect_true(all(chk))

  df_state <- df %>%
    dplyr::mutate(State = "RI") %>%
    qaqc_sites() %>%
    format_sites()
  df_town <- df %>%
    dplyr::mutate(Town = "Providence") %>%
    qaqc_sites() %>%
    format_sites()
  df_state_town <- df_town %>%
    dplyr::mutate(State = "RI") %>%
    qaqc_sites() %>%
    format_sites()

  expect_false("State" %in% colnames(df_state))
  expect_true("Town" %in% colnames(df_town))
  expect_false("Town" %in% colnames(df_state_town))
})

test_that("Adds Town_Code/County_Code column", {
  expect_true("Town_Code" %in% colnames(df_format))
  expect_false("County_Code" %in% colnames(df_format))

  df_no_town <- df %>%
    dplyr::select(!Town) %>%
    qaqc_sites() %>%
    format_sites()

  expect_false("Town_Code" %in% colnames(df_no_town))
  expect_true("County_Code" %in% colnames(df_no_town))
})
