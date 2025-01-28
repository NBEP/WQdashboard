# Check qaqc_sites ------------------------------------------------------------
# df <- data.frame(Site_ID = c("001", "002"),
#                  Site_Name = c("Site1", "Site2"),
#                  Latitude = c(41.83, 42.28),
#                  Longitude = c(-71.41, -71.77),
#                  Town = c("Providence", "Worcester"),
#                  County = c("Providence", "Worcester"),
#                  State = c("Rhode Island", "MA"),
#                  Watershed = c(1, 2),
#                  Red_Herring = c("Foo", "Bar"))
# df_qaqc <- qaqc_sites(df)
# df_format <- format_sites(df_qaqc)

test_that("qaqc_sites sends error message if missing columns", {
  df <- data.frame(
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("RI", "MA"),
    Watershed = c(1, 2)
  )

  expect_error(
    qaqc_sites(df),
    regexp = "The following columns are missing: Site_ID, Site_Name"
  )
})

test_that("qaqc_sites sends error message if duplicate Site_ID", {
  df <- data.frame(
    Site_ID = c("foo", "foo"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("Rhode Island", "MA"),
    Watershed = c(1, 2)
  )

  expect_error(
    qaqc_sites(df),
    regexp = "Duplicate Site_ID in rows 1, 2"
  )
})

test_that("qaqc_sites sends error message if non-numeric Latitude, Longitude", {
  df_lat <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c("foo", "42.28"),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("Rhode Island", "MA"),
    Watershed = c(1, 2)
  )

  df_lon <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c("-71.41", "bar"),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("Rhode Island", "MA"),
    Watershed = c(1, 2)
  )

  expect_error(
    qaqc_sites(df_lat),
    regexp = "Non-numeric entries for Latitude found in rows: 1"
  )
  expect_error(
    qaqc_sites(df_lon),
    regexp = "Non-numeric entries for Longitude found in rows: 2"
  )
})

test_that("qaqc_sites converts state names to abbreviations", {
  df <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("Rhode Island", "MA"),
    Watershed = c(1, 2)
  )
  df2 <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("RI", "MA"),
    Watershed = c(1, 2)
  )

  expect_equal(qaqc_sites(df), df2)

  # Check - state errors
  df_fail <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("Road Island", "MA"),
    Watershed = c(1, 2)
  )
  expect_error(
    qaqc_sites(df_fail),
    "Invalid entry for State in rows 1"
  )
})

test_that("Ocean samples marked as saltwater", {
  df <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("RI", "MA"),
    Watershed = c(1, 2),
    Location_Type = c("Ocean", "Ocean")
  )
  df2 <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("RI", "MA"),
    Watershed = c(1, 2),
    Location_Type = c("Ocean", "Ocean"),
    Group = c("Saltwater", "Saltwater")
  )

  expect_equal(qaqc_sites(df), df2)
})

# # Check format_sites -----------------------------------------------------------
test_that("format_sites drops extra columns", {
  df <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("RI", "MA"),
    Watershed = c(1, 2),
    Red_Herring = c("Foo", "Bar")
  )
  df_format <- format_sites(df)

  expect_false("Red_Herring" %in% colnames(df_format))
})

test_that("format_sites adds Town_Code", {
  # Test - 2 towns, 2 states
  # Expected behavior: add Town_Code, drop Town
  df <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    State = c("RI", "MA"),
    Watershed = c(1, 2)
  )
  df2 <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    State = c("RI", "MA"),
    Watershed = c(1, 2),
    Town_Code = c("Providence, RI", "Worcester, MA")
  )
  expect_equal(format_sites(df), df2)

  # Test - 2 towns, 1 state
  # Expected behavior: add Town_Code, drop State, Town
  df_state <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Westerly"),
    State = c("RI", "RI"),
    Watershed = c(1, 2)
  )
  df_state2 <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Watershed = c(1, 2),
    Town_Code = c("Providence", "Westerly")
  )
  expect_equal(format_sites(df_state), df_state2)

  # Check - one town, two states
  # Expected behavior: add Town_Code, drop Town
  df_town <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Providence"),
    State = c("RI", "MA"),
    Watershed = c(1, 2)
  )
  df_town2 <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    State = c("RI", "MA"),
    Watershed = c(1, 2),
    Town_Code = c("Providence, RI", "Providence, MA")
  )
  expect_equal(format_sites(df_town), df_town2)

  # Check - one town, one state
  # Expected behavior: drop town and state columns
  df_loc <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Providence"),
    State = c("RI", "RI"),
    Watershed = c(1, 2)
  )
  df_loc2 <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Watershed = c(1, 2)
  )
  expect_equal(format_sites(df_loc), df_loc2)
})

test_that("format_sites adds County_Code", {
  # Test - 2 counties, 2 states
  # Expected behavior: add County_Code, drop County
  df <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    County = c("Providence", "Worcester"),
    State = c("RI", "MA"),
    Watershed = c(1, 2)
  )
  df2 <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    State = c("RI", "MA"),
    Watershed = c(1, 2),
    County_Code = c("Providence County, RI", "Worcester County, MA")
  )
  expect_equal(format_sites(df), df2)

  # Test - 2 counties, 1 state
  # Expected behavior: add County_Code, drop State, County
  df_state <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    County = c("Providence", "Kent"),
    State = c("RI", "RI"),
    Watershed = c(1, 2)
  )
  df_state2 <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Watershed = c(1, 2),
    County_Code = c("Providence County", "Kent County")
  )
  expect_equal(format_sites(df_state), df_state2)

  # Check - one county, two states
  # Expected behavior: add County_Code, drop County
  df_town <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    County = c("Providence", "Providence"),
    State = c("RI", "MA"),
    Watershed = c(1, 2)
  )
  df_town2 <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    State = c("RI", "MA"),
    Watershed = c(1, 2),
    County_Code = c("Providence County, RI", "Providence County, MA")
  )
  expect_equal(format_sites(df_town), df_town2)

  # Check - one county, one state
  # Expected behavior: drop county and state columns
  df_loc <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    County = c("Providence", "Providence"),
    State = c("RI", "RI"),
    Watershed = c(1, 2)
  )
  df_loc2 <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Watershed = c(1, 2)
  )
  expect_equal(format_sites(df_loc), df_loc2)
})
