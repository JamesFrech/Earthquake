# Function Tests

# Function 1: read_earthquake_data
testthat::expect_that(read_earthquake_data("NOAASignificantEarthquakes.tsv"), testthat::is_a("spec_tbl_df"))

# Function 2: replace_NA
testthat::expect_that(replace_NA(c(0, 1, NA, 3), 2), testthat::equals(c(0, 1, 2, 3)))

# Function 3: eq_clean_data
testthat::expect_that(length(eq_clean_data(EarthquakeData)), testthat::equals(36))

# Function 4: eq_location_clean
testdata <- EarthquakeData %>%
  eq_clean_data() %>%
  eq_location_clean(7)

testthat::expect_that(colnames(testdata[,37]), testthat::is_identical_to("Country"))

# Function 5: geom_timeline
testplot1 <- ggplot(testdata) +
  geom_timeline(aes(x = Date, y = 1))

testthat::expect_that(testplot1, testthat::is_a("gg"))

# Function 6: geom_timelinelabel
testplot2 <- ggplot(testdata) +
  geom_timeline(aes(x = Date, y = 1)) +
  geom_timelinelabel(data = testdata[1:5,], aes(x = Date, y = 1, label = Country))

testthat::expect_that(testplot2, testthat::is_a("gg"))

# Function 7: n_max
testthat::expect_that(nrow(n_max(testdata[3000:3150,], 11, 7)), testthat::equals(7))

# Function 8: eq_map
testmap1 <- testdata %>%
   filter(Country == "Mexico" & year(Date) >= 2000) %>%
   eq_map(annot_col = .$Date)

testthat::expect_that(testmap1, testthat::is_a("leaflet"))

# Function 9: eq_create_label
testmap2 <- testdata %>%
  filter(Country == "Mexico" & year(Date) >= 2000) %>%
  mutate(popup = eq_create_label(.)) %>%
  eq_map(annot_col = .$popup)

testthat::expect_that(testmap2, testthat::is_a("leaflet"))
