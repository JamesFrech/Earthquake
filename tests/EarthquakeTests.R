# Function Tests
library(dplyr)

# Function 1: read_earthquake_data
read_earthquake_data <- function(file){
  data <- readr::read_tsv(file)
  return(data)
}

EarthquakeData <- read_earthquake_data("NOAASignificantEarthquakes.tsv")

testthat::expect_that(read_earthquake_data("NOAASignificantEarthquakes.tsv"), testthat::is_a("spec_tbl_df"))

# Function 2: replace_NA
replace_NA <- function(df, value){
  for(i in seq_along(df)){
    if(is.na(df[i])){
      df[i] <- value
    }
  }
  return(df)
}

testthat::expect_that(replace_NA(c(0, 1, NA, 3), 2), testthat::equals(c(0, 1, 2, 3)))

# Function 3: eq_clean_data
eq_clean_data <- function(df){
  df <- df[2:6243,] %>%
    # replace NA's in date and time with 01 for day and month and 0 for hour, minutes, and seconds
    dplyr::mutate(Mo = replace_NA(.$Mo, 01), Dy = replace_NA(.$Dy, 01), Hr = replace_NA(.$Hr, 0),
                  Mn = replace_NA(.$Mn, 0), Sec = replace_NA(.$Sec, 0)) %>%
    # add in 3000 years since the ymd function can only work with dates over the year 1000
    dplyr::mutate(Year = .$Year+3152) %>%
    dplyr::select(!"Search Parameters") %>%
    # create date column from the separate year, month, and date columns
    tidyr::unite(Date, Year, Mo, Dy, sep ="-") %>%
    dplyr::mutate(Date = lubridate::ymd(.$Date)) %>%
    # subtract the amount of days necessary to get the years back to the right time.
    dplyr::mutate(Date = .$Date-1151244) %>%
    dplyr::mutate(Latitude = as.numeric(.$Latitude), Longitude = as.numeric(.$Longitude))
  return(df)
}

testthat::expect_that(length(eq_clean_data(EarthquakeData)), testthat::equals(36))

# Function 4: eq_location_clean
eq_location_clean <- function(df, j){
  column <- as.data.frame(df[,j])
  country <- rep(0, nrow(df))
  for(i in 1:nrow(column)){
    if(grepl(":", column[i,])){
      country[i] <- stringr::str_extract(column[i,], "^[^:]+")
      column[i,] <- stringr::str_extract(column[i,], "[^:]+$")
      column[i,] <- stringr::str_trim(column[i,], side = "left")
    }
    else{
      country[i] <- column[i,]
    }
    column[i,] <- stringr::str_to_title(column[i,])
  }
  for(i in 1:nrow(column)){
    if((any(state.name %in% column[i,]) | any(toupper(state.name) %in% country[i])) &
       ((df$Longitude[i] < 40) | is.na(df$Longitude[i]))){
      country[i] <- "USA"
    }
  }
  df[,j] <- column
  df <- dplyr::mutate(df, Country = stringr::str_to_title(country))
  return(df)
}

testdata <- EarthquakeData %>%
  eq_clean_data() %>%
  eq_location_clean(7)

testthat::expect_that(colnames(testdata[,37]), testthat::is_identical_to("Country"))

# Function 5: geom_timeline
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x", "y"),
                                 default_aes = ggplot2::aes(color = "black", size = 1,
                                                            alpha = 1, shape = 16),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_scales, coord){
                                   coords <- ggplot2::coord_munch(coord, data, panel_scales)
                                   coords <- na.omit(coords)
                                   grid::pointsGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     size = grid::unit(coords$size*.5, "char"),
                                     pch = coords$shape,
                                     gp = grid::gpar(col = coords$col, alpha = coords$alpha)
                                   )
                                 })

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping, data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
  )
}

testplot1 <- ggplot2::ggplot(testdata) +
  geom_timeline(ggplot2::aes(x = Date, y = 1))

testthat::expect_that(testplot1, testthat::is_a("gg"))

# Function 6: geom_timelinelabel
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                      required_aes = c("x", "y", "label"),
                                      default_aes = ggplot2::aes(color = "grey", alpha = .75),
                                      draw_key = ggplot2::draw_key_vline,
                                      draw_panel = function(data, panel_scales, coord){
                                        coords <- ggplot2::coord_munch(coord, data, panel_scales)
                                        coords <- na.omit(coords)
                                        Line <- grid::segmentsGrob(
                                          x0 = coords$x,
                                          x1 = coords$x,
                                          y0 = coords$y,
                                          y1 = coords$y + .05,
                                          gp = grid::gpar(col = coords$col, alpha = coords$alpha)
                                        )
                                        Text <- grid::textGrob(
                                          x = coords$x,
                                          y = coords$y + .1,
                                          label = coords$label,
                                          rot = 40
                                        )
                                        grid::gTree(children = grid::gList(Line, Text))
                                      }
)

geom_timelinelabel <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                               na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping, data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
  )
}

testplot2 <- ggplot2::ggplot(testdata) +
  geom_timeline(ggplot2::aes(x = Date, y = 1)) +
  geom_timelinelabel(data = testdata[1:5,], ggplot2::aes(x = Date, y = 1, label = Country))

testthat::expect_that(testplot2, testthat::is_a("gg"))

# Function 7: n_max
n_max <- function(df, column, n, desc = TRUE){
  if(desc == TRUE){
    df <- df %>%
      dplyr::arrange(desc(df[,column])) %>%
      head(n)
  }
  else{
    df <- df %>%
      dplyr::arrange(df[,column]) %>%
      head(n)
  }
  return(df)
}

testthat::expect_that(nrow(n_max(testdata[3000:3150,], 11, 7)), testthat::equals(7))

# Function 8: eq_map
eq_map <- function(df, annot_col, color = "blue", radius = 5, weight = 1){
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = df, lat = ~ Latitude, lng = ~ Longitude, radius = radius,
                              popup = paste0(annot_col), color = color, weight = weight)
}

testmap1 <- testdata %>%
   dplyr::filter(Country == "Mexico" & lubridate::year(Date) >= 2000) %>%
   eq_map(annot_col = .$Date)

testthat::expect_that(testmap1, testthat::is_a("leaflet"))

# Function 9: eq_create_label
eq_create_label <- function(df){
  label <- paste("<b>Location:</b>", df$`Location Name`, "<br />",
                 "<b>Magnitude:</b>", df$Mag, "<br />",
                 "<b>Total Deaths:</b>", df$`Total Deaths`, "<br />")
  return(label)
}

testmap2 <- testdata %>%
  dplyr::filter(Country == "Mexico" & lubridate::year(Date) >= 2000) %>%
  dplyr::mutate(popup = eq_create_label(.)) %>%
  eq_map(annot_col = .$popup)

testthat::expect_that(testmap2, testthat::is_a("leaflet"))
