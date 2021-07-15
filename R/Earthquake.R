#########################
# Reading/cleaning Data #
#########################

#' read_earthquake_data
#'
#' @description Reads in the data from your file with earthquake data. Just calls in read_tsv from readr package, but
#' specified to earthquake data. Could really be used with any tsv file.
#'
#' @note The data required for this package can be obtained at \url{https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/search.}
#' Click the search button with no filters and the resulting data will contain all the information in their database.
#' You can then download the tsv file from the download button in the top left corner of the resulting data table.
#' Citation: DOI:10.7289/V5TD9V7K
#'
#' @param file The file you would like to read in for use.
#'
#' @examples \dontrun{read_earthquake_data("NOAASignificantEarthquakes.tsv")}
#'
#' @return Returns the data from your tsv file
#'
#' @importFrom readr read_tsv
#'
#' @export
read_earthquake_data <- function(file){
  data <- readr::read_tsv(file)
  return(data)
}

#' EarthquakeData
#'
#' @format A data frame with 6,243 rows and 39 columns:
#' \describe{
#'   \item{Search parameter}{Search Parameter for database (not important)}
#'   \item{Year}{The year of the earthquake}
#'   \item{Mo}{The month of the earthquake}
#'   \item{Dy}{The day of the earthquake}
#'   \item{Hr}{The hour of the earthquake}
#'   \item{Mn}{The minute of the earthquake}
#'   \item{Sec}{The second of the earthquake}
#'   \item{Tsu}{Related tsunami}
#'   \item{Vol}{Related Volcano}
#'   \item{Location Name}{The location of the earthquake}
#'   \item{Latitude}{The latitude of the earthquake}
#'   \item{Longitude}{The longitude of the earthquake}
#'   \item{Focal Depth (km)}{The focal depth of the earthquake in kilometers}
#'   \item{Mag}{The magnitude of the earthquake}
#'   \item{MMI Int}{The modified Mercalli Intensity scale}
#'   \item{Deaths}{How many people died}
#'   \item{Death Description}{Scale from 0-4 of how many people died}
#'   \item{Missing}{How many people went missing}
#'   \item{Missing Description}{Scale from 0-4 of how many people went missing}
#'   \item{Injuries}{How many people were injured}
#'   \item{Injuries Description}{Scale from 0-4 of how many people were injured}
#'   \item{Damage ($Mil)}{Amount of damage in units of million dollars}
#'   \item{Damage Description}{Scale from 0-4 of how much damage was caused}
#'   \item{Houses Destroyed}{The amount of houses destroyed in the earthquake}
#'   \item{Houses Destroyed Description}{Scale from 0-4 of how many houses were destroyed}
#'   \item{Houses Damaged}{How many houses were damaged}
#'   \item{Houses Damaged Description}{Scale from 0-4 of how many houses were damaged}
#'   \item{Total Deaths}{Total amount of deaths in the earthquake}
#'   \item{Total Death Description}{Scale from 0-4 of how many total deaths there were}
#'   \item{Total Missing}{Total amount of people who went missing}
#'   \item{Total Missing Description}{Scale from 0-4 of how many people went missing}
#'   \item{Total Injuries}{Total amount of people injured}
#'   \item{Total Injuries Description}{Scale from 0-4 of how many people were injured in total}
#'   \item{Total Damage ($Mil)}{Total amount of damage in units of million dollars}
#'   \item{Total Damage Description}{Scale from 0-4 of how much total damage was caused}
#'   \item{Total Houses Destroyed}{Total amount of houses destroyed}
#'   \item{Total Houses Destroyed Description}{Scale from 0-4 of how many total houses were destroyed}
#'   \item{Total Houses Damaged}{Total amount of houses that were damaged}
#'   \item{Total Houses Damaged Description}{Scale from 0-4 of how many total houses were damaged}
#' }
#'
#' @source DOI:10.7289/V5TD9V7K \url{https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/search}
"EarthquakeData"



#' replace_NA
#'
#' @description Replaces the NA's in a column of a data frame with a specified value
#'
#' @param df The dataframe column to be modified using the df$column syntax
#' @param value The value used to replace the NA's in the given column
#'
#' @examples \dontrun{replace_NA(EarthquakeData$Mo, 01)}
#' @examples \dontrun{replace_NA(EarthquakeData$Hr, 00)}
#'
#' @return The original dataframe with the updated column is returned.
#'
#' @export
replace_NA <- function(df, value){
  for(i in seq_along(df)){
    if(is.na(df[i])){
      df[i] <- value
    }
  }
  return(df)
}

#' eq_clean_data
#'
#' @description This function cleans the data from the NOAA Significant Earthquake Database.
#'
#' @param df The data frame to be cleaned. Specified for data from the NOAA Significant Earthquake Database,
#' but can be used for any dataframe with the correct column names
#'
#' @examples \dontrun{eq_clean_data(EarthquakeData)}
#'
#' @note NA's in the Mo and Dy columns are replaced by 01. NA's in the Hr, Mn, and Sec columns are replaced with 0.
#'
#' @return Returns the given data frame in a cleaner version with the year, month, and date in a single column
#' as a Date object. Also converts Latitude and Longitude columns into the numeric class.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tidyr unite
#' @importFrom lubridate ymd
#'
#' @export
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


#' eq_location_clean
#'
#' @description This function cleans the Location Name column of a given data set,
#' removing the country name along with the colons and white spaces around the city/area name.
#' A separate column for countries is added.
#'
#' @param df The data frame to be cleaned.
#' @param j The column number or name for the "location name" column in your data frame.
#'
#' @examples \dontrun{eq_location_clean(EarthquakeData, 7)}
#'
#' @return Returns the data frame with the location name column updated to remove countries and adding a new column for countries.
#'
#' @importFrom stringr str_extract
#' @importFrom stringr str_trim
#' @importFrom stringr str_to_title
#' @importFrom dplyr mutate
#'
#' @export
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

###################
# Time line Geoms #
###################

#' GeomTimeline
#'
#' @description A Geom Object that will create a points Grob of a timeline.
#'
#' @note x and y aesthetics are required. If you do not desire to use any variable as the y value, just set it to a constant value.
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom ggplot2 coord_munch
#' @importFrom grid pointsGrob
#' @importFrom grid gpar
#' @importFrom grid unit
#'
#' @export
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

#' geom_timeline
#'
#' @description This function adds a time line to a ggplot object.
#'
#' @param mapping mapping aesthetics to be used for your time line. Includes x, y, color, size, alpha, and shape parameters.
#' @param data the data to be used to add your time line to the graph. Defaults to the ggplot data if not specified.
#' @param stat A stat to be used to transform the given data.
#' @param position The positioning of the time line on the graph.
#' @param na.rm Removes NA's if TRUE.
#' @param show.legend Can show a legend for the time line.
#' @param inherit.aes Asks if you would like to inherit parameters.
#' @param ... any additional arguments.
#'
#' @examples \dontrun{
#' ggplot(data = EarthquakeData[6175:6200,]) +
#' geom_timeline(aes(x = Date, y = factor(Mag, levels = unique(Mag)),
#'                   color = `Houses Destroyed`, size = `Total Deaths`, alpha = `Total Injuries`))
#' }
#' @examples \dontrun{ggplot(EarthquakeData) + geom_timeline(aes(x = Date, y = 5))}
#'
#' @return A pointsGrob object in the shape of a time line on your graph.
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping, data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
  )
}

#' GeomTimelineLabel
#'
#' @description A Geom Object that will create a points Grob of a timeline.
#'
#' @note x, y, and label aesthetics are required. If you do not desire to use any variable as the y value,
#' just set it to a constant value.
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_vline
#' @importFrom ggplot2 coord_munch
#' @importFrom grid segmentsGrob
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom grid gTree
#' @importFrom grid gList
#'
#' @export
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

#' geom_timelinelabel
#'
#' @description This function adds labels to your time line.
#'
#' @param mapping mapping aesthetics to be used for your time line. Includes x, y, label, color, and alpha parameters.
#' @param data the data to be used to add your labels. Defaults to the ggplot data if not specified.
#' @param stat A stat to be used to transform the given data.
#' @param position The positioning of the time line labels on the graph.
#' @param na.rm Removes NA's if TRUE.
#' @param show.legend Can show a legend for the labels
#' @param inherit.aes Asks if you would like to inherit parameters.
#' @param ... any additional arguments.
#'
#' @note In order to label only certain points by an attribute, use n_max() on the data given in the data parameter.
#'
#' @examples \dontrun{
#' ggplot(data = EarthquakeData[1000:1020,]) +
#' geom_timeline(aes(x = Date, y = 0, color = `Total Deaths`, alpha = Mag)) +
#' geom_timelinelabel(aes(x = Date, y = 1, label = `Location Name`)) +
#' ylim(c(-1, 2)) +
#' theme_minimal()
#' }
#' @examples \dontrun{
#' Earthquake3000 <- arrange(filter(EarthquakeData[3000:3150,], Country %in% c("Italy", "Indonesia", "China")), desc(Mag))
#'
#' ggplot(data = Earthquake3000) +
#'   geom_timeline(aes(x = Date, y = factor(Country, levels = unique(Country)), color = `Total Deaths`, size = Mag), alpha = 0.5) +
#'   geom_timelinelabel(data = n_max(Earthquake3000, 11, 5),
#'                      aes(x = Date, y = factor(Country, levels = unique(Country)), label = `Location Name`)) +
#'   theme_minimal()}
#'
#'
#'
#' @return A gTree object with segment and text Grob children added to your graph
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timelinelabel <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                               na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping, data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
  )
}

#' n_max
#'
#' @description This function filters data for desired labels.
#'
#' @param df The data frame you would like to filter
#' @param column The number of the column you want to arrange the order of your data by.
#' @param n The number of labels you want. The labels will be retrieved from the top of the data you rearranged.
#' @param desc TRUE if you want to arrange the data in descending order to label points with the highest numbers.
#' FALSE if you want to label the points with the lowest numbers.
#'
#' @examples \dontrun{n_max(EarthquakeData[3000:3150,], 11, 7)}
#' @examples \dontrun{ggplot(EarthquakeData[6100:6150,]) +
#' geom_timeline(aes(x = Date, y = factor(Country, levels = unique(Country)), color = Mag)) +
#'   geom_timelinelabel(data = n_max(EarthquakeData[6100:6150,], 11, 7),
#'                      aes(x = Date, y = factor(Country, levels = unique(Country)), label = `Location Name`)) +
#'   ylab("Country") +
#'   theme_minimal()}
#'
#' @return The rows of the data frame from which the labels will be retrieved from.
#'
#' @importFrom dplyr arrange
#'
#' @export
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

###############
# Map Widgets #
###############

#' eq_map
#'
#' @description This function creates a map widget of Earthquake Epicenters.
#'
#' @param df The data frame you would like to use. Must have Latitude and Longitude columns.
#' @param annot_col The column from which the annotations are taken from. Must use df$column notation.
#' @param color The color of the location dots.
#' @param radius The size of the location dots.
#' @param weight The size of the outlines of the dots.
#'
#' @examples \dontrun{
#' EarthquakeData %>%
#'   filter(Country == "Mexico" & year(Date) >= 2000) %>%
#'   eq_map(annot_col = .$Date)}
#'
#' @return A map widget with location points of each earthquake
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#'
#' @export
eq_map <- function(df, annot_col, color = "blue", radius = 5, weight = 1){
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = df, lat = ~ Latitude, lng = ~ Longitude, radius = radius,
                              popup = paste0(annot_col), color = color, weight = weight)
}

#' eq_create_label
#'
#' @description Creates a label in HTML format for each point observation of your data.
#'
#' @param df The data frame to retrieve data from
#'
#' @note Data must contain columns `Location Name`, Mag, and `Total Deaths` for this function to work.
#'
#' @examples \dontrun{
#' EarthquakeData[EarthquakeData$Country == "Usa", colnames(EarthquakeData)] %>%
#'   mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = .$popup_text)}
#' @examples \dontrun{
#' EarthquakeData %>%
#'   filter(Country == "Italy" & year(Date) >= 2000) %>%
#'   mutate(popup_label = eq_create_label(.)) %>%
#'   eq_map(annot_col = .$popup_label, radius = 7, weight = 1)}
#'
#' @return A vector of HTML labels to add to your data frame. Labels include Location, Magnitude, and Total Deaths.
#'
#' @export
eq_create_label <- function(df){
  label <- paste("<b>Location:</b>", df$`Location Name`, "<br />",
                 "<b>Magnitude:</b>", df$Mag, "<br />",
                 "<b>Total Deaths:</b>", df$`Total Deaths`, "<br />")
  return(label)
}
