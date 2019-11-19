#' eq_map
#'
#' Takes an argument data containing the filtered data frame
#' with earthquakes to visualize.
#' The function maps the epicenters (LATITUDE/LONGITUDE)
#' and annotates each point with in pop up window containing
#' annotation data stored in a column of the data frame.
#'
#' @param data_map A cleaned data frame with data  from NOAA website
#' @param annot_col The name of the column from the data to be use for annotation
#'
#' @return A map of the earthquakes epicenters and providing some annotations
#'
#' @examples
#' \dontrun{
#' readr::read_delim("signif.txt", delim = "\t") %>%
#' eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
#'   eq_map(annot_col = "date")
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @export

eq_map <- function(data_map,annot_col="date"){
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = data_map$LONGITUDE, lat = data_map$LATITUDE,
                              radius = data_map$EQ_PRIMARY, popup = data_map[,annot_col],
                              stroke = FALSE, fillOpacity = 0.5)

}

#' eq_create_label
#'
#' Created description of the earthquake next to the graph
#'
#' @param map_data A cleaned data frame with data obtained from NOAA website
#'
#' @return An HTML label that can be used as the annotation text in the leaflet map.
#'
#' @examples
#' \dontrun{
#' readr::read_delim("signif.txt", delim = "\t") %>%
#'   eq_clean_data() %>%
#'   eq_location_clean() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' }
#'
#' @export

eq_create_label<- function(map_data){

  paste(ifelse(is.na(map_data$LOCATION_NAME),"", paste("<b>Location: </b>",map_data$LOCATION_NAME,"<br/>")),
        ifelse(is.na(map_data$EQ_PRIMARY),"", paste("<b>Magnitude: </b>",map_data$EQ_PRIMARY,"<br/>")),
        ifelse(is.na(map_data$TOTAL_DEATHS),"", paste("<b>Total deaths: </b>",map_data$TOTAL_DEATHS,"<br/>")))

}


