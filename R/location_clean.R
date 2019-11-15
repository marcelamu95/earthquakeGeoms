#' eq_location_clean
#'
#' limpie la variable LOCATION_NAME y conviértala al primer formato en mayúsculas
#'
#' @param data  Dataset clean NOAA data frame.
#' @param column The column LOCATION_NAME
#'
#' @return This function returns the raw NOAA
#' data frame with the LOCATION_NAME columns cleaned
#'
#' @export
#'
#' @importFrom stringr str_to_title
#'
#' @examples
#'
#' \dontrun{
#'
#' dataset = read_delim("data/signif.txt", delim = "\t")
#' dataset = eq_location_clean(data= input, column="LOCATION_NAME")
#'
#' }


eq_location_clean <- function(data, column){
  data[, column] <- stringr::str_to_title(sub(".*(\\:)\\s+","",data[[column]]))
  return(data)
}

#DF <- eq_location_clean(data=input, column="LOCATION_NAME")
# data_clean <- eq_clean_data(input)
# data_clean <- eq_location_clean(data_clean,"LOCATION_NAME")
#



#sample_USA <- readr::read_delim(filename, delim = "\t") %>%
# eq_clean_data() %>%
# input %>%
#   eq_clean_data() %>%
#   eq_location_clean("LOCATION_NAME") %>%
#   filter(YEAR >= 2000) %>%
#   filter(COUNTRY %in% c("USA", "MEXICO")) %>%
#   ggplot(aes(x = date, color = DEATHS, size = EQ_PRIMARY)) +
#   geom_timeline(aes(y = COUNTRY)) +
#   # geom_timeline_label(aes(label=LOCATION_NAME),n_max=5) + theme_classic()+
#   scale_size_continuous(name = 'Richter scale value', guide = guide_legend(order = 1)) +
#   scale_color_continuous(name = '# of Deaths', guide = guide_colorbar(order = 2)) +
#   new_theme()
#




