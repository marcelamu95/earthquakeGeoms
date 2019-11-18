#' eq_clean_data
#'
#' this function takes raw NOAA data frame and returns a clean data
#' frame with a date column created by uniting the year, month, day
#' and converting it to the Date class,
#' LATITUDE and LONGITUDE columns converted to numeric class
#'
#' @param input set Dataset depicting the raw NOAA data frame.
#'
#' @return This function returns a dataframe with the NOAA data frame cleaned.
#'
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' input = read_delim("data/signif.txt", delim = "\t")
#' dataset = eq_clean_data(input)
#' }

eq_clean_data <- function(input){

### Crea la variable data
input <- input %>% dplyr::mutate(date = as.Date(paste(input$YEAR, input$MONTH, input$DAY, sep =
                                        "-"), "%Y-%m-%d"))
## Vuelce la variable LATITUDE and LONGITUDE numericas si no lo son
if(class(input$LATITUDE)!="numeric") input$LATITUDE <- as.numeric(input$LATITUDE)
if(class(input$LONGITUDE)!="numeric") input$LONGITUDE <- as.numeric(input$LONGITUDE)
if(class(input$DEATHS)!="numeric") input$DEATHS <- as.numeric(input$DEATHS)
if(class(input$EQ_PRIMARY)!="numeric") input$EQ_PRIMARY <- as.numeric(input$EQ_PRIMARY)
### Devuelve las variables necesarias y limpias
#input <- input%>%select(date,LONGITUDE,LATITUDES,LOCATION_NAME)

return(input)
}

