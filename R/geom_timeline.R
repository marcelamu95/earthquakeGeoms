#' GeomTimeline
#'
#' for plotting a time line of earthquakes ranging from xmin
#' to xmaxdates with a point for each earthquake.
#' Optional aesthetics include color, size, and alpha
#' (for transparency).
#'
#' @details The data to be used with this geom must be downloaded and readed from NOAA site,
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}.
#'
#' @section Parameters to Geom:
#'
#' \code{data}: A Dataframe Object containing the data to plot. \cr \cr
#' \code{x}: Column of `data` depicting the x-coordinate of where each of the points will be located. \cr \cr
#' \code{size}: OPTIONAL. Column of `data` depicting size of each point in the pointsGrob displayed (if
#' missing, the first \code{nmax} labels will be displayed). \cr \cr
#' \code{y}: OPTIONAL. Column of `data` depicting the y-coordinate of where each of the points will be located. \cr \cr
#' \code{colour}: OPTIONAL. Column of `data` depicting the colour of each point in the pointGrob displayed. \cr \cr
#' \code{shape}: OPTIONAL. Column of 'data' depicting the shape of each of the points. \cr \cr
#' \code{alpha}: OPTIONAL. Column of `data` which will be used to apply the alpha to each point. \cr \cr
#' \code{fill}: OPTIONAL. Color 'string' (added outside of aes) to modify the default fill of the points (before applying \code{colour}). \cr \cr
#' \code{stroke}: OPTIONAL. Stroke of the image (need not be used). \cr \cr
#'
#' @importFrom scales alpha
#' @importFrom grid unit
#' @importFrom grid gList
#' @importFrom grid linesGrob
#' @importFrom grid pointsGrob
#' @importFrom grid gpar
#' @importFrom ggplot2 .pt
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom ggplot2 Geom
#'
#' @return
#'
#' @examples
#'
#' \dontrun{
#'
#' filename <- system.file("extdata", "signif.txt", package="earthquakeGeoms")
#' library(readr)
#' input <- readr::read_delim(filename, delim = "\t")
#'
#'  sample <- input %>%
#'  eq_clean_data() %>%
#'  eq_location_clean("LOCATION_NAME")%>%
#'  filter(YEAR >= 2000) %>%
#'  filter(COUNTRY %in% c("USA", "MEXICO"))
#'
#' ggplot2::ggplot(data=sample, aes(x = date, y = COUNTRY, color = DEATHS, size = EQ_PRIMARY)) +
#'  geom_timeline() +
#'  scale_size_continuous(name = 'Richter scale value', guide = guide_legend(order = 1)) +
#'  scale_color_continuous(name = '# of Deaths', guide = guide_colorbar(order = 2))
#'
#' }
#'
#' @export


GeomTimeline  <- ggproto(
  "GeomTimeline",
  Geom,
  required_aes = c("x"),
  default_aes = aes(
    y = 0.5,
    size = 3,
    shape = 19,
    alpha = 0.5,
    stroke = 0,
    colour = "gray23"
  ),
  draw_key = draw_key_point,
  draw_panel = function(data, panel_scales, coord) {
    ## Transform the data first
    coords <-
      coord$transform(data, panel_scales)

    ## Construct a grid grob

    grid::pointsGrob(
      x = coords$x,
      y = coords$y,
      size = grid::unit(data$size, "mm"),
      pch = coords$shape,
      gp = grid::gpar(col = coords$colour,
                      alpha = coords$alpha)
    )
  }
)

#' geom_timeline
#'
#' This function displays the dates of Earthquakes
#' by presenting each Earthquake, on a straight line,
#' as a point of varying sizes depending on their Ritcher scale value (based on the \code{size} variable). The Earthquakes are color coded by the death toll it caused (\code{colour}). If the \code{y} variable is supplied, the Earthquakes will be displayed in multiple horizontal lines,
#' each line represnting the quakes for each Country.
#'
#' @param data A Dataframe Object containing the data to plot.
#' @param mapping Mapping argument to the ggplot layer function.
#' @param stat Stat argument to the ggplot layer function.
#' @param position Position argument to the ggplot layer function.
#' @param na.rm na.rm argument to the ggplot layer function.
#' @param show.legend show.legend argument to the ggplot layer function.
#' @param inherit.aes inherit.aes argument to the ggplot layer function.
#' @param  ... Extra Params.
#'
#' @importFrom ggplot2 layer
#'
#' @return This function adds the lineGrobs and pointGrob into the current graphics device.
#'
#' @examples
#'
#' \dontrun{
#'
#' filename <- system.file("extdata", "signif.txt", package="earthquakeGeoms")
#' library(readr)
#' input <- readr::read_delim(filename, delim = "\t")
#'
#'  sample <- input %>%
#'  eq_clean_data() %>%
#'  eq_location_clean("LOCATION_NAME")%>%
#'  filter(YEAR >= 2000) %>%
#'  filter(COUNTRY %in% c("USA", "MEXICO"))
#'
#' ggplot2::ggplot(data=sample, aes(x = date, y = COUNTRY, color = DEATHS, size = EQ_PRIMARY)) +
#'  geom_timeline() +
#'  scale_size_continuous(name = 'Richter scale value', guide = guide_legend(order = 1)) +
#'  scale_color_continuous(name = '# of Deaths', guide = guide_colorbar(order = 2))
#'
#' }
#'
#' @export

geom_timeline <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      geom = GeomTimeline,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }


#' new_theme
#'
#' Created a new theme tovisualizes ggplots
#'
#' @param legend.position the position of legends.
#' @param axis.line.y lines for each axis y.
#' @param axis.line.x lines for each axis x.
#' @param panel.grid.major.y Specify major grid lines.
#' @param  ... Extra Params.
#'
#' @importFrom ggplot2 layer
#'
#' @return This function adds the lineGrobs and pointGrob into the current graphics device.
#'
#' @examples
#'
#' \dontrun{
#'
#' filename <- system.file("extdata", "signif.txt", package="earthquakeGeoms")
#' library(readr)
#' input <- readr::read_delim(filename, delim = "\t")
#'
#'  sample <- input %>%
#'  eq_clean_data() %>%
#'  eq_location_clean("LOCATION_NAME")%>%
#'  filter(YEAR >= 2000) %>%
#'  filter(COUNTRY %in% c("USA", "MEXICO"))
#'
#' ggplot2::ggplot(data=sample, aes(x = date, y = COUNTRY, color = DEATHS, size = EQ_PRIMARY)) +
#'  geom_timeline() +
#'  scale_size_continuous(name = 'Richter scale value', guide = guide_legend(order = 1)) +
#'  scale_color_continuous(name = '# of Deaths', guide = guide_colorbar(order = 2))+
#'  new_theme()
#' }
#'
#' @export


new_theme <- function(...) {
  ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = 'bottom',
      axis.line.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      #axis.ticks.y = ggplot2::element_blank(),
      #axis.line.x = ggplot2::element_line(size = 1),
      panel.grid.major.y = ggplot2::element_line(color = "gray")
    ) +
    ggplot2::theme(...)
}

