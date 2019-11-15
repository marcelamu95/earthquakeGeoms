#' geom_timeline_label
#'
#' adding annotations to the earthquake data.
#' This geom adds a vertical line to each data point with a text annotation (e.g.
#' the location of the earthquake) attached to each line.
#'
#' @param data A Dataframe Object containing the data to plot.
#' @param mapping Mapping argument to the ggplot layer function.
#' @param stat Stat argument to the ggplot layer function.
#' @param position Position argument to the ggplot layer function.
#' @param na.rm na.rm argument to the ggplot layer function.
#' @param show.legend show.legend argument to the ggplot layer function.
#' @param inherit.aes inherit.aes argument to the ggplot layer function.
#' @param n_max maximum number of tags
#' @param  ... Extra Params.
#'
#' @importFrom ggplot2 layer
#'
#' @export
#'
#' @example
#'
#'  \dontrun{
#' input <- readr::read_delim("signif.txt", delim = "\t")
#' data <- eq_clean_data(input)
#' data <- eq_location_clean(data)
#' data %>%
#' dplyr::filter(COUNTRY == c("MEXICO","USA") & lubridate::year(date) >= 2010) %>%
#' ggplot(aes(x=date,y=COUNTRY,color=TOTAL_DEATHS,size=EQ_PRIMARY)) +
#' geom_timeline(alpha=0.5) +
#' geom_timelinelabel(aes(label=LOCATION_NAME),n_max=3) +
#' theme(legend.position="bottom", legend.box="horizontal", plot.title=element_text(hjust=0.5)) +
#' ggtitle("Earthquakes Visualization Tool") +
#' labs(size = "Richter scale value", color = "# deaths")
#' }


geom_timeline_label <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           n_max = 5,
           ...) {
    #top n_max using NSE
    datamax <- data %>% dplyr::group_by_(~ COUNTRY) %>% dplyr::top_n(n_max,EQ_PRIMARY)

    ggplot2::layer(
      geom = GeomTimeLineLabel,
      mapping = mapping,
      data = datamax,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )

  }


#' GeomTimeLineLabel
#'
#' GeomTimeLineLabel Geom coding
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid segmentsGrob gpar textGrob gList
#' @importFrom dplyr slice arrange_ group_by_ %>%
#'
#' @export

GeomTimeLineLabel <- ggproto(
  "GeomTimeLineLabel",
  ggplot2::Geom,
  required_aes = c("x", "label"),
  default_aes = ggplot2::aes(
    y = 0.5, size=0.2, shape=19,
    colour = "gray23",
    linetype = 1,
    alpha = NA,
    angle = 45,
    hjust = 0,
    vjust = 0,
    family = "",
    fontface = 2,
    lineheight = 1.5
  ),
  draw_key = draw_key_label,
  draw_panel = function(data, panel_scales, coord) {

    ## Transform the data
    coords <-
      coord$transform(data, panel_scales)

    ## Vertical lines

    vline <- grid::segmentsGrob(
      x0 = coords$x,
      x1 = coords$x,
      y0 = coords$y,
      y1 = coords$y + 0.1,
      default.units = "native",
      gp = grid::gpar(
        size = 0.5,
        alpha = 1,
        color = "black"
        # alpha = coords$alpha
      )
    )

    line_annot <- grid::textGrob(
      coords$label,
      x = coords$x,
      y = coords$y + 0.1,
      default.units = "native",
      hjust = coords$hjust,
      vjust = coords$vjust,
      rot = coords$angle,
      gp = grid::gpar(
        col = "black",
        alpha = 1,  #coords$alpha
        fontsize = 3.5 * .pt,
        fontfamily = coords$family,
        fontface = coords$fontface,
        lineheight = coords$lineheight
      )
    )
    timeline_label <-
      grid::gTree(children = grid::gList(vline, line_annot))

  }
)
