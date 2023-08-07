#' theme_crgt
#'
#' This is a custom \href{https://ggplot2.tidyverse.org/}{ggplot2} theme for the
#' crgt package which controls all non-data display elements. Use
#' \code{\link[ggplot2]{theme}} instead if you just need to tweak the display of
#' an existing theme.
#'
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 margin
#'
#' @param base_size base font size, given in pts.
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#' @param ... additional arguments to be passed to \code{\link[ggplot2]{theme}}
#'
#' @references
#' Wickham, H. (2016). ggplot2: elegant graphics for data analysis. New York, N.Y.: Springer-Verlag.
#'
#' @seealso \code{\link[ggplot2]{theme}}
#'
#' @export
theme_crgt <- function(base_size = 16, base_family = "sans", base_line_size = base_size/22,
                       base_rect_size = base_size/22, ...) {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family,
                    base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    ggplot2::theme(
      # plotting components
      panel.border = ggplot2::element_rect(fill = NA, colour = "grey20", size = 1),
      panel.grid = ggplot2::element_line(colour = "grey85"),
      panel.grid.minor = ggplot2::element_line(size = rel(0.5)),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      # panel.grid.minor = ggplot2::element_blank(),
      # panel.grid.major =  ggplot2::element_line(color = "black"),
      # fill the plot and panel spaces with grey and remove border
      panel.background = ggplot2::element_rect(fill = "white", color = "black"),

      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      # panel.border = element_blank(),
      # remove strip background
      # adjust the margins of plots and remove axis ticks
      plot.margin = ggplot2::margin(0.5, 1, 0.5, 1, unit = "cm"),
      legend.title = ggplot2::element_text(face = "bold", size = rel(1.33)),
      legend.text = ggplot2::element_text(face = "bold", size = rel(1.2)),
      # axis.ticks = element_blank(),
      # change text family, size, and adjust position of titles
      text = ggplot2::element_text(family = text_family, size = base_size),
      axis.text = ggplot2::element_text(face = "bold", color = "black", size = rel(1.33)),
      axis.ticks.length = ggplot2::unit(base_size/3, "pt"),
      axis.ticks = ggplot2::element_line(size = base_size/15),
      axis.title = ggplot2::element_text(face = "bold", size = rel(1.5)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(0.5, 0, 0, 0, unit = "cm")),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 0.5, 0, 0, unit = "cm"), angle=90),
      plot.title = ggplot2::element_text(face = "bold", size = rel(1.67), hjust = 0),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(face = "bold", size = rel(1.3), margin = ggplot2::margin(1, 0, 0.5, 0, unit = "cm"), hjust = 0.1),
      plot.caption = ggplot2::element_text(size = 10, margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
      strip.text = ggplot2::element_text(size = rel(1.3), face = "bold", margin = ggplot2::margin(0.25, 0.25, 0.25, 0.25, unit = "cm")),
      strip.background = ggplot2::element_rect(fill = "grey90", colour = "black", size = 2),
      complete = TRUE,
      ...
    )
}
