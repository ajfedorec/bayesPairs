#' Plot pairs posteriors
#'
#' @param posteriors a data.frame with parameters as columns and samples as rows
#' @param priors a data.frame sampled from the prior distribution used to produce the posteriors
#' @param limits a data.frame with paramaters as columns and two rows, one with upper bounds and one with lower bounds (the order does not matter)
#'
#' @return a gtable with rug plots on the top and lefthand side, parameter names on the diagonal and 2D density plots in other positions
#' @export
#'
#' @examples
bayes_pairs <- function(posteriors, priors=c(), limits=c()){
  if(length(priors)>0){
    return(bayes_pairs_priors(posteriors, priors))
  }
  if(length(limits)>0){
    return(bayes_pairs_limits(posteriors, limits))
  }
}

theme_ajf <- function(base_size = 8, base_family = "",
                      base_line_size = base_size / 22,
                      base_rect_size = base_size / 22) {
  half_line <- base_size / 2

  ggplot2::theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line =               ggplot2::element_line(
      colour = "black", size = base_line_size,
      linetype = 1, lineend = "butt"
    ),
    rect =               ggplot2::element_rect(
      fill = "white", colour = "black",
      size = base_rect_size, linetype = 1
    ),
    text =               ggplot2::element_text(
      family = base_family, face = "plain",
      colour = "black", size = base_size,
      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = ggplot2::margin(), debug = FALSE
    ),

    axis.line =          ggplot2::element_line(colour = "black", size = ggplot2::rel(1)),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          ggplot2::element_text(size = ggplot2::rel(0.8), colour = "black"),
    axis.text.x =        ggplot2::element_text(margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =    ggplot2::element_text(margin = ggplot2::margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        ggplot2::element_text(margin = ggplot2::margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =  ggplot2::element_text(margin = ggplot2::margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks =         ggplot2::element_line(colour = "grey20"),
    axis.ticks.length =  ggplot2::unit(half_line / 2, "pt"),
    axis.title.x =       ggplot2::element_text(
      margin = ggplot2::margin(t = half_line),
      vjust = 1
    ),
    axis.title.x.top =   ggplot2::element_text(
      margin = ggplot2::margin(b = half_line),
      vjust = 0
    ),
    axis.title.y =       ggplot2::element_text(
      angle = 90,
      margin = ggplot2::margin(r = half_line),
      vjust = 1
    ),
    axis.title.y.right = ggplot2::element_text(
      angle = -90,
      margin = ggplot2::margin(l = half_line),
      vjust = 0
    ),

    legend.background =  ggplot2::element_rect(colour = NA),
    legend.spacing =     ggplot2::unit(0.4, "cm"),
    legend.spacing.x =    NULL,
    legend.spacing.y =    NULL,
    legend.margin =      ggplot2::margin(0.2, 0.2, 0.2, 0.2, "cm"),
    legend.key =         ggplot2::element_blank(),
    legend.key.size =    ggplot2::unit(1, "lines"),
    legend.key.height =  ggplot2::unit(0.6, "lines"),
    legend.key.width =   ggplot2::unit(0.6, "lines"),
    legend.text =        ggplot2::element_text(size = ggplot2::rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    legend.box.margin =  ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(0.4, "cm"),

    panel.background =   ggplot2::element_rect(fill = "white", colour = NA),
    panel.border     =   ggplot2::element_rect(fill = NA, colour = "grey20", size = ggplot2::rel(1)),
    panel.grid =         ggplot2::element_blank(),
    panel.grid.major =   ggplot2::element_blank(),
    panel.grid.minor =   ggplot2::element_blank(),
    panel.spacing =      ggplot2::unit(half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   ggplot2::element_rect(fill = "white", colour = NA),
    strip.text =         ggplot2::element_text(
      colour = "black",
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(half_line, half_line, half_line, half_line)
    ),
    strip.text.x =       NULL,
    strip.text.y =       ggplot2::element_text(angle = -90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = ggplot2::unit(0.1, "cm"),
    strip.switch.pad.wrap = ggplot2::unit(0.1, "cm"),

    plot.background =    ggplot2::element_rect(colour = "white"),
    plot.title =         ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0, vjust = 1,
      margin = ggplot2::margin(b = half_line * 1.2)
    ),
    plot.subtitle =      ggplot2::element_text(
      size = ggplot2::rel(0.9),
      hjust = 0, vjust = 1,
      margin = ggplot2::margin(b = half_line * 0.9)
    ),
    plot.caption =       ggplot2::element_text(
      size = ggplot2::rel(0.9),
      hjust = 1, vjust = 1,
      margin = ggplot2::margin(t = half_line * 0.9)
    ),
    plot.margin =        ggplot2::margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )
}
