#' ggThemeScanpy: A package of theme modifications for ggplot that produce plots that are very similar to the default scanpy plotting options
#'
#' @section Functions:
#' All functions take a ggplot plot object and return a modified ggplot plot object.
#'
#' theme_scanpy_scatter(plot.object) produces plots that are similar to scanpy PCA or UMAP scatter plots
#'
#' @docType package
#' @name ggThemeScanpy
NULL

#' Return a ggplot2 theme object blanking unused theme elements
#'
#' @return ggplot2 theme
theme_scanpy <- function() {
  ggplot2::theme(axis.ticks.x.top = ggplot2::element_blank(),
                 axis.ticks.y.right = ggplot2::element_blank(),
                 axis.title.x.top = ggplot2::element_blank(),
                 axis.title.y.right = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank())
}


#' Apply scanpy theme to a ggplot2 plot object that is continuous on both axes
#'
#' @param plot.object ggplot2 Plot Object
#' @return ggplot2 Plot object
#' @export
theme_scanpy_continuous <- function(plot.object) {
  plot.object +
    theme_scanpy() +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(breaks = 0)) +
    ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(breaks = 0))
}

#' Apply scanpy scatter (e.g. PCA or UMAP) theme to a ggplot2 plot object
#'
#' @param plot.object ggplot2 Plot Object
#' @param color.level Applies hue to either "color" or "fill" properties. Set to NULL to skip applying hue.
#' @return ggplot2 Plot object
#' @export
theme_scanpy_scatter <- function(plot.object, color.level="color") {

  plot.object <- theme_scanpy_continuous(plot.object) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank())


  if (is.null(color.level)) {return(plot.object)}

  #https://stackoverflow.com/questions/19214914/how-can-i-make-the-legend-in-ggplot2-the-same-height-as-my-plot
  panel.height <- unit(1,"npc") - sum(ggplot2::ggplotGrob(plot.object)[["heights"]][-3]) - unit(1,"line")

  color.level <- tolower(color.level)
  new.guide <- ggplot2::guide_colorbar(barheight=panel.height,
                                       frame.colour = "black",
                                       frame.linewidth = 1,
                                       ticks.colour = "black",
                                       ticks.linewidth = 1,
                                       override.aes = list(alpha=1))

  if (color.level == "fill") {
    plot.object <- plot.object +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::guides(fill=new.guide)
  } else if ((color.level == "color") | (color.level == "colour")) {
    plot.object <- plot.object +
      ggplot2::scale_color_viridis_c() +
      ggplot2::guides(color=new.guide)
  }
  return(plot.object)
}

#' Apply scanpy theme to a ggplot2 plot object that is continuous on the y-axis and discrete on the x-axis
#'
#' @param plot.object ggplot2 Plot Object
#' @return ggplot2 Plot object
#' @export
theme_scanpy_x_categorical <- function(plot.object) {

  plot.object <- plot.object +
    theme_scanpy() +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(breaks = 0)) +
    ggplot2::annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black', x = -Inf, xend = Inf, size = 1)

  return(plot.object)
}

#' Apply scanpy theme to a ggplot2 plot object that is discrete on the y-axis and continuous on the x-axis
#'
#' @param plot.object ggplot2 Plot Object
#' @return ggplot2 Plot object
#' @export
theme_scanpy_y_categorical <- function(plot.object) {

  plot.object <- plot.object +
    theme_scanpy() +
    ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(breaks = 0)) +
    ggplot2::annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = Inf, xend = Inf, size = 1)

  return(plot.object)
}

#' Apply scanpy theme to a ggplot2 plot object that is discrete on both the x-axis and the y-axis
#'
#' @param plot.object ggplot2 Plot Object
#' @return ggplot2 Plot object
#' @export
theme_scanpy_x_y_categorical <- function(plot.object) {

  plot.object <- plot.object +
    theme_scanpy() +
    ggplot2::annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black', x = -Inf, xend = Inf, size = 1) +
    ggplot2::annotate(geom = 'segment', y = -Inf, yend = Inf, color = 'black', x = Inf, xend = Inf, size = 1)

  return(plot.object)
}
