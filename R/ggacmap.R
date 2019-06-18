#' @importFrom ggplot2 fortify
#' @importFrom Racmacs agCoords
#' @importFrom Racmacs srCoords
#' @importFrom Racmacs agFill
#' @importFrom Racmacs srFill
#' @method fortify racmap
#' @export
fortify.racmap <- function(model, data, ...) {
  dd <- as.data.frame(rbind(agCoords(model),
                            srCoords(model)
                            )
                      )

  names(dd) <- c('x', 'y')

  dd$fill  <- c(agFill(model),
                rep('grey', length(srFill(model)))
                )

  dd$type <- c(rep('AG', length(agFill(model))),
               rep('SR', length(srFill(model))))

  return(dd)
}


#' ggacmap
#'
#' @param acmap AC map
#'
#' @return ggplot object
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 aes_
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_y_reverse
#' @importFrom ggplot2 scale_size
#' @importFrom ggplot2 scale_size_identity
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_color_identity
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 coord_fixed
#' @export
ggacmap <- function(acmap) {
  p <- ggplot(acmap, aes_(x = ~x,
                         y = ~y,
                         color = ~fill,
                         shape = ~type)
              )

  rx <- floor(range(p$data$x))
  ry <- floor(range(p$data$y))

  p + geom_point(aes(size=3)) +
    scale_y_reverse(breaks = seq(ry[1], ry[2])) +
    scale_size_identity() +
    scale_shape_manual(values=c(16, 0)) +
    scale_x_continuous(breaks = seq(rx[1], rx[2])) +
    scale_color_identity(guide = "legend") +
    theme_bw() + xlab(NULL) + ylab(NULL) +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          legend.position='none') +
    coord_fixed()
}

