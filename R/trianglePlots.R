#' @rdname triplot
#' @importFrom ggplot2 ggproto Stat
#' @export
StatTriangle <-
  ggplot2::ggproto("StatTriangle", ggplot2::Stat,
          compute_group = function(data, scales) {
            x0 <- c(0, 0)
            y0 <- c(1, 0)
            z0 <- c(0.5, 1)
            T <- as.matrix(data[, c("x", "y", "z")]) %*% rbind(x0, y0, z0)
            data$xorig <- data$x
            data$yorig <- data$y
            data$zorig <- data$z
            data$x <- T[,1]
            data$y <- T[,2]
            data['z'] <- NULL
            data
          },

          required_aes = c("x", "y", "z")
)

#' @rdname triplot
#' @export
stat_triangle <-
  function(
    mapping = NULL, data = NULL,
    geom = "point", position = "identity",
    show.legend = NA, inherit.aes = TRUE, ...) {
    layer(
      stat = StatTriangle, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(...)
    )
  }

#' Triangle Plots
#'
#' Triangle Plots
#'
#' @rdname triplot
#' @export
#' @examples
#' ddd <- dplyr::tibble( x=c(1, 0, .5, .25, 0), y = c(0, .2, .2, .25, .8)) %>% mutate(z = 1 - x - y)
#' ggplot()  +
#'   tri_theme() +
#'   tri_grid(colour="red") +
#'   tri_labels(size=10, padding=.05) +
#'   geom_path(data=ddd, aes(x=x, y=y, z=z), stat="triangle", size=2, alpha=.3) +
#'   geom_point(data=ddd, aes(x=x, y=y, z=z), stat="triangle", size=3, alpha=.6, colour="navy")
#' ggplot()  +
#'   tri_theme() +
#'   tri_grid(colour="red", labels = c("x", "y", "z")) +
#'   tri_labels(size=10, padding=.05) +
#'   geom_path(data=ddd, aes(x=x, y=y, z=z), stat="triangle", size=2, alpha=.3) +
#'   geom_point(data=ddd, aes(x=x, y=y, z=z), stat="triangle", size=3, alpha=.6, colour="navy")
#'
tri_grid <- function(colour = color, color = "gray70", grid_lines = 5,
                     labels = c("alpha[1]", "alpha[2]", "alpha[3]"),
                     ...) {
  h <- seq(0, 1, by=1/grid_lines)
  n <- length(h)
  zero <- rep(0, n)
  one <- rep(1, n)
  ids <- 1:n

  lines_data <- dplyr::tibble(
    x = c(   h,    h, zero,  1-h, zero,  1-h ),
    y = c(zero,  1-h,    h,    h,  1-h, zero ),
    z = c( 1-h, zero,  1-h, zero,    h,    h ),
    id = c(ids,  ids, ids + n, ids + n, ids + 2*n, ids + 2*n)
  )
  geom_line(
    data = lines_data, stat="triangle",
    aes(x=x, y=y, z=z, group=id), colour = colour, ...
    )
}

#' @rdname triplot
#' @export
tri_labels <-
  function(labels = c("alpha[1]", "alpha[2]", "alpha[3]"), parse = TRUE,
           colour = color, color = "gray60", size = 4, padding = 0.1, ...) {
  text_data <- dplyr::tibble(
    x = c(1 + padding, -.5 *padding, -.5 * padding),
    y = c(-.5 *padding, 1 + padding, -.5 * padding),
    z = c(-.5 *padding, -.5 * padding, 1 + padding),
    label = rep(labels, length.out = 3)
  )
  geom_text(
    data = text_data, stat="triangle",
    aes(label = label, x = x, y = y, z = z),
    vjust = 0.5, hjust = 0.5,
    colour=colour, size=size,
    parse = parse,
    ...
  )
}

#' @rdname triplot
#' @export
geom_tripoint <- function(..., stat = "triangle") {
  geom_point( ..., stat=stat)
}

#' @rdname triplot
#' @export
geom_triline <- function(..., stat = "triangle") {
  geom_line( ..., stat=stat)
}

#' @rdname triplot
#' @export
geom_tripath <- function(..., stat = "triangle") {
  geom_path( ..., stat=stat)
}

#' @export
triPlot <-
  function(data, mapping,
           labels=c("alpha[1]", "alpha[2]", "alpha[3]"),
           parse=TRUE, grid_lines=4, aes_string = NULL,
           geom=geom_point, ...) {
  if (!is.null(aes_string))
    stop( "aes_string has been deprecated.",
          "Assign the output of aes() to mapping instead.")


  p <-  ggplot(data = data)

  q <- p +
    expand_limits( x=c(-.2,1.2), y=c(-.05,1.25) ) +
    tri_theme() +
    tri_grid(grid_lines = grid_lines) +
    tri_labels(labels = labels )
  q + do.call( geom, c(list(mapping=mapping, stat="triangle"), list(...)) )
}

#' @export
standardTriPlot <- function(
  data,
  mapping = aes(),
  grid_lines=5,
  orig_color = "gray70",
  labels=c("alpha[3]", "alpha[1]", "alpha[2]"),
  size=1.0,
  alpha=0.2,
  facet_formula = country ~ nest )
{

  p <- triPlot(subset(data, method!="orig"),
               mapping = modifyList(aes(x=alpha_1, y=alpha_2, z=alpha_3), mapping),
               labels=labels,
               grid_lines=grid_lines,
               size=size, alpha=alpha ) +
    geom_point(data=subset(data, method=="orig"),
               aes(x = alpha_1,
                   y = alpha_2,
                   z = alpha_3),
               color=orig_color, alpha=1, size=4, shape=10, stat="triangle")
  if ( !is.null(facet_formula) ) {
    if ( length(facet_formula)==2 ) {
      p <- p + facet_wrap( facet_formula )
    } else {
      p <- p + facet_grid( facet_formula )
    }
  }
  return(p)
  # scale_colour_gradient(expression(lambda), high="navy", low="skyblue")
}
