#' Some Graphics functions
#' 
#' Some graphics functions for working with Economic Models.
#'
#' @import ggplot2
#' @import mosaic
#' @import scales

#' @export
tri2x <- function(x,y,z) {
  return( x * 0.5 + y * 0 + z * 1 )
}

#' @export
tri2y <- function(x,y,z) {
  return( x * 1 + y * 0 + z * 0 )
}

#' @export
tri_theme <- function(base_size=12, base_family = "", base_theme=theme_bw) {
    xy_theme(base_size = base_size, base_family = base_family) %+replace% 
      theme(panel.border = element_blank(), 
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.background = element_blank(),  
            strip.background =  element_rect(fill = NA, colour = xy_theme()$axis.text$colour)
            )
}  

#' @export
xy_theme <- function(base_size=12, base_family = "", base_theme=theme_bw, label_colour="gray50") {
  base_theme(base_size = base_size, base_family = base_family) %+replace% 
    theme(# panel.border = element_blank(), 
          # panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          strip.text = element_text(colour=label_colour, size=.8 * base_size),
          strip.background =  element_rect(fill = NA, colour = NA),
          legend.background = element_rect(fill=NA, colour=NA),
          legend.key= element_rect(fill=NA, colour=NA),
          legend.text=element_text(colour=label_colour),
          axis.text = element_text(colour=label_colour, size=.8 * base_size), 
          axis.ticks = element_blank(),
          axis.title = element_text(colour=label_colour, size=base_size)
    )
}

#' @export
triPlot <- function(data, x, y, z, labels=c("gamma", "alpha", "beta"), 
                    parse=TRUE, grid_lines=4, aes_string="", geom=geom_point, ...) {
  h <- seq(0, 1, by=1/grid_lines)
  points <- data.frame( h=h )
  if ( nchar(aes_string) > 0 ) aes_string <- paste(",", aes_string)
  
  command <- paste("ggplot( aes(x=tri2x(", 
                   deparse(substitute(x)), ",", 
                   deparse(substitute(y)), ",", 
                   deparse(substitute(z)),
                   "), y=tri2y(",
                   deparse(substitute(x)), ",", 
                   deparse(substitute(y)), ",", 
                   deparse(substitute(z)),
                   ")", aes_string, "), data=", deparse(substitute(data)), " )", sep="")

    p <- eval(parse(text=command),envir=parent.frame())                   
    p + 
    expand_limits( x=c(-.2,1.2), y=c(-.05,1.25) ) +
    tri_theme() + 
    geom_segment(aes(x=tri2x(h,0,1-h), xend = tri2x(h, 1-h, 0), 
                     y=tri2y(h,0,1-h), yend = tri2y(h, 1-h, 0)),
                     data=points, color=xy_theme()$panel.grid.major$colour) +
    geom_segment(aes(x=tri2x(0,h,1-h), xend = tri2x(1-h, h, 0), 
                     y=tri2y(0,h,1-h), yend = tri2y(1-h, h, 0)),
                     data=points, color=xy_theme()$panel.grid.major$colour) +
    geom_segment(aes(x=tri2x(0,1-h,h), xend = tri2x(1-h, 0, h), 
                     y=tri2y(0,1-h,h), yend = tri2y(1-h, 0, h)),
                     data=points, color=xy_theme()$panel.grid.major$colour) +
    geom_text(aes(label=label, x=x, y=y, hjust=hj, vjust=vj), 
              color=xy_theme()$axis.title$colour, 
              size=4,
              data=data.frame(label=rep(labels, length.out=3),
                              x=c(.5,-.02,1.02), 
                              y=c(1.02,0,0),
                              hj = c(.5,1,0),
                              vj = c(0,0.5,0.5)
              ), 
              parse=parse) +
    do.call( geom, list(...) )
      
}

#' @export
Log <- function(x, ...) {
  if (is.null(x) ) { 
    return(rep(1,length(x)))
  }
  log(x, ...)
}

#' @export
sigma_trans <- function(base = exp(1)) {
  trans <- function(x) pnorm(log(x, base))
  inv <- function(x) base ^ qnorm( x )
  
  trans_new(paste0("sigma-", format(base)), 
            transform = function(x) {
              # print(match.call())  
              pnorm(Log(x, base))
            },
            inverse = function(x) base ^ qnorm( x ),
            breaks=function(x) { return( base^qnorm((seq(0,1, length.out=5) ))) },
            domain = c(0, Inf))
}

#' @export
standardTriPlot <- function(data, 
                            grid_lines=5, 
                            aes_string="", #"color=lambda", 
                            orig_color = "gray70",
                            mapping = aes(),
                            size=1.0, 
                            alpha=0.2,
                            facet_formula = country ~ nest ){
  p <- triPlot(subset(data, method!="orig"), 
               gamma, alpha, beta,
               labels=c("gamma", "alpha", "beta"),
               grid_lines=grid_lines,  aes_string=aes_string, mapping=mapping, 
               size=size, alpha=alpha ) + 
    geom_point(data=subset(data, method=="orig"), 
               color=orig_color, alpha=1, size=3, shape=10) 
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

#' @export
standardScatterPlot <- function(data, mapping=aes(), orig_color="gray70", size=2.0, alpha=0.4, 
                                facet_formula = countryAbbrev ~ nest) {
  p <- ggplot( data=subset(data, method != "orig"), mapping ) 
  p <- p + geom_point(size=size, alpha=alpha) 
  # p <- p + geom_hex( bins=60) 
  
  p <- p + geom_point(data=subset(data, method=="orig"),  
                      color=orig_color, size=4, shape=10 )
  if ( !is.null(facet_formula) ) {
    if ( length(facet_formula)==2 ) {
      p <- p + facet_wrap( facet_formula )
    } else { 
      p <- p + facet_grid( facet_formula )
    }
  }  
  if ("color" %in% mapping || "colour" %in% mapping) {
    p <- p + scale_colour_gradient(expression(lambda), high="navy", low="skyblue") 
  }
  p + xy_theme()
}

#' @export
spaghettiPlot <- function(data, 
                          variables = union(all.vars(facet_formula), "Year"), 
                          geom_actual = geom_line, 
                          facet_formula = Country ~ nestStr,
                          alpha=0.15, 
                          level = 1, ...){
  alph = .5 * (1 - level)
  data$alph = alph
  variables <- intersect( variables, names(data) )
  seData <- plyr::ddply( subset(data, resampled), variables, plyr::summarise, 
                   n = length(iGDP.hat),
                   lower = quantile(iGDP.hat, alph[1]),
                   upper = quantile(iGDP.hat, 1-alph[1]),
                   center = iGDP.hat[1]
  ) 
  graph <- ggplot(aes(y=iGDP, x=Year), 
                  data=subset(data, !resampled)) 
  
  graph <- graph + 
    geom_smooth(aes(x=Year, ymin=lower, ymax=upper, y=center), 
                data=seData,
                col=NA, fill="gray10", 
                lty=1, size=.5, stat="identity")
  
  graph <- graph + 
    geom_line( aes(x=Year, y=iGDP),
               data = subset(data, !resampled),
               color="black", size=.4, shape=1, alpha=1.0)
  
  graph <- graph + 
    geom_line(aes(x=Year, y=iGDP.hat), 
              data=subset(data, !resampled), 
              color="gray90", size=.4, shape=1, alpha=1.0)
  
  if (!is.null( facet_formula ) ) {
    if (length(facet_formula) == 2) {
      graph <- graph + facet_wrap( facet_formula, scales="free_y" )
    } else {
      graph <- graph + facet_grid( facet_formula, scales="free_y" )
    }
  }
  graph <- graph + xlab("")
  graph <- graph + xy_theme()
  return(graph)
}

#' Create graph of historical data.
#' 
#' @param data a data frame containing data to be plotted.
#' @param mapping the argument to the geom_line function. 
#' Use for setting, x, y, color, among other things: mapping=aes(x=scale, y=lambda, color=country)
#' @param facet_formula a formula of the form \code{y ~ x} where \code{y} is the varible to facet in 
#' the y direction 
#' and \code{x} is the variable to facet in the x direction.
#' @param line_types is a vector of linetype identifiers to be applied as the line types for the \code{group}s.
#' @details This function returns a figure with facets specified by \code{facet_formula}
#' and various lines specified by \code{line_formula}.
#' You may have to use the package \code{reshape2} to "melt" your data to the correct form before passing
#' it to this function.
#' @export
historicalPlot <- function(data, mapping, facet_formula, line_types){
  graph <- ggplot(data, mapping=mapping) + 
    geom_line(mapping) +
    facet_grid( facet_formula, scales="free_y" ) + 
    scale_linetype_manual(name="", values=line_types) + 
    xy_theme()
  return(graph)
}