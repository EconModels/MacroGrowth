require(ggplot2)
require(mosaic)
require(scales)

tri2x <- function(x,y,z) {
  return( x * 0.5 + y * 0 + z * 1 )
}

tri2y <- function(x,y,z) {
  return( x * 1 + y * 0 + z * 0 )
}

tri_theme <- function(base_size=12, base_family = "", base_theme=theme_bw) {
    base_theme(base_size = base_size, base_family = base_family) %+replace% 
      theme(panel.border = element_blank(), 
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.background = element_blank(),
            strip.background =  element_rect(fill = NA, colour = "gray50")
            )
}  

xy_theme <- function(base_size=12, base_family = "", base_theme=theme_bw) {
  base_theme(base_size = base_size, base_family = base_family) %+replace% 
    theme(# panel.border = element_blank(), 
          # panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          strip.background =  element_rect(fill = NA, colour = "gray50")
    )
}

triPlot <- function(data, x, y, z, labels=c("gamma", "alpha", "beta"), 
                    parse=TRUE, grid_lines=4, aes_string="", ...) {
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
    expand_limits( x=c(-.2,1.2), y=c(-.05,1.15) ) +
    tri_theme() + 
    geom_segment(aes(x=tri2x(h,0,1-h), xend = tri2x(h, 1-h, 0), 
                     y=tri2y(h,0,1-h), yend = tri2y(h, 1-h, 0)),
                     data=points, color="gray70") +
    geom_segment(aes(x=tri2x(0,h,1-h), xend = tri2x(1-h, h, 0), 
                     y=tri2y(0,h,1-h), yend = tri2y(1-h, h, 0)),
                     data=points, color="gray70") +
    geom_segment(aes(x=tri2x(0,1-h,h), xend = tri2x(1-h, 0, h), 
                     y=tri2y(0,1-h,h), yend = tri2y(1-h, 0, h)),
                     data=points, color="gray70") +
    geom_text(aes(label=label, x=x, y=y, hjust=hj, vjust=vj), color="black", 
              data=data.frame(label=rep(labels, length.out=3),
                              x=c(.5,-.02,1.02), 
                              y=c(1.02,0,0),
                              hj = c(.5,1,0),
                              vj = c(0,0.5,0.5)
                              ), 
              parse=parse) +
    geom_point(...)
}

Log <- function(x, ...) {
  if (is.null(x) ) { 
    return(rep(1,length(x)))
  }
  log(x, ...)
}

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

standardTriPlot <- function(data, 
                            grid_lines=5, 
                            aes_string="", #"color=lambda", 
                            orig_color = "gray70",
                            size=1.0, 
                            alpha=0.2,
                            facet_formula = countryAbbrev ~ nest ){
  p <- triPlot(subset(data, method!="orig"), 
               gamma, alpha, beta,
               labels=c("gamma", "alpha", "beta"),
               grid_lines=grid_lines,  aes_string=aes_string, 
               size=size, alpha=alpha ) + 
    geom_point(data=subset(data, method=="orig"), 
               color=orig_color, alpha=1, size=3, shape=1) 
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

standardScatterPlot <- function(data, mapping, orig_color="gray70", size=2.0, alpha=0.4, facet_formula = countryAbbrev ~ nest) {
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