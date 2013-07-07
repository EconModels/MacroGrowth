require(ggplot2)
require(mosaic)

tri2x <- function(x,y,z) {
  return( x * 0 + y * 1 + z * 0.5 )
}

tri2y <- function(x,y,z) {
  return( x * 0 + y * 0 + z * 1 )
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
            plot.background = element_blank()
            )
  }  

triPlot <- function(data, x, y, z, labels=c("lambda", "alpha", "beta"), 
                    parse=TRUE, n.grid=4, aes_string="", ...) {
  h <- seq(0, 1, by=1/n.grid)
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
    expand_limits( x=c(-.1,1.1), y=c(-.05,1.15) ) +
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

# an example use

# first we create some fake data with four "countries"
ddd <- data.frame( x=runif(80) ) 
ddd <- transform(ddd, y = runif(80, 0, 1-x))
ddd <- transform(ddd, z = runif(80, 0, 1-x-y))
ddd <- transform(ddd, country=resample(toupper(letters[1:4]), 80))

# Now we plot the data by providing a data frame, the x, y, and z coordinates
# computed in that data frame, and any optional arguments (like size and alpha).
# The order of x, y, and z is top, lower left, lower right.
# The guidelines are at fixed values of one of the three.  n.grid determines the 
# number of gridlines to use.
#
# This is probably still a bit brittle and may not look good at extreme sizes, but
# this will get us started.

triPlot( ddd, x,y,z, size=3, alpha=.5 ) + facet_wrap( ~ country )

exampleData <- rbind(
  resampleFits( "cde", "TZ", "X", method="wild", n=100),
  resampleFits( "cde", "US", "X", method="wild", n=100),
  resampleFits( "cde", "UK", "X", method="wild", n=100),
  resampleFits( "cde", "CN", "X", method="wild", n=100)
)

triPlot( data=exampleData, lambda, alpha, beta, n.grid=5, 
         aes_string="color=method", size=3, alpha=.5 ) + 
  geom_point( data=subset(exampleData, method=="orig"), color="red", alpha=1, size=3) +
  facet_wrap( ~ countryAbbrev ) 
