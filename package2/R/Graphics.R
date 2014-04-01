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
    geom_point(...)
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
                            size=1.0, 
                            alpha=0.2,
                            facet_formula = countryAbbrev ~ nest ){
  p <- triPlot(subset(data, method!="orig"), 
               gamma, alpha, beta,
               labels=c("gamma", "alpha", "beta"),
               grid_lines=grid_lines,  aes_string=aes_string, 
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

#' @export
spaghettiPlot <- function(energyType='none', 
                              nest, 
                              data=loadCESSpaghettiGraphData(energyType=energyType, 
                                                             nest=nest, ...), 
                              split = union(all.vars(facet_formula), "Year"), 
                              geom_actual = geom_line, 
                              facet_formula = Country ~ nest,
                              alpha=0.15, level = 1, ...){
  #############
  # Returns a graph that shows lines for each resample model
  # for the energyType and nest arguments.
  # Alternatively, you can specify the data argument only.
  # You can obtain data with the loadCESSpaghettiGraphData function. 
  # For example:
  # data <- loadCESSpaghettiGraphData(energyType="Q", nest="(kl)e", 
  #         archive="data_archive/data_resample_2013-11-16_Best_Algorithm_Saving_Models_n=50.zip")
  # cesSpaghettiGraph(data=data)
  ##
  alph = .5 * (1 - level)
  data$alph = alph
  split <- intersect( split, names(data) )
  seData <- ddply( subset(data, Type=="fitted"), split, summarise, 
                   n = length(iGDP),
                   lower= quantile(iGDP, alph[1]),
                   upper= quantile(iGDP, 1-alph[1]),
                   iGDP = iGDP[1]
  ) 
  # return(seData)
  # return(names(data))
  # graph <- qplot(Year, iGDP, group=ResampleNumber, data=data, facets = ~Country, geom="line", alpha=I(alpha))
  graph <- ggplot( data=subset(data, Type=="actual"), aes(Year, iGDP)) 
  graph <- graph + geom_smooth(aes(ymin=lower, ymax=upper), col=NA, fill="gray10", 
                               lty=1, size=.5, data=seData, stat="identity")
#  graph <- graph + geom_area(fill="black", shape=1, alpha=0.05)
  graph <- graph + geom_line(color="black", size=.4, shape=1, alpha=1.0)
  graph <- graph + geom_line(data=subset(data, !Resampled & Type =="fitted"), color="gray90", size=.4, shape=1, alpha=1.0)
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

#' @export
historicalPlot <- function(countryAbbrev){
  ####################################
  # Creates a graph that displays all of the factors of production for all countries 
  #	(if you leave off countryAbbrev)
  # or a specific country (if you supply a 2-letter abbreviation for a country that we know).
  # for converting between graphs in a paper and graphs in a beamer presentation.
  ##
  # Code that deals with items that are specific to whether we want all countries or a specific country.
  if (missing(countryAbbrev)){
    # We want a graph with panels for all countries
    data <- loadData("All")
    # Now set the order for the countries
    data$Country <- factor(data$Country, levels=countryAbbrevs)
  } else {
    # We want only a specific country
    data <- loadData(countryAbbrev)
  }
  graph <- ggplot(aes(x=Year), data=data) +
    geom_line(aes(y=iGDP, lty="y")) +
    geom_line(aes(y=iCapStk, lty="k")) +
    geom_line(aes(y=iLabor, lty="l")) +
    geom_line(aes(y=iQ, lty="q")) +
    facet_grid( Country ~ ., scales="free_y" ) +
    ylab("Indexed (1980=1 or 1991=1)") +
    xlab("") +
    scale_linetype_manual(name="",   
                          limits=c("y", "k", "l", "q"), 
                          values=c("y"=1, "k"=5, "l"=2, "q"=3) ) +
    xy_theme()
    
  return(graph)
}
