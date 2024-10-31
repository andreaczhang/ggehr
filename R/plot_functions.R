#' Plot drug use over time
#' 
#' @description
#' Plot drug use over time. For antibiotics, a tricolor systme (WHO AWaRe) is applied.
#' 
#' @param data_drug_use drug dataframe
#' @return A plot that visualizes drug use events over time
#' @export



plot_drug_use <- function(data_drug_use){
  
  if(!'Category' %in% colnames(data_drug_use)){stop('Need drug category')}
  
  # information for WHO ab color category
  ab_category <- c('Access','Watch','Reserve')
  ab_color <- c('#048a04','#f0c11a','#a11c15')
  
  
  p <- ggplot(data_drug_use, aes(x = time, y = label)) 
  p <- p + geom_tile(aes(fill = Category)) 
  p <- p + theme_bw()
  
  # modify color
  p <- p + scale_fill_manual(breaks = ab_category, 
                             values = ab_color, 
                             na.value = 'transparent')
  # make axis 45 degrees
  p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  # add more ticks
  p <- p + scale_x_continuous(breaks = breakpoints)
  
  # remove x, y axis title
  p <- p + theme(axis.title.y = element_blank(), 
                 axis.title.x = element_blank())
  p <- p + theme(panel.grid.minor = element_blank())
  p <- p + theme(panel.grid.major.y = element_blank())
  # legend title (optional)
  p <- p + labs(fill = 'AWaRe')
  p
  return(p)
  
}



#' Plot drug prescription
#' 
#' @description
#' Plot drug prescription events over time, on top of drug use plot. It is necessary to supply a drug use plot first.
#' 
#' @param data_prescribe drug prescription dataframe
#' @param plot_obj plot object from drug use
#' @return An enhanced plot with drug prescription events
#' @export

plot_drug_prescribe <- function(data_prescribe, plot_obj){
  
  # TO DO:
  # should also return data for checks
  # should allow disabling unused drugs 
  p <- plot_obj + geom_point(data = data_prescribe, 
                             aes(x = time, 
                                 y = ab_name))
  return(p)
}



#' Plot drug prescription interactively with additional information
#' 
#' @description
#' Plot drug prescription interactively with additional information
#' 
#' @param data_prescribe drug prescription dataframe
#' @param plot_obj plot object from drug use
#' @import ggiraph
#' @return An enhanced plot with drug prescription events
#' @export

plot_drug_prescribe_interactive <- function(data_prescribe, 
                                            plot_obj){
  
  # TO DO: 
  # more tweaking on the font size
  # create interactive obj 
  pint <-  plot_obj + ggiraph::geom_point_interactive(
    data = data_prescribe, 
    aes(x = time, 
        y = ab_name,
        tooltip = purpose, 
        data_id = purpose), 
    size = 3, 
    hover_nearest = T)
  
  # pass into girafe
  p <- ggiraph::girafe(ggobj = pint, 
                       width_svg = 8,
                       height_svg = 4,
                       options = list(
                         opts_hover(css = "fill:black; stroke: yellow;"),
                         opts_hover_inv(css = "opacity:0.2;"),
                         opts_zoom(max = 10)
                       ))
  
  p
  return(p)
  
}


#' Plot demographic information card
#' 
#' @description
#' Plot demographic information card
#' 
#' @param info_text Text information, parsed to be plotted. See make_demographic_info() for more.
#' @param plot_obj plot object from drug use
#' @import ggtext
#' @return An enhanced plot that has demographic information
#' @export

plot_info_demographic <- function(info_text, plot_obj){
  
  p <- plot_obj + labs(title = info_text) + 
    theme(
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        size = 15,
        lineheight = 1,
        padding = margin(5.5, 5.5, 5.5, 5.5),
        margin = margin(0, 0, 5.5, 0),
        halign = 0.5,
        r = grid::unit(3, 'pt'),
        fill = "lightgrey"
      ))
  
  return(p)
}


#' Plot location 
#' 
#' @description
#' Plot location information as color blocks
#' 
#' @param loc_obj location object
#' @param keep_text Keep text. Logical, default is True
#' @param keep_legend Keep legend. Logical, default is True
#' @param keep_time Keep time. Logical, default is True
#' @return A plot of location information
#' @export

plot_location <- function(loc_obj, 
                          keep_text = T, 
                          keep_legend = F, 
                          keep_time = T){
  
  # pd <- dl1
  # loc_obj <- dloc
  
  pd <- loc_obj$loc_long
  p <- ggplot(pd, aes(x = time, y = label, height = .75)) + 
    geom_tile(aes(fill = location_display))
  
  p <- p + theme_minimal()
  
  # change color 
  # for the moment just use the standard
  p <- p + scale_fill_brewer(palette = 'Spectral')
  
  
  # make axis 45 degrees
  p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  # add more ticks
  p <- p + scale_x_continuous(breaks = breakpoints)
  # remove x, y axis title
  p <- p + theme(axis.title.y = element_blank(), 
                 axis.title.x = element_blank())
  p <- p + theme(panel.grid.minor = element_blank())
  p <- p + theme(panel.grid.major.y = element_blank())
  
  # whether keep legend
  # legend
  if(keep_legend == T){
    p <- p + theme(legend.position = 'bottom',
                   legend.title = element_blank())
    nrow_legend <- ceiling(length(unique(pd$location_code))/3)
    p <- p + guides(fill = guide_legend(nrow = nrow_legend))
  }else{
    p <- p + theme(legend.position = 'none')
  }
  
  if(keep_text == T){
    # display text rather than legend
    # compute the mid point between two times to display text
    lc <- loc_obj$loc_compact
    lc$tmid <- (lc$time + lc$time_to)/2
    p <- p + annotate('text',
                      x = lc$tmid,
                      y = 1,
                      label = lc$location_display,
                      angle = 45)
    
    # if want to shift up/down, change y = 1.05
  }
  
  if(keep_time == F){
    p <- p + theme(axis.text.x = element_blank())
  }
  
  return(p)
  
}


#' Breakpoint
#' 
#' @description
#' Make pretty break points for plotting
#' 
#' @param x time vector
#' @param n number of breakpoints
#' @return a vector of breakpoints
#' @export

breakpoints <- function(x, n=8){
  # this function is to be used in the ggplot to set breakpoints
  # x <- 2250:2500
  br <- pretty(x, n = n)
  br
}



