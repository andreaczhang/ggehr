#' Make drug use events
#' 
#' @description
#' Collect results from the new diet discovery, along with the current diet.
#' 
#' @param data_drug_use drug dataframe
#' @return A plot
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



#' Make drug use events
#' 
#' @description
#' Collect results from the new diet discovery, along with the current diet.
#' 
#' @param data_prescribe drug dataframe
#' @param plot_obj tmin
#' @return A datafrme with drug uses
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



#' Make drug use events
#' 
#' @description
#' Collect results from the new diet discovery, along with the current diet.
#' 
#' @param data_prescribe drug dataframe
#' @param plot_obj tmin
#' @import ggiraph
#' @return A datafrme with drug uses
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


#' Make drug use events
#' 
#' @description
#' Collect results from the new diet discovery, along with the current diet.
#' 
#' @param info_text drug dataframe
#' @param plot_obj tmin
#' @import ggtext
#' @return A datafrme with drug uses
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


#' Make drug use events
#' 
#' @description
#' Collect results from the new diet discovery, along with the current diet.
#' 
#' @param loc_obj drug dataframe
#' @param keep_text tmin
#' @param keep_legend tmax
#' @param keep_time description
#' @return A dataframe with drug uses
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


#' Make drug use events
#' 
#' @description
#' Collect results from the new diet discovery, along with the current diet.
#' 
#' @param x drug dataframe
#' @param n tmin
#' @return A datafrme with drug uses
#' @export

breakpoints <- function(x, n=8){
  # this function is to be used in the ggplot to set breakpoints
  # x <- 2250:2500
  br <- pretty(x, n = n)
  br
}



