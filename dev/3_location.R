# location ----


loc <- ggehr::location
loc1 <- dplyr::filter(loc, ID == 1)
loc1

# data_location[order(time)]



# need a cross info list for location names
# and location types, for coloring
# library(data.table)
make_location <- function(data_location, 
                          tadmin, 
                          los){
  
  # data_location <- loc1
  # sort based on time (works for data.table)
  data_location <- data.table::setDT(data_location)
  data_location <- data_location[order(time)]
  dl <- data_location[, .(time, post)]
  setnames(dl, old = 'post', new = 'location_code')
  
  # check tadmin
  # if less than time of first loc, suggests missing
  # add unknown
  if(tadmin < min(dl$time)){
    dl <- rbind(data.table(time = tadmin, location_code = 'Unknown'),
                dl)
    print('Unknown first location added')
  }
  
  # compute discharge time
  tdischarge <- tadmin + los
  all_timestamps <- c(dl$time, tdischarge)
  
  dl$time_to <- dplyr::lead(all_timestamps, n=1)[1:nrow(dl)]
  
  # make it into long format
  loc_list <- list()
  for(i in 1:nrow(dl)){
    loc_list[[i]] <- loc_expand(dl[i, ])
  }
  loc_long <- do.call(rbind, loc_list)
  
  loc_long$label <- 'Location'
  
  # load location meta data
  location_meta <- ggehr::location_meta
  
  loc_long <- dplyr::left_join(loc_long, location_meta)
  loc_compact <- dplyr::left_join(dl, location_meta)
  
  
  
  return(list(loc_compact = loc_compact, 
              loc_long = loc_long))
}



# loc_expand(dl[1,])

loc_expand <- function(x){
  x <- as.data.frame(x)
  from <- x$time
  to <- x$time_to
  where <- x$location_code
  df <- data.table(time = seq(from, to, by = 1),
                   location_code = where
  )
  return(df)
}






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



# location on top of drug figure ----

# require time info
# just select the locations of interst
# next stage 
p1
p1 + annotate('rect', xmin = 2276, xmax = 2280, ymin = 0, ymax = 2, color = 'black')
library(patchwork)
p1 + p2 + plot_layout(ncol = 1, heights = 2:1)





# ________ ----
# TESTS WITH DATA ----

loc <- ggehr::location
demographics <- ggehr::demographics

id <- 10

dpatient_demog <- dplyr::filter(demographics, ID == id)

dpatient_loc <- dplyr::filter(loc, ID == id)

# dpatient_loc




dloc <- make_location(data_location = dpatient_loc, 
                      tadmin = dpatient_demog$t0, 
                      los = dpatient_demog$los)



plot_location(loc_obj = dloc)
plot_location(loc_obj = dloc, keep_legend = T)


# put together ----

library(patchwork)
p1 + p2 + plot_layout(ncol = 1, heights = 2:1)




