# visualize personal temporal 
library(ggplot2)


d <- readRDS('dev/three_patient.rda')
d2 <- d[[3]]

d2


# this data is already quite clean, but still need to fill the gaps

impute_empty_rec <- function(personal_rec){
  
  # personal_rec <- d2 
  t0 <- unique(personal_rec$t0)
  tmax <- max(personal_rec$time)
  events <- unique(personal_rec$event)
  
  skeleton <- expand.grid(time = seq(t0, tmax), 
                          event = events)
  
  # attach label
  personal_rec$event_label <- 'Yes'
  dmerge <- dplyr::left_join(skeleton, personal_rec)
  
  return(dmerge)
}


d2_full <- impute_empty_rec(d2)
d2_full

p <- ggplot(d2_full, aes(x = time, y = event)) + 
  geom_tile(aes(fill = event_label), colour = "white") 

p



# is it possible to add facet? to indicate a different type of event?

d2_full
dd <- data.table::data.table(d2_full)

dd

# add facet ----
# this is useful when adding other procedures or durations

dd[, type := 'ab_green']
dd[event == 'vancomycin', type := 'ab_yellow']
dd

ggplot(dd, aes(x = time, y = event)) + 
  geom_tile(aes(fill = event_label), colour = "white")  + 
  #facet_wrap(~type, ncol = 1, scale = 'free') + 
  facet_grid(rows = vars(type), scales = "free", space='free')
  theme_minimal()


  
  
  
  
# _____----- 
# if need to add more text information
dt <- data.frame(title = 'c', name = '2')
dt
gt <- gridExtra::tableGrob(dt)


gridExtra::grid.arrange(p, gt)

