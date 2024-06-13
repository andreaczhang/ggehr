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



# fake data from ahus ----

library(readxl)
demographics <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "Demograpphics")
catheters <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "Tube")
abpres <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "AB_pres")
abuse <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "ABU")
location <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "Location")


library(dplyr)

# ab use ----
# try patient 1, abuse 
d1 <- filter(abuse, ID == 1)
d1
# two drugs, different doses


make_event_ab_use <- function(d_ab, 
                              t0 = NULL, 
                              tmax = NULL){
  # d_ab <- d1
  # set time limits
  if(is.null(t0)){
    t0 <- min(d_ab$time)
  }
  if(is.null(tmax)){
    tmax <- max(d_ab$time)
  }
  ab_name <- unique(d_ab$ab_name)
  
  # make skeleton
  skeleton <- expand.grid(time = seq(t0, tmax), 
                          ab_name = ab_name)
  # attach label to the original data
  d_ab$event_label <- 'Yes'
  dmerge <- dplyr::left_join(skeleton, d_ab)
  
  return(dmerge)
}




d1
d1 <- filter(abuse, ID == 1)
d1

d1_full <- make_event_ab_use(d1)
d1_full <- make_event_ab_use(d1, t0 = 2254, tmax = 2254+112)

d1_full
tail(d1_full)
#head(d1_full)
#filter(d1_full, event_label == 'Yes')
p <- ggplot(d1_full, aes(x = time, y = ab_name)) + 
  geom_tile(aes(fill = event_label), color = 'white') 

p

# when there are lots of time points, the color becomes hard to distinguish
# natural extension: time t0 tmax plot against t admission and discharge


# ab prescription -----

abpres
abp1 <- filter(abpres, ID == 1)
abp1

p + geom_point(data = abp1[c(1,4),], aes(x = time, y = ab_name))
p + geom_point(data = abp1, aes(x = time, y = ab_name))





# locations ----

location
loc1 <- filter(location, ID == 1)
loc1

dloc <- loc1  
t0 <- min(dloc$time)
tmax <- max(dloc$time)
t0 <- 2254
tmax <- 2254+112
post <- unique(dloc$post)

skeleton <- expand.grid(time = seq(t0, tmax), 
                        post = post)

# attach label
dloc$event_label <- 'Yes'
dmerge <- dplyr::left_join(skeleton, dloc)
head(dmerge)
filter(dmerge, event_label == 'Yes')


p2 <- ggplot(dmerge, aes(x = time, y = post)) + 
  geom_tile(aes(fill = event_label), color = 'white') 

library(patchwork)
p + p2 + plot_layout(ncol = 1)

# should be sorted over time 
# also, time end should be matching discharge
demographics
2254+112

# for location, it would be ideal if multiple locations can be combined into 
# ONE horizontal bar, with different colors



# catheter ----

catheters
# complication here is that there might be multiple

catheters$ID |> table()
# patient 6 has 60 records 

cat1 <- filter(catheters, ID == 1)

cat1$t_use <- round(cat1$use/60, 0)
demographics

cat6 <- filter(catheters, ID == 6)
cat6$tube |> table()





