# development code for ab prescription and use 

library(readxl)
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggiraph)

# install.packages('ggiraph')


# fake data from ahus
demographics <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "Demograpphics")
# catheters <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "Tube")
abpres <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "AB_pres")
abuse <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "ABU")
# location <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "Location")





# try patient 1, abuse 
d1 <- filter(abuse, ID == 1)
d1
# two drugs, different doses


# the drug_df should have standard names
abuse <- rename(abuse, label = ab_name)
abuse <- rename(abuse, method = ab_method)



# data requires to have at least drug_name and time!

make_event_drug_use <- function(drug_df, 
                              tmin = NULL, 
                              tmax = NULL){
  # data_ab <- d1
  # set time limits
  # if no time is set, use the min/max time from the data
  if(is.null(tmin)){
    tmin <- min(drug_df$time)
  }
  if(is.null(tmax)){
    tmax <- max(drug_df$time)
  }
  
  # take the unique drug names
  label <- unique(drug_df$label)
  
  # make skeleton
  skeleton <- expand.grid(time = seq(tmin, tmax), 
                          label = label)
  
  # attach label to the original data
  drug_df$event <- 'Yes'
  dmerge <- dplyr::left_join(skeleton, drug_df)
  
  return(dmerge)
}





d1 <- filter(abuse, ID == 1)
d1

d1c <- left_join(d1, ablist_ahus, join_by(label == Norsk_AHUS))

d1_full <- make_event_drug_use(d1c)
# d1_full <- make_event_drug_use(d1, tmin = 2254, tmax = 2254+112)

d1_full
tail(d1_full)
head(d1_full)
#filter(d1_full, event_label == 'Yes')


# ________ ----
# plot ----
p <- ggplot(d1_full, aes(x = time, y = label)) + 
  geom_tile(aes(fill = event), color = 'white') 

p


# change color ----
d1_full

# d1c <- left_join(d1_full, ablist_ahus, join_by(label == Norsk_AHUS))

p <- ggplot(d1_full, aes(x = time, y = label)) 
p <- p + geom_tile(aes(fill = color_code)) 
p <- p + scale_fill_manual(breaks = abgroup, 
                           values = colorsab, 
                           na.value = 'transparent')
p <- p + theme_bw()
p

abgroup <- c('yellow',
                'green',
                'red')

colorsab <- c('#f0c11a', 
               '#048a04', 
               '#a11c15')





# when there are lots of time points, the color becomes hard to distinguish
# natural extension: time t0 tmax plot against t admission and discharge


# ab prescription -----
# ab prescription time on the plot
# add a table 

abpres
abp1 <- filter(abpres, ID == 1)
abp1

p + geom_point(data = abp1[c(1,4),], aes(x = time, y = ab_name))
p + geom_point(data = abp1, aes(x = time, y = ab_name))




# prescription table ----#
# dt <- data.frame(title = 'c', name = '2')

dt <- abp1[, c(3, 6, 7)]
dt
gt <- gridExtra::tableGrob(dt)


gridExtra::grid.arrange(p, gt)


# rather than table, might be more suitable for interactivitiy ontop of the dots

# tooltip: column of data to display
# data_id: 

pinter <- p + geom_point_interactive(data = abp1, 
                                     aes(x = time, 
                                         y = ab_name,
                                         tooltip = purpose, 
                                         data_id = purpose), 
                                     size = 3, hover_nearest = T)

# any other text need to be added before girafe it!

ppp <- girafe(ggobj = pinter, 
              width_svg = 8,
              height_svg = 4,
              options = list(
                opts_hover(css = "fill:black; stroke: yellow;"),
                opts_hover_inv(css = "opacity:0.2;"),
                opts_zoom(max = 10)
              ))
ppp





# other information ----

demographics
demop2 <- filter(demographics, ID == 2)

demop2
make_demographic_info(demo_df = demop2)

?stopifnot

make_demographic_info <- function(demo_df, 
                                  keep_id = T, 
                                  keep_sex = T, 
                                  keep_age = T, 
                                  keep_dept = T, 
                                  keep_admtype = T, 
                                  keep_t0 = T,
                                  keep_los = T){
  
  # demo_df <- demop1
  # check if it is only one patient data
  if(nrow(demo_df) != 1){stop('Should only have data for one patient at a time')}
  
  # ID
  if(keep_id == T){
    id_text <- paste0('**ID**: ', demo_df$ID)
  }else{
    id_text <- '**ID**: Unknown'
  }
  
  # sex
  if(keep_sex == T){
    sex_text <- paste0('**Sex**: ', demo_df$sex)
  }else{
    sex_text <- '**Sex**: Unknown'
  }
  
  # age
  if(keep_age == T){
    age_text <- paste0('**Age**: ', demo_df$age)
  }else{
    age_text <- '**Age**: Unknown'
  }
  
  # dept
  if(keep_dept == T){
    dept_text <- paste0('**Department**: ', demo_df$dept)
  }else{
    dept_text <- '**Department**: Unknown'
  }
  
  # admtype
  if(keep_admtype == T){
    admtype_text <- paste0('**Admission type**: ', demo_df$adm_type)
  }else{
    admtype_text <- '**Admission type**: Unknown'
  }
  
  # t admission
  if(keep_t0 == T){
    admtime_text <- paste0('**Time admission**: ', demo_df$t0)
  }else{
    admtime_text <- '**Time admission**: Unknown'
  }
  
  # los 
  if(keep_los == T){
    los_text <- paste0('**Length of stay (Hr)**: ', demo_df$los)
  }else{
    los_text <- '**Length of stay**: Unknown'
  }
  
  # make a paragraph or bullet points containing these information 
  title_text <- 'Patient record'
  
  # arrange the text ----# 
  # line 1: title
  line1 <- title_text
  # line 2: id, sex, age, time_adm, separated by /
  # leave one space before / for readability
  line2 <- paste(id_text, sex_text, age_text, admtime_text, sep = ' /')
  # line 3: los, dept, admtype
  line3 <- paste(los_text, dept_text, admtype_text, sep = ' /')
  
  # formatting
  text <- paste0(
    '<b>', line1, '</b><br>', 
    "<span style = 'font-size:10pt'>", line2, '<br>',
    line3, "</span>"
  )
  return(text)
}





# example_text <- "<b>Patient record</b><br><span style = 'font-size:10pt'>**ID**: 1 / **Sex**: M / **Age**: 63 / **Time admission**: 2254 <br>
#      **Length of stay**: 112H / **Department**: gastrosurgery / **Admission type**: emergency
#     </span>"



p + labs(title = text) + 
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




demop1 <- filter(demographics, ID == 1)
text_p1 <- make_demographic_info(demo_df = demop1)
pinter <- pinter + labs(title = text_p1) + 
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











