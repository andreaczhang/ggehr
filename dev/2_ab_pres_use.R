# development code for ab prescription and use 

# library(readxl)
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggiraph)

# install.packages('ggiraph')


# fake data from ahus
# demographics <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "Demograpphics")
# catheters <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "Tube")
# abpres <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "AB_pres")
# abuse <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "ABU")
# location <- read_excel("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", sheet = "Location")

# call datasets directly from the package
# ggehr::demographics
demographics
ab_prescription
ab_use




# try patient 1, abuse 
# d1 <- dplyr::filter(ab_use, ID == 1)
# d1
# two drugs, different doses




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


# the drug_df should have standard names
ab_use <- rename(ab_use, label = ab_name)
ab_use <- rename(ab_use, method = ab_method)

# get the information
ablist <- ggehr::ab_ahus

d1 <- filter(ab_use, ID == 1)
d1

d1c <- left_join(d1, ablist, join_by(label == Norsk_AHUS))




# this is the step to make event

d1_full <- make_event_drug_use(d1c)
# d1_full <- make_event_drug_use(d1, tmin = 2254, tmax = 2254+112)

d1_full
tail(d1_full)
head(d1_full)
#filter(d1_full, event_label == 'Yes')


# plot ----
# basic tile plot
p <- ggplot(d1_full, aes(x = time, y = label)) + 
  geom_tile(aes(fill = event), color = 'white') 

p




# change color ----
d1_full
d1_full$time |> max()

# d1c <- left_join(d1_full, ablist_ahus, join_by(label == Norsk_AHUS))







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

plot_drug_use(data = d1_full)



breakpoints <- function(x, n=8){
  # this function is to be used in the ggplot to set breakpoints
  # x <- 2250:2500
  br <- pretty(x, n = n)
  br
}
# when there are lots of time points, the color becomes hard to distinguish
# natural extension: time t0 tmax plot against t admission and discharge


# ab prescription -----
# ab prescription time on the plot
# add a table 
?ab_prescription
ab_prescription
abp1 <- filter(ab_prescription, ID == 1)
abp1

p + geom_point(data = abp1[c(1,4),], aes(x = time, y = ab_name))
p + geom_point(data = abp1, aes(x = time, y = ab_name))

plot_drug_prescribe <- function(data_prescribe, plot_obj){
  
  # TO DO:
  # should also return data for checks
  # should allow disabling unused drugs 
  p <- plot_obj + geom_point(data = data_prescribe, 
                             aes(x = time, 
                                 y = ab_name))
  return(p)
}



# prescription table ----#
# dt <- data.frame(title = 'c', name = '2')

# dt <- abp1[, c(3, 6, 7)]
# dt
# gt <- gridExtra::tableGrob(dt)
# gridExtra::grid.arrange(p, gt)


# rather than table, might be more suitable for interactivitiy ontop of the dots

# tooltip: column of data to display
# data_id: 

p1 <- plot_drug_use(data_drug_use = d1_full)
plot_drug_prescribe(data_prescribe = abp1, plot_obj = p1)

plot_drug_prescribe_interactive(data_prescribe = abp1, plot_obj = p1)

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









# other static information ----

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
  title_text <- 'Patient record (Demo)'
  
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



# p + labs(title = text) + 
#   theme(
#     plot.title.position = "plot",
#     plot.title = element_textbox_simple(
#       size = 15,
#       lineheight = 1,
#       padding = margin(5.5, 5.5, 5.5, 5.5),
#       margin = margin(0, 0, 5.5, 0),
#       halign = 0.5,
#       r = grid::unit(3, 'pt'),
#       fill = "lightgrey"
#     ))




demop1 <- filter(demographics, ID == 1)
text_p1 <- make_demographic_info(demo_df = demop1)

p1 <- plot_drug_use(data_drug_use = d1_full)
p1p <- plot_drug_prescribe(data_prescribe = abp1, plot_obj = p1)


plot_info_demographic(info_text = text_p1, plot_obj = p1p)

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


p1p + labs(title = text_p1) + 
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







# ________ ----
# TESTS WITH DATA ----

ab_use <- ggehr::ab_use
ab_prescription <- ggehr::ab_prescription
demographics <- ggehr::demographics


# the drug_df should have standard names
ab_use <- rename(ab_use, label = ab_name)
ab_use <- rename(ab_use, method = ab_method)

# get the information on drugs
abinfo <- ggehr::ab_ahus


# extract drug use for patient id
id <- 1

dpatient_demog <- filter(demographics, ID == id)

dpatient_drug <- filter(ab_use, ID == id)
dpatient_drug

# add drug information 
dpatient_drug <- left_join(dpatient_drug, abinfo, 
                           join_by(label == Norsk_AHUS))


# this is the step to make event
# should use admin and discharge times
tadmin <- dpatient_demog$t0
tdischarge <- tadmin + dpatient_demog$los
d <- make_event_drug_use(dpatient_drug, 
                         tmin = tadmin, 
                         tmax = tdischarge)

# d1_full <- make_event_drug_use(d1, tmin = 2254, tmax = 2254+112)

dpatient_presc <- filter(ab_prescription, ID == id)

dpatient_demoinfo <- make_demographic_info(demo_df = dpatient_demog)




# plots

p1 <- plot_drug_use(data_drug_use = d)
p1
p1p <- plot_drug_prescribe(data_prescribe = dpatient_presc, 
                           plot_obj = p1)
p1p

p1pp <- plot_info_demographic(info_text = dpatient_demoinfo, 
                      plot_obj = p1)

# if want interactive: 
# generate the figure with info
# then make it into interactive
plot_drug_prescribe_interactive(data_prescribe = dpatient_presc, 
                                plot_obj = p1pp)

# might have to include the time limits regardless
# id = 4: drug use data 
# id = 5
# id = 6: many entries. some are hard to read - might be useful if enhance
# id = 7: merge stage has warning






