# test function with data
# NOT UNIT TESTS


# load and process data ----
ab_use <- ggehr::ab_use
ab_prescription <- ggehr::ab_prescription
demographics <- ggehr::demographics
loc <- ggehr::location


# the drug_df should have standard names
ab_use <- rename(ab_use, label = ab_name)
ab_use <- rename(ab_use, method = ab_method)

# get the information on drugs
abinfo <- ggehr::ab_ahus


# extract drug use for patient id
id <- 10

dpatient_demog <- filter(demographics, ID == id)
dpatient_drug <- filter(ab_use, ID == id)
dpatient_presc <- filter(ab_prescription, ID == id)
dpatient_loc <- filter(loc, ID == id)


# dpatient_drug

# add drug information 
dpatient_drug <- left_join(dpatient_drug, abinfo, 
                           join_by(label == Norsk_AHUS))

# this is the step to make event
# should use admin and discharge times
tadmin <- dpatient_demog$t0
tdischarge <- tadmin + dpatient_demog$los



# drug use ----#
d <- make_event_drug_use(dpatient_drug, 
                         tmin = tadmin, 
                         tmax = tdischarge)


# demographics ----# 
dpatient_demoinfo <- make_demographic_info(demo_df = dpatient_demog)




# location ----#
dloc <- make_location(data_location = dpatient_loc, 
                      tadmin = dpatient_demog$t0, 
                      los = dpatient_demog$los)





# plot ----

# drug use 
p1 <- plot_drug_use(data_drug_use = d)
p1

# drug prescription
p1p <- plot_drug_prescribe(data_prescribe = dpatient_presc, 
                           plot_obj = p1)
p1p

p1pp <- plot_info_demographic(info_text = dpatient_demoinfo, 
                              plot_obj = p1)
p1pp 


# location
p2 <- plot_location(loc_obj = dloc, keep_time = F)
p2
# if want to keep the legend 
# p2 <- plot_location(loc_obj = dloc, keep_legend = T)



# put together ----

library(patchwork)
p1 + p2 + plot_layout(ncol = 1, heights = 2:1)
p1p + p2 + plot_layout(ncol = 1, heights = 2:1)

p1pp + p2 + plot_layout(ncol = 1, heights = 2:1)





# might have to include the time limits regardless
# id = 4: drug use data 
# id = 5
# id = 6: many entries. some are hard to read - might be useful if enhance
# id = 7: merge stage has warning





# interactive  -----
# if want interactive: 
# generate the figure with info
# then make it into interactive
# interactive one does not work with patchwork 
p1i <- plot_drug_prescribe_interactive(data_prescribe = dpatient_presc, 
                                       plot_obj = p1pp)




