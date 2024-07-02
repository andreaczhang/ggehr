library(data.table)

# patient data for demonstration 

# demographics ----
demographics <- readxl::read_xlsx("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", 
                                  sheet = "Demograpphics")

setDT(demographics)
save(demographics, file = 'data/demographics.rda', compress = 'xz')



# ab prescription ----
ab_prescription <- readxl::read_xlsx("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", 
                            sheet = "AB_pres")
setDT(ab_prescription)

# minor modification for norwegian 
ab_prescription[purpose == 'Akutt buk/peritonitt', 
                purpose := 'Acute abdomen/peritonitis']


save(ab_prescription, file = 'data/ab_prescription.rda', compress = 'xz')




# ab use ----
ab_use <- readxl::read_xlsx("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", 
                           sheet = "ABU")

setDT(ab_use)
head(ab_use)
save(ab_use, file = 'data/ab_use.rda', compress = 'xz')




# catheters ----
catheters <- readxl::read_xlsx("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", 
                               sheet = "Tube")


setDT(catheters)
head(catheters)
setnames(catheters, 'tube', 'catheter')
save(catheters, file = 'data/catheters.rda', compress = 'xz')





# location ----
location <- readxl::read_xlsx("~/Documents/Data/ahus/ahus_ehr_sample.xlsx", 
                              sheet = "Location")

setDT(location)
head(location)
setnames(location, 'moderid', 'subpostid')
save(location, file = 'data/location.rda', compress = 'xz')





