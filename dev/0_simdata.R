# simulate some realistic data 

# abs
# mvp 
# adm

d_adm <- data.frame(
  id = c(1, 2, 3), 
  sex = c('M', 'F', 'M'), 
  dept = c('gastrosurgical', 'orthopedics', 'gastrosurgical'),
  t0 = c(-1, 20, 51)
)

d_adm

# abs (or any kind of prescription skjema)


ab_names <- c('metronidazol', 'ampicillin', 'cefalotin', 'amoksicillin', 'vancomycin')



# simulate 3 meta tables
set.seed(1)
meta_list <- list()

for(i in 1:nrow(d_adm)){
  meta_list[[i]] <- sim_meta(eventnames = ab_names,
                             size = sample(1:5, size = 1, replace = F))
}


# then simulate individual person's records
personal_list <- list()

for(i in 1:nrow(d_adm)){
  personal_list[[i]] <- sim_records(id = d_adm$id[i], 
                                    t0 = d_adm$t0[i], 
                                    meta = meta_list[[i]])
}

personal_list


saveRDS(personal_list, file = 'dev/three_patient.rda')













# util ----

# example
# sim_prescribe(prescription = 'metronidazol', n = 2, td = 2)


sim_prescribe <- function(prescription, n, td){
  # name <- 'vancomycin'
  # n <- 1
  # td <- 3
  
  # if only one record, t_prescribe = t0+td
  # multiple records, simulate time differences
  if(n == 1){
    tvec <- td
  }else if(n >1){
    # simulate random number with mean = td
    tds <- round(rexp(n, rate = 1/td), digits = 0)
    tvec <- cumsum(tds)
  }
  
  rec <- data.frame(
    time_diff = tvec,
    event = prescription
  )
  return(rec)
}

# example
# meta <- sim_meta(eventnames = ab_names, size = 3)

sim_meta <- function(eventnames, size){
  
  # eventnames <- ab_names
  # size <- 3
  which_event <- sample(1:length(eventnames), 
                        size = size, 
                        replace = F)
  
  events <- eventnames[which_event]
  # how many events for each type
  freq <- sample(1:10, 
                 size = size,
                 replace = T)
  
  # time difference (mean)
  td_mean <- sample(1:10, 
                    size = size, 
                    replace = T)
  
  meta <- data.frame(events = events, 
                     freq = freq, 
                     td_mean = td_mean)
  
  return(meta)
}

# example
# sim_records(id = 2, t0 = 2, meta = meta_1)

sim_records <- function(id, t0, meta){
  
  # id <- 1
  # t0 <- -1
  # meta <- sim_meta(eventnames = ab_names, size = 3)
  
  rec <- list()
  for(i in 1:nrow(meta)){
    rec[[i]] <- sim_prescribe(
      prescription = meta$events[i], 
      n = meta$freq[i],
      td = meta$td_mean[i])
  }
  recdf <- do.call(rbind, rec)
  # attach id and t0
  record <- cbind(id = id, 
                  t0 = t0,
                  recdf)
  record$time <- record$t0 + record$time_diff
  return(record)
  
}



