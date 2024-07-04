#' Make drug use events
#' 
#' @description
#' Collect results from the new diet discovery, along with the current diet.
#' 
#' @param drug_df drug dataframe
#' @param tmin tmin
#' @param tmax tmax
#' @import dplyr
#' @return A datafrme with drug uses
#' @export


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




#' Make demographic info
#' 
#' @description
#' Collect results from the new diet discovery, along with the current diet.
#' 
#' @param demo_df drug dataframe
#' @param keep_id tmin
#' @param keep_sex tmax
#' @param keep_age description
#' @param keep_dept t
#' @param keep_admtype t
#' @param keep_t0 t
#' @param keep_los t
#' @return A datafrme with drug uses
#' @export

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



#' Make location
#' 
#' @description
#' Collect results from the new diet discovery, along with the current diet.
#' 
#' @param data_location drug dataframe
#' @param tadmin tmin
#' @param los tmax
#' @importFrom dplyr left_join
#' @return A datafrme with drug uses
#' @export

make_location <- function(data_location, 
                          tadmin, 
                          los){
  
  # data_location <- loc1
  # sort based on time (works for data.table)
  data_location <- data.table::setDT(data_location)
  data_location <- data_location[order(time)]
  dl <- data_location[, .(time, post)]
  data.table::setnames(dl, old = 'post', new = 'location_code')
  
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



#' Expand location
#' 
#' @description
#' Collect results from the new diet discovery, along with the current diet.
#' 
#' @param x data
#' @return A dataframe with drug uses
#' @export

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


