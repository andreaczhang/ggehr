# define the color and text to display 

location_code <- c('Unknown',
                   'emergency_admin', 
                   'uro_post', 
                   'sop',
                   'postop',
                   'gastro_102',
                   'ortho_post',
                   'neuro',
                   'gyn_ortho',
                   'intensive')

location_display <- c('Unknown',
                      'ER',
                      'Urol',
                      'OR',
                      'Postop',
                      'GI-Sur',
                      'Orthop',
                      'Neur',
                      'Orthop-Gyn',
                      'ICU')

location_meta <- data.table(location_code, location_display)
location_meta

# add type
location_meta[, type := 'Bedward']
location_meta[location_code == 'Unknown', type := 'Unknown']
location_meta[location_code == 'emergency_admin', type := 'ER']
location_meta[location_code == 'sop', type := 'OR']
location_meta[location_code ==  'postop', type := 'POSTOP']
location_meta[location_code ==  'intensive', type := 'ICU']
location_meta

save(location_meta, file = 'data/location_meta.rda', compress = 'xz')



# location_meta[, color := '#b3e2ea']
# location_meta



