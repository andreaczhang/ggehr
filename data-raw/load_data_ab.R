library(readxl)
library(data.table)


# antibiotics who ----
# this list also has data on the AHUS
# list.files(system.file('extdata', package = 'ggehr'))
# 
# fs::path_package('ggehr')
# system.file("extdata", "ab_list_who.xlsx", 
#             package = "ggehr")
# dir(system.file("extdata", "ab_list_who.xlsx", 
#                 package = "ggehr"))
# getwd()
ablist <- readxl::read_xlsx('./inst/extdata/ab_list_who.xlsx')

# ablist <- readxl::read_xlsx(system.file("extdata", "ab_list_who.xlsx", 
#                                             package = "ggehr"))
ablist <- setDT(ablist)
head(ablist)
colnames(ablist)

# code colors
# access: green
# watch: yellow
# reserve: red

ablist[Category == 'Access', color_code := 'green']
ablist[Category == 'Watch', color_code := 'yellow']
ablist[Category == 'Reserve', color_code := 'red']

head(ablist)

# for who, drop the one for ahus
ab_who <- copy(ablist)
ab_who[, Norsk_AHUS := NULL]

# save
save(ab_who, file = 'data/ab_who.rda', compress = 'xz')



# ahus specific ----
# keep the ahus ones 
ab_ahus <- ablist[!is.na(Norsk_AHUS), .(Antibiotic, Category, Norsk_AHUS, color_code)]

ab_ahus$Category |> table()
# 21 access, 2 reserve, 16 watch


ab_ahus[Category == 'Reserve']
# Aztreonam, Linezolid (very rare)
ab_ahus[Category == 'Watch']


# save
save(ab_ahus, file = 'data/ab_ahus.rda', compress = 'xz')



