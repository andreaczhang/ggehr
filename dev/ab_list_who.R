library(readxl)
library(data.table)

ablist_who <- read_excel("inst/ab_list_who.xlsx")
ablist_who <- data.table(ab_list_who)
head(ablist_who)
colnames(ablist_who)

# code colors
# access: green
# watch: yellow
# reserve: red


ablist_who[Category == 'Access', color_code := 'green']
ablist_who[Category == 'Watch', color_code := 'yellow']
ablist_who[Category == 'Reserve', color_code := 'red']




# ahus specific ----
# keep the ahus ones 
ablist_ahus <- ablist_who[!is.na(Norsk_AHUS), .(Antibiotic, Category, Norsk_AHUS, color_code)]

ablist_ahus$Category |> table()
# 21 access, 2 reserve, 16 watch


ablist_ahus[Category == 'Reserve']
# Aztreonam, Linezolid (very rare)
ablist_ahus[Category == 'Watch']



# these two datasets need to be saved



