library(readxl)
library(data.table)

ab_list_who <- read_excel("inst/ab_list_who.xlsx")
ab_list_who <- data.table(ab_list_who)
head(ab_list_who)
colnames(ab_list_who)
ab <- ab_list_who[!is.na(Norsk_AHUS), .(Antibiotic, Category, Norsk_AHUS)]

ab$Category |> table()
# 21 access, 2 reserve, 16 watch


ab[Category == 'Reserve']
# Aztreonam, Linezolid (very rare)
ab[Category == 'Watch']


