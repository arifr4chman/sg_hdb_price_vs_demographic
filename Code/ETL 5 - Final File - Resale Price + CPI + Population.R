library(tidyverse)
library(arrow)
library(psych)

setwd ('C:\\Users\\rachmaa1\\Documents\\Arif\\GT\\MGT 6203\\Project\\data')

hdb_cpi <- read_parquet("HDB Resale Price 2015-2023 + CPI.parquet")
population <- read_csv ('Population.csv')

hdb_cpi$year_hdb = year (hdb_cpi$date)

str (hdb_cpi)

final_hdb <- merge(hdb_cpi,population,by.x = 'year_hdb', by.y ='year', all.x= TRUE)
final_hdb <- select(final_hdb, -c(year_hdb)) 
str (final_hdb)

write_parquet(final_hdb, "Final HDB price.parquet")
write_csv(final_hdb, "Final HDB price.csv")
