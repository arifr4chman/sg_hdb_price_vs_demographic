library(tidyverse)
library(arrow)
library(psych)

setwd ('C:\\Users\\rachmaa1\\Documents\\Arif\\GT\\MGT 6203\\Project\\data')
hdb <- read_parquet("HDB Resale Price 2015-2023.parquet")
cpi <- read_csv ("cpi.csv")


summary (hdb)
str (hdb)
describe (hdb)
describeBy(hdb, hdb$flat_type)

summary (cpi)
str (cpi)
describe (cpi)

hdb_cpi <- merge(hdb,cpi,by.x = 'date', by.y ='month', all.x= TRUE)

hdb_cpi$price_adj <- ( hdb_cpi$price * 100 ) / hdb_cpi$CPI 
hdb_cpi$price_adj_sqm <- ( hdb_cpi$price_sqm * 100 ) / hdb_cpi$CPI

col_order <- c("date",  "price", "price_sqm", "price_adj", "price_adj_sqm", "town", "flat_type", "flat_model", "floor_area_sqm", "block", 
               "street_name", "storey_range", "lease_commence_date", "remaining_lease", "CPI")
hdb_cpi <- hdb_cpi[, col_order]
hdb_cpi$town <- as.factor(hdb_cpi$town)

summary (hdb_cpi)
str (hdb_cpi)

write_parquet(hdb_cpi, "HDB Resale Price 2015-2023 + CPI.parquet")
write_csv(hdb_cpi, "HDB Resale Price 2015-2023 + CPI.csv")
