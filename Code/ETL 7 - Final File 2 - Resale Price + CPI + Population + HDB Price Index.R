library(tidyverse)
library(arrow)
library(psych)

setwd ('C:\\Users\\rachmaa1\\Documents\\Arif\\GT\\MGT 6203\\Project\\data')

hdb <- read_parquet("Final HDB price.parquet")
hdb$storey_range <- as.factor(hdb$storey_range)
hdb$total_population <- hdb$children + hdb$adult + hdb$senior_citizen

hdb_pi <- read_csv ('HDB_Price_Index.csv')

hdb_price_index <- merge(hdb,hdb_pi,by.x = 'date', by.y ='date', all.x= TRUE)
hdb_price_index$price_adj2     <- ( hdb_price_index$price * 100 ) / hdb_price_index$HDB_price_index 
hdb_price_index$price_adj_sqm2 <- ( hdb_price_index$price_sqm * 100 ) / hdb_price_index$HDB_price_index 

summary (hdb_price_index)
str (hdb_price_index)

write_parquet(hdb_price_index, "Final HDB price2.parquet")
write_csv(hdb_price_index, "Final HDB price2.csv")
