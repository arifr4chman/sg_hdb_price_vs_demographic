library(tidyverse)
library(arrow)
library(psych)

setwd ('C:\\Users\\rachmaa1\\Documents\\Arif\\GT\\MGT 6203\\Project\\data')

# This file contains the 1st version, without HDB Price Index
hdb <- read_parquet("Final HDB price.parquet")
summary (hdb)
str (hdb)
describe (hdb)
# describeBy(hdb, hdb$flat_type)

# This file contains the 2nd version, with  HDB Price Index
hdb2 <- read_parquet("Final HDB price2.parquet")
summary (hdb2)
str (hdb2)
describe (hdb2)
# describeBy(hdb, hdb$flat_type)