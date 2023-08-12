library(tidyverse)
library(arrow)
library(psych)
library(car)
library(ggplot2)
library(MASS)
library(corrplot)
library(dplyr)

setwd ('C:\\Users\\rachmaa1\\Documents\\Arif\\GT\\MGT 6203\\Project\\data')
hdb <- read_parquet("Final HDB price2.parquet")
str (hdb)

#############################################
# Check Price data
#############################################
# Histogram -> skewed to the right
ggplot(data=hdb, aes(price_adj_sqm2)) + 
  geom_histogram(breaks=seq(1000, 15000, by =250), 
                 col="black", 
                 fill= "green", 
                 alpha = .2) + 
  labs(title="Histogram for Price SQM adj by HDB Index") +
  labs(x="price", y="Count")

ggplot(data=hdb, aes(price_adj_sqm)) + 
  geom_histogram(breaks=seq(1000, 15000, by =250), 
                 col="red" , 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Original Price") +
  labs(x="Price SQM Original", y="Count")

ggplot(data=hdb, aes(price_adj2)) + 
  geom_histogram(breaks=seq(1000, 1500000, by =25000), 
                 col="black", 
                 fill="blue", 
                 alpha = .2) + 
  labs(title="Histogram for Price SQM adj by HDB Index") +
  labs(x="Price adj. by HDB index", y="Count")

ggplot(data=hdb, aes(price)) + 
  geom_histogram(breaks=seq(1000, 1500000, by =25000), 
                 col="red", 
                 fill="blue", 
                 alpha = .2) + 
  labs(title="Histogram for Price adj by HDB Index") +
  labs(x="Price Original", y="Count")

# Run Shapiro test to check if Price is distributed normally
# data is sampled for 5000 rows, that's limitation from Shapiro
shapiro.test (sample_n (hdb,5000)$price_adj_sqm2) #p-value < 2.2e-16
# Since p-value is < 0.05, Shapiro test concludes the distribution is not normal.

qqnorm (hdb$price_adj_sqm2)
qqline(hdb$price_adj_sqm2, probs = c(0.25, 0.75))

boxcox(lm(hdb$price_adj_sqm2 ~ 1))
# https://www.r-bloggers.com/2022/10/box-cox-transformation-in-r/
# https://www.statology.org/box-cox-transformation-in-r/

#############################################
# Check Correlation
#############################################

hdb_numeric <-  dplyr::select (hdb, -c(block, street_name, date, 
                                       lease_commence_date, town, flat_type, 
                                       flat_model, storey_range, total_population) )
corrplot(hdb_numeric, is.corr = FALSE, method = "square")
str (hdb_numeric)

ggpairs(hdb_numeric, 
        upper = list(continuous = wrap("cor", size = 9))) 
