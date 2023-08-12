library(tidyverse)
library(stringr)

setwd ('C:\\Users\\rachmaa1\\Documents\\Arif\\GT\\MGT 6203\\Project\\data')

hdb_pi_csv <- read_csv("housing-and-development-board-resale-price-index-1q2009-100-quarterly.csv")

# Init data frame
columns = c("date","HDB_price_index") 
hdb_pi = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(hdb_pi) = columns

# Convert quarterly based data into monthly based data
for(i in 1:nrow(hdb_pi_csv)) { 
  x <- i <- hdb_pi_csv [i,]
  
  year <- str_sub(x$quarter,1,4)
  quarter <- str_sub(x$quarter,-2,-1)
  
  if (quarter == 'Q1') { c_month <- c('01','02','03') 
  } else if (quarter == 'Q2') {c_month <- c('04','05','06')
  } else if (quarter == 'Q3') {c_month <- c('07','08','09')
  } else if (quarter == 'Q4') {c_month <- c('10','11','12') }
  
  for (j in 1:length (c_month)) { 
    date <- paste(year, c_month[j], '01', sep = "-")
    # hdb_pi_row <- c(date,x$index) 
    hdb_pi[nrow(hdb_pi) + 1,] <- c(date,x$index)
    
  }
}

hdb_pi$date <- as.Date (hdb_pi$date)
hdb_pi$HDB_price_index <- as.numeric (hdb_pi$HDB_price_index)
str (hdb_pi)
summary (hdb_pi)
head (hdb_pi)
glimpse (hdb_pi)

write_csv(hdb_pi, file='HDB_Price_Index.csv')
