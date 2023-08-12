library(tidyverse)

setwd ('C:\\Users\\rachmaa1\\Documents\\Arif\\GT\\MGT 6203\\Project\\data')

# Read data only on row 11 (header) and 12
cpi_csv <- read_csv("M212881.csv",skip = 10) #, header = T, nrows = 1)

cpi <- cpi_csv [1,]
cpi <- select(cpi, -"Data Series")

# transpose the data
cpi <- t(cpi)
colnames(cpi)[1] ="CPI"
cpi <-as.data.frame(cpi)
cpi$CPI <- as.numeric (cpi$CPI)
cpi$month <- row.names(cpi)

# transform into date format
cpi <- cpi |> mutate(month = paste (month,' 01', sep = ''))
cpi$month <- as.Date (cpi$month,format = "%Y %b %d"  )

# rearrange column
cpi <- cpi[, c(2,1)]

# impute missing last data in May 2023
cpi_last = mean (c (cpi$CPI [cpi$month == '2023-04-01'], cpi$CPI [cpi$month == '2023-03-01']) )
cpi_last
cpi_last_data <- data.frame ("2023-05-01", cpi_last)
names(cpi_last_data) <- names(cpi)  
cpi_last_data
cpi <- rbind(cpi, cpi_last_data)
cpi <- cpi[order(cpi$month, decreasing = T),]


str (cpi)
summary (cpi)
head (cpi)
view (cpi)
glimpse (cpi)

write_csv(cpi, file='CPI.csv')
