library(tidyverse)
library(arrow)
library(psych)

setwd ('C:\\Users\\rachmaa1\\Documents\\Arif\\GT\\MGT 6203\\Project\\data\\resale price')


####################################################
# 1. Load HDB Price year 2015 - 2023               #
####################################################
fourth_file <- read_csv("resale-flat-prices-based-on-registration-date-from-jan-2015-to-dec-2016.csv")

# fix remaining lease on fifth file
fifth_file <- read_csv("resale-flat-prices-based-on-registration-date-from-jan-2017-onwards.csv") 
fifth_file$lease_year  = as.numeric (substr (fifth_file$remaining_lease,1,2)) 
fifth_file$lease_year
fifth_file$lease_month = as.numeric (substr (fifth_file$remaining_lease,10,11)) 
fifth_file$lease_month <- replace(fifth_file$lease_month, is.na(fifth_file$lease_month), 0)
fifth_file$lease_month
fifth_file$remaining_lease2 = as.numeric (fifth_file$lease_year + fifth_file$lease_month/12)
fifth_file[1:300, c('remaining_lease' ,'lease_year','lease_month','remaining_lease2')]
# Drop temp columns 
fifth_file <- select(fifth_file,-c(lease_year,lease_month,remaining_lease))
fifth_file <- fifth_file %>% rename("remaining_lease" = "remaining_lease2")
summary (fifth_file)
str (fifth_file)

file <- rbind( fourth_file,  fifth_file)
summary (file)
str (file)


# Enrich month and lease commence date to become a full date
file <- file |> mutate(month_temp = paste (month,'-01', sep = '')) |>
  mutate(lease_commence_date_temp = paste (lease_commence_date,'-01-01', sep = ''))

# Convert month from char to date
file$month2 <- as.Date (file$month_temp, format = "%Y-%m-%d")
head (file$month2,10)
#check result
file[6660:6670, c('month' ,'month2')]

# Convert lease commence date from char to date
file$lease_commence_date2 <- as.Date (file$lease_commence_date_temp, format = "%Y-%m-%d")
#check result
file[60:70, c('lease_commence_date' ,'lease_commence_date2')]

# Drop temp columns 
file <- select(file,-c(month,lease_commence_date,month_temp,lease_commence_date_temp))
file <- file %>% rename("date" = "month2", 
                        "lease_commence_date" = "lease_commence_date2",
                        "price" = "resale_price")
file$price_sqm = file$price / file$floor_area_sqm

col_order <- c("price", "price_sqm", "date", "town", "flat_type", "flat_model", "floor_area_sqm", "block", 
               "street_name", "storey_range", "lease_commence_date", "remaining_lease")
file <- file[, col_order]



unique (file$flat_type)

file$flat_type [file$flat_type == 'MULTI GENERATION'] <- 'MULTI-GENERATION'  
file$flat_model <- toupper(file$flat_model)
file$flat_type <- as.factor (file$flat_type)
file$flat_model <- as.factor (file$flat_model)


summary (file)
str (file)
hdb <- file
# write_csv(file, 'HDB Resale Price 2015-2023.csv')
# # write_rds(file, "HDB Resale Price 1990-2023.rds")
# write_parquet(file, "HDB Resale Price 2015-2023.parquet")


####################################################
# 2. Load CPI data                                 #
####################################################

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
# view (cpi)
glimpse (cpi)

# write_csv(cpi, file='CPI.csv')

####################################################
# 3. Adjust Inflation of HDB price with CPI        #
####################################################

hdb_cpi <- merge(hdb,cpi,by.x = 'date', by.y ='month', all.x= TRUE)

hdb_cpi$price_adj <- ( hdb_cpi$price * 100 ) / hdb_cpi$CPI 
hdb_cpi$price_adj_sqm <- ( hdb_cpi$price_sqm * 100 ) / hdb_cpi$CPI

col_order <- c("date",  "price", "price_sqm", "price_adj", "price_adj_sqm", "town", "flat_type", "flat_model", "floor_area_sqm", "block", 
               "street_name", "storey_range", "lease_commence_date", "remaining_lease", "CPI")
hdb_cpi <- hdb_cpi[, col_order]
hdb_cpi$town <- as.factor(hdb_cpi$town)

summary (hdb_cpi)
str (hdb_cpi)

####################################################
# 4. Load Population data                          #
####################################################

# Read data only on row 11 (header) and 12
population_csv <- read_csv("M810011-table.csv",skip = 10) #, header = T, nrows = 1)

population <- rbind (population_csv [2:19,],population_csv [25,] )

population <- population [,1:9]
# view (population)

str (population)


# transpose the data
population2 <- as.data.frame (t(population) )
colnames (population2) <- population2[1,]
population2 <- population2 [2:nrow (population2),]

population2 <- cbind(year = rownames(population2), population2)
rownames(population2) <- 1:nrow(population2)


population2[, "0 - 4 Years"] = as.numeric(population2[, "0 - 4 Years"] )
# population2$year <- as.Date (population2$year, format = '%Y')
# view (population2)

# cast all field numeric
columns <- colnames (population2)
# columns <- columns [2:length (columns)]  #Skip year?
x <- 0
for (x in 1:length (columns)) { 
  population2[, columns [x]] = as.numeric(population2[, columns [x]] )
}

population2$children = population2$`0 - 4 Years` + population2$`5 - 9 Years`+
  population2$`10 - 14 Years` + population2$`15 - 19 Years` 
population2$adult = population2$`20 - 24 Years` + population2$`25 - 29 Years`+
  population2$`30 - 34 Years` + population2$`35 - 39 Years` + 
  population2$`40 - 44 Years` + population2$`45 - 49 Years` + 
  population2$`50 - 54 Years` + population2$`55 - 59 Years`

population2$senior_citizen = population2$`60 - 64 Years` + 
  population2$`65 - 69 Years` + population2$`70 - 74 Years` +
  population2$`75 - 79 Years` + population2$`80 - 84 Years` + 
  population2$`85 - 89 Years` + population2$`90 Years & Over`

str (population2)
summary (population2)

####################################################
# 5. Merge Population data with HDB data           #
####################################################
hdb_cpi$year_hdb = year (hdb_cpi$date)

str (hdb_cpi)

final_hdb <- merge(hdb_cpi,population2,by.x = 'year_hdb', by.y ='year', all.x= TRUE)
final_hdb <- select(final_hdb, -c(year_hdb)) 
str (final_hdb)


####################################################
# 6. Load HDB Price Index data                     #
####################################################

hdb_pi_csv <- read_csv("housing-and-development-board-resale-price-index-1q2009-100-quarterly.csv")

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

####################################################
# 7. Adjust Inflation using HDB Price Index data   #
####################################################
final_hdb$storey_range <- as.factor(final_hdb$storey_range)
final_hdb$total_population <- final_hdb$children + final_hdb$adult + final_hdb$senior_citizen

hdb_price_index <- merge(final_hdb,hdb_pi,by.x = 'date', by.y ='date', all.x= TRUE)
hdb_price_index$price_adj2     <- ( hdb_price_index$price * 100 ) / hdb_price_index$HDB_price_index 
hdb_price_index$price_adj_sqm2 <- ( hdb_price_index$price_sqm * 100 ) / hdb_price_index$HDB_price_index 

summary (hdb_price_index)
str (hdb_price_index)


####################################################
# 8. Write to file                     #
####################################################

write_parquet(hdb_price_index, "Final HDB price2.parquet")
write_csv(hdb_price_index, "Final HDB price2.csv")

