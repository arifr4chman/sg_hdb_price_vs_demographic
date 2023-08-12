library(tidyverse)

setwd ('C:\\Users\\rachmaa1\\Documents\\Arif\\GT\\MGT 6203\\Project\\data\\resale price')


####################
# year 2015 - 2023 #
####################
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

write_csv(file, 'HDB Resale Price 2015-2023.csv')
# write_rds(file, "HDB Resale Price 1990-2023.rds")

library(arrow)
write_parquet(file, "HDB Resale Price 2015-2023.parquet")
