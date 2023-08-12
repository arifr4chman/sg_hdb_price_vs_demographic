library(tidyverse)

setwd ('C:\\Users\\rachmaa1\\Documents\\Arif\\GT\\MGT 6203\\Project\\data\\resale price')

####################
# year 1990 - 2014 #
####################

first_file <- read_csv("resale-flat-prices-based-on-approval-date-1990-1999.csv")
second_file <- read_csv("resale-flat-prices-based-on-approval-date-2000-feb-2012.csv")
third_file <- read_csv("resale-flat-prices-based-on-registration-date-from-mar-2012-to-dec-2014.csv")

#view (first_file)
#summary (first_file)
#str (first_file)

file <- rbind( first_file,  second_file, third_file)
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

# Calculate remaining lease
file$remaining_lease = as.numeric(difftime(file$month2,file$lease_commence_date2, units = "weeks") / 52.25) 

# Check result
file[1:20, c('remaining_lease' ,'month2', 'lease_commence_date2')]


# Drop temp columns 
file <- select(file,-c(month,lease_commence_date,month_temp,lease_commence_date_temp))
file <- file %>% rename("month" = "month2", 
                        "lease_commence_date" = "lease_commence_date2")
summary (file)
str (file)
head (file,10)


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

file2 <- rbind( fourth_file,  fifth_file)
summary (file2)
str (file2)


# Enrich month and lease commence date to become a full date
file2 <- file2 |> mutate(month_temp = paste (month,'-01', sep = '')) |>
  mutate(lease_commence_date_temp = paste (lease_commence_date,'-01-01', sep = ''))

# Convert month from char to date
file2$month2 <- as.Date (file2$month_temp, format = "%Y-%m-%d")
head (file2$month2,10)
#check result
file2[6660:6670, c('month' ,'month2')]

# Convert lease commence date from char to date
file2$lease_commence_date2 <- as.Date (file2$lease_commence_date_temp, format = "%Y-%m-%d")
#check result
file2[60:70, c('lease_commence_date' ,'lease_commence_date2')]

# Drop temp columns 
file2 <- select(file2,-c(month,lease_commence_date,month_temp,lease_commence_date_temp))
file2 <- file2 %>% rename("month" = "month2", 
                          "lease_commence_date" = "lease_commence_date2")


summary (file)
summary (file2)
final_file <- rbind( file, file2)

final_file$flat_type [final_file$flat_type == 'MULTI GENERATION'] <- 'MULTI-GENERATION'  
final_file$flat_model <- toupper(final_file$flat_model)
final_file$flat_type <- as.factor (final_file$flat_type)
final_file$flat_model <- as.factor (final_file$flat_model)


summary (final_file)
str (final_file)

write_csv(final_file, 'HDB Resale Price 1990-2023.csv')
write_rds(final_file, "HDB Resale Price 1990-2023.rds")

library(arrow)
write_parquet(final_file, "HDB Resale Price 1990-2023.parquet")
