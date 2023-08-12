library(tidyverse)
library(arrow)
library(psych)
library(car)

setwd ('C:\\Users\\rachmaa1\\Documents\\Arif\\GT\\MGT 6203\\Project\\data')
hdb <- read_parquet("Final HDB price2.parquet")


summary (hdb)
str (hdb)
contrasts (hdb$town)
contrasts (hdb$storey_range)
unique (hdb$flat_type)
unique (hdb$town)
unique (hdb$storey_range)


describe (hdb)
# describeBy(hdb, hdb$flat_type)

# Model with Price Adj -> 5 high VIF variables
lm_price <- lm (price_adj2 ~ town + floor_area_sqm + storey_range + flat_type +  remaining_lease + children + adult + senior_citizen, data = hdb )
summary (lm_price)     # R2 = 0.8445, Adj R2 = 0.8445
# children -3.152e-01, adult 8.949e-02, senior_citizen -1.737e-02
plot (lm_price)
vif(lm_price)          # 5 variables with high VIF

# Model with Price Adj -> 2 high VIF variables
lm_price2_scale <- lm (price_adj2 ~ town + scale (floor_area_sqm, scale = TRUE) + storey_range + flat_type + 
                         scale (remaining_lease, scale = TRUE) + 
                         scale (total_population, scale = TRUE), 
                         # scale (children, scale = TRUE) + scale (adult, scale = TRUE) + scale (senior_citizen, scale = TRUE),
                       data = hdb )
summary (lm_price2_scale)     # R2 = 0.8443, Adj R2 = 0.8442  
# children                  -6579.3, adult                    2437.4, senior_citizen*           -1488.6
# total_population 2750.8      
plot (lm_price2_scale)
vif(lm_price2_scale) #2 high VIF variables : Floor area sqm and flat type


# Model with Price Adj minus floor_area_sqm -> No high VIF variables, NOT normally distributed, too many Influent points
lm_price2_scale <- lm (price_adj2 ~ town + storey_range + flat_type + 
                         scale (remaining_lease, scale = TRUE) + 
                         scale (total_population, scale = TRUE), 
                       # scale (children, scale = TRUE) + scale (adult, scale = TRUE) + scale (senior_citizen, scale = TRUE),
                       data = hdb )
summary (lm_price2_scale)     # R2 = 0.8171, Adj R2 = 0.8171  
# total_population 2289.7            
plot (lm_price2_scale)
vif(lm_price2_scale) #0 high VIF variables 

cooksD <- cooks.distance(lm_price2_scale)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential
length (influential) # 9058


# #########################################################

# Model with Price Adj minus floor_area_sqm -> No high VIF variables, NOT normally distributed, too many Influential points
lm_price_sqm <- lm (price_adj_sqm2 ~ town + storey_range + flat_type + 
                         scale (remaining_lease, scale = TRUE) + 
                         scale (total_population, scale = TRUE), 
                       # scale (children, scale = TRUE) + scale (adult, scale = TRUE) + scale (senior_citizen, scale = TRUE),
                       data = hdb )
summary (lm_price_sqm)     # R2 = 0.7675, Adj R2 = 0.7674   
# total_population 24.381 (out of thousands)            
plot (lm_price_sqm)
vif(lm_price_sqm) #0 high VIF variables 

cooksD <- cooks.distance(lm_price_sqm)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential
length (influential) #10443

lm_price_sqm_loglin <- lm (log (price_adj_sqm2) ~ town + storey_range + flat_type + 
                      scale (remaining_lease, scale = TRUE) + 
                      scale (total_population, scale = TRUE), 
                    # scale (children, scale = TRUE) + scale (adult, scale = TRUE) + scale (senior_citizen, scale = TRUE),
                    data = hdb )
summary (lm_price_sqm_loglin)     # R2 = 0.7554, Adj R2 = 0.7554   
plot (lm_price_sqm_loglin)
vif(lm_price_sqm_loglin) #0 high VIF variables 

cooksD <- cooks.distance(lm_price_sqm_loglin)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential
length (influential) #12231

town_amk <- filter (hdb, town == 'ANG MO KIO')
ggplot(town_amk, aes(x = town, y = log (price_adj_sqm2))) +
  geom_boxplot()

town_amk_4room <- filter (town_amk, flat_type == '4 ROOM')
ggplot(town_amk_4room, aes(x = storey_range, y = log (price_adj_sqm2))) +
  geom_boxplot()
str (town_amk_4room)

town_amk_4room_4to6storey <- filter (town_amk_4room, storey_range == '04 TO 06')
str (town_amk_4room_4to6storey)
summary (town_amk_4room_4to6storey)
ggplot(town_amk_4room_4to6storey, aes(x = flat_model, y = log (price_adj_sqm2))) +
  geom_boxplot()

lm_amk <- lm ( (price_adj_sqm2) ~ flat_model + 
                             scale (remaining_lease, scale = TRUE) + 
                             scale (total_population, scale = TRUE),
                           # scale (children, scale = TRUE) +
                           # scale (adult, scale = TRUE),
                           # scale (senior_citizen, scale = TRUE),
                           data = town_amk_4room_4to6storey )
summary (lm_amk)
vif (lm_amk)

ggplot(town_amk_4room_4to6storey, aes(x = flat_model, y = remaining_lease)) +
  geom_boxplot()
ggplot(town_amk_4room, aes(x = flat_model, y = remaining_lease)) +
  geom_boxplot()
ggplot(town_amk, aes(x = flat_model, y = remaining_lease)) +
  geom_boxplot()


unique (town_amk_4room$flat_type)
filter (town_amk_4room, flat_type != '4 ROOM')

# #########################################################


lm_price2_vif <- lm (price_adj2 ~ town + floor_area_sqm + storey_range + remaining_lease + total_population, data = hdb2 )
summary (lm_price2_vif)     # R2 = 0.8405, Adj R2 = 0.8405 
# Total population 5.416e-02
par(mfrow = c(2, 2))
plot (lm_price2_vif)
vif(lm_price2_vif)

# Cook's distance
cooksD <- cooks.distance(lm_price2_vif)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

# ##################################################################

lm_price_sqm <- lm (price_adj_sqm ~ town + flat_type + floor_area_sqm + remaining_lease + storey_range + children + adult + senior_citizen, data = hdb )
summary (lm_price_sqm) # R2 = 0.7762,	Adj R2 =  0.7762 


lm_simple <- lm (price_adj ~ flat_type + remaining_lease + children + adult + senior_citizen, data = hdb )
summary (lm_simple)    #  R2 = 0.4761,	Adj R2 =  0.4761


hdb_4room <- filter(hdb,flat_type == '4 ROOM')
lm_4room <- lm (price_adj ~ town + floor_area_sqm  + storey_range + remaining_lease + children + adult + senior_citizen, data = hdb_4room)
summary (lm_4room)     # R2 = 0.8193,	Adj R2 = 0.8192 
# children              4.695e+00, adult                -1.820e+00, senior_citizen        9.081e-01
lm_4room_sqm <- lm (price_adj_sqm ~ town + floor_area_sqm  + storey_range + remaining_lease + children + adult + senior_citizen, data = hdb_4room)
summary (lm_4room_sqm)     # R2 = 0.8365,	Adj R2 = 0.8364  
# children              4.960e-02, adult                -1.928e-02, senior_citizen        9.580e-03


hdb_5room <- filter(hdb,flat_type == '5 ROOM')
lm_5room <- lm (price_adj ~ town + floor_area_sqm  + storey_range + remaining_lease + children + adult + senior_citizen, data = hdb_5room)
summary (lm_5room)         # R2 = 0.8162,	Adj R2 = 0.816 
# children              4.855e+00, adult                -1.943e+00, senior_citizen        1.039e+00

hdb_3room <- filter(hdb,flat_type == '3 ROOM')
lm_3room <- lm (price_adj ~ town + floor_area_sqm  + storey_range + remaining_lease + children + adult + senior_citizen, data = hdb_3room)
summary (lm_3room)     # R2 = 0.7107,	Adj R2 = 0.7104  
# children              4.171e+00, adult                -1.661e+00, senior_citizen        6.883e-01

hdb_2room <- filter(hdb,flat_type == '2 ROOM')
lm_2room <- lm (price_adj ~ town + floor_area_sqm  + storey_range + remaining_lease + children + adult + senior_citizen, data = hdb_2room)
summary (lm_2room)     # R2 = 0.6209,	Adj R2 = 0.6153   
# children              5.023e+00, adult                -1.905e+00, senior_citizen        7.776e-01


hdb_MGen <- filter(hdb,flat_type == 'MULTI-GENERATION')
lm_MGen <- lm (price_adj ~ town + floor_area_sqm  + storey_range + remaining_lease + children + adult + senior_citizen, data = hdb_MGen)
summary (lm_MGen)          # R2 = 0.7876,	Adj R2 = 0.755 
# children adult and senior citizen are not statistically significant

hdb_Exec <- filter(hdb,flat_type == 'EXECUTIVE')
lm_Exec <- lm (price_adj ~ town + floor_area_sqm  + storey_range + remaining_lease + children + adult + senior_citizen, data = hdb_Exec)
summary (lm_Exec)          # R2 = 0.7161,	Adj R2 = 0.7153 
# children              5.268e+00, adult                -2.150e+00, senior_citizen        1.051e+00

lm_date <- lm (price_adj ~ date, data = hdb)
summary (lm_date)                 
lm_year <- lm (price_adj ~ year(date), data = hdb)
summary (lm_year)                 
lm_year <- lm (price_adj_sqm ~ year(date), data = hdb)
summary (lm_year)       
lm_year <- lm (price_adj ~ year(date), data = hdb_4room)
summary (lm_year)                


