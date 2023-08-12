## Data source - HDB first dataset

hdb_raw <- read.csv('Final HDB price2.csv')
summary(hdb_raw)

missing_values <- colSums(is.na(hdb_raw))
missing_percentage <- (missing_values / nrow(hdb_raw)) * 100
columns_with_missing <- names(missing_percentage[missing_percentage > 0])
columns_with_missing

# Remove rows with missing values
Data_clean <- na.omit(hdb_raw)

# Missing value final check
missing_values_final <- colSums(is.na(Data_clean))

# Check
missing_percentage_final <- (missing_values_final / nrow(Data_clean)) * 100
columns_with_missing_final <- names(missing_percentage_final[missing_percentage_final > 0])
columns_with_missing_final

library(dplyr)
hdb_mod1 <- Data_clean %>%
  mutate(young_grp = X20...24.Years+X25...29.Years+X30...34.Years+X35...39.Years+
           X35...39.Years+X40...44.Years+X45...49.Years+X50...54.Years+
           X55...59.Years) %>%
  mutate(old_grp = X60...64.Years+X65...69.Years+X70...74.Years+X75...79.Years+
           X80...84.Years+X85...89.Years+X90.Years...Over)
summary(hdb_mod1)

hdb_mod1$date <- as.Date(hdb_mod1$date, format =  "%d/%m/%Y")
class(hdb_mod1$date)

hdb_mod1_cln <- distinct(hdb_mod1)
dim(hdb_mod1_cln)

## Linear Regression Model

# Gross Price
# Linear-Linear Model
model_lnr_price1 <- lm(price~young_grp, data=hdb_mod1_cln)
summary(model_lnr_price1)
plot(model_lnr_price1)

model_lnr_price2 <- lm(price~young_grp+old_grp, data=hdb_mod1_cln)
summary(model_lnr_price2)
plot(model_lnr_price2)

# Log-Linear Model
model_lnr_price_lg_lnr <- lm(log(price)~young_grp+old_grp, data=hdb_mod1_cln)
summary(model_lnr_price_lg_lnr)
plot(model_lnr_price_lg_lnr)

# Linear-Log Model
model_lnr_price_lnr_lg <- lm(price~log(young_grp)+log(old_grp), data=hdb_mod1_cln)
summary(model_lnr_price_lnr_lg)
plot(model_lnr_price_lnr_lg)

# Log-Log Model
model_lnr_price_lg_lg <- lm(log(price)~log(young_grp)+log(old_grp), data=hdb_mod1_cln)
summary(model_lnr_price_lg_lg)
plot(model_lnr_price_lg_lg)

# Price per square metre
model_lnr_price_sqm <- lm(price_sqm~young_grp+old_grp, data=hdb_mod1_cln)
summary(model_lnr_price_sqm)
plot(model_lnr_price_sqm)

# Gross Price - inflation-adjusted
model_lnr_price_adj <- lm(price_adj~young_grp+old_grp, data=hdb_mod1_cln)
summary(model_lnr_price_adj)
plot(model_lnr_price_adj)

# Price per square metre - inflation-adjusted
model_lnr_price_adj_sqm <- lm(price_adj_sqm~young_grp+old_grp, data=hdb_mod1_cln)
summary(model_lnr_price_adj_sqm)
plot(model_lnr_price_adj)

# Polynomial Regression Model
# Gross Price
model_poly_price <- glm(price~young_grp+old_grp, data=hdb_mod1_cln)
summary(model_poly_price)
plot(model_poly_price)

# Price per square metre
model_poly_price_sqm <- glm(price_sqm~young_grp+old_grp, data=hdb_mod1_cln)
summary(model_poly_price_sqm)  
plot(model_poly_price_sqm)

# Gross Price - inflation-adjusted
model_poly_price_adj <- glm(price_adj~young_grp+old_grp, data=hdb_mod1_cln)
summary(model_poly_price_adj)
plot(model_poly_price_adj)

# Price per square metre - inflation-adjusted
model_poly_price_adj_sqm <- glm(price_adj_sqm~young_grp+old_grp, data=hdb_mod1_cln)
summary(model_poly_price_adj_sqm)
plot(model_poly_price_adj_sqm)
