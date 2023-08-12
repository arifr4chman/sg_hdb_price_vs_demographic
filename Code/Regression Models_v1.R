## Final dataset - Hong Yee
library(tidyverse)
library(caret)

hdb_final <- read.csv('hdb_train.csv')
summary(hdb_final)

# Check for missing values
missing_values_final <- colSums(is.na(hdb_final))
missing_values_final

# Check on variable class types
str(hdb_final)


## Linear Regression Model

# Train the models
# Linear-Linear Model
model_lnr_price1 <- lm(price ~ y30_34Years, data=hdb_final)
summary(model_lnr_price1)
plot(model_lnr_price1)

model_lnr_price2 <- lm(price ~ y30_34Years + y80_84Years, data=hdb_final)
summary(model_lnr_price2)
plot(model_lnr_price2)

# Using all variables
model_lnr_price3 <- lm(price ~ ., data=hdb_final)
summary(model_lnr_price3)
plot(model_lnr_price3)

# Log-Linear model
model_lnr_price_lg_lnr <- lm(log(price) ~ ., data=hdb_final)
summary(model_lnr_price_lg_lnr)
plot(model_lnr_price_lg_lnr)

# log-linear model shows a higher adj r2 value of 0.799 so we will adopt it.

# Test prediction accuracy - validation dataset
hdb_final_valid <- read.csv('hdb_validation.csv')
summary(hdb_final_valid)
str(hdb_final_valid)

# Predictions
predictions1 <- model_lnr_price_lg_lnr %>% 
                  predict(hdb_final_valid)

# Model Performance
modelPerformance1 <- data.frame(
  RMSE = RMSE(predictions1, hdb_final_valid$price),
  R2 = R2(predictions1, hdb_final_valid$price))

print(modelPerformance1)


# Test prediction accuracy - test dataset
hdb_final_test <- read.csv('hdb_test.csv')
summary(hdb_final_test)
str(hdb_final_test)

# Predictions
predictions2 <- model_lnr_price_lg_lnr %>% 
  predict(hdb_final_test)

# Model Performance
modelPerformance2 <- data.frame(
  RMSE = RMSE(predictions2, hdb_final_test$price),
  R2 = R2(predictions2, hdb_final_test$price))

print(modelPerformance2)


## Adding in interaction terms
# Age category by storey_median
model_lnr_price_lg_lnr_1 <- lm(log(price) ~ . + y30_34Years*storey_median, data=hdb_final)
summary(model_lnr_price_lg_lnr_1)
plot(model_lnr_price_lg_lnr_1)

model_lnr_price_lg_lnr_2 <- lm(log(price) ~ . + y30_34Years*storey_median + 
                                 y80_84Years*storey_median, data=hdb_final)
summary(model_lnr_price_lg_lnr_2)
plot(model_lnr_price_lg_lnr_2)

# Test prediction accuracy - validation dataset
# Predictions
predictions_storey_v <- model_lnr_price_lg_lnr_2 %>% 
  predict(hdb_final_valid)
# Model Performance
modelPerformance_storey_v <- data.frame(
  RMSE = RMSE(predictions_storey_v, hdb_final_valid$price),
  R2 = R2(predictions_storey_v, hdb_final_valid$price))

print(modelPerformance_storey_v)

# Test prediction accuracy - test dataset
# Predictions
predictions_storey_t <- model_lnr_price_lg_lnr_2 %>% 
  predict(hdb_final_test)
# Model Performance
modelPerformance_storey_t <- data.frame(
  RMSE = RMSE(predictions_storey_t, hdb_final_test$price),
  R2 = R2(predictions_storey_t, hdb_final_test$price))

print(modelPerformance_storey_t)

# Age category by location
model_lnr_price_lg_lnr_3 <- lm(log(price) ~ . + y30_34Years*townCENTRAL.AREA + 
                                 y80_84Years*townCENTRAL.AREA, data=hdb_final)
summary(model_lnr_price_lg_lnr_3)
plot(model_lnr_price_lg_lnr_3)

# Interesting point is that the coefficient for the 80 to 84 age group for the
# central area is not statistically signifcant, but it is for the 30 to 34 years
# age group. However, the coefficient is larger than the other two interaction
# terms modeled.

# Test prediction accuracy - validation dataset
# Predictions
predictions_loc_v <- model_lnr_price_lg_lnr_3 %>% 
  predict(hdb_final_valid)
# Model Performance
modelPerformance_loc_v <- data.frame(
  RMSE = RMSE(predictions_loc_v, hdb_final_valid$price),
  R2 = R2(predictions_loc_v, hdb_final_valid$price))

print(modelPerformance_loc_v)

# Test prediction accuracy - test dataset
# Predictions
predictions_loc_t <- model_lnr_price_lg_lnr_3 %>% 
  predict(hdb_final_test)
# Model Performance
modelPerformance_loc_t <- data.frame(
  RMSE = RMSE(predictions_loc_t, hdb_final_test$price),
  R2 = R2(predictions_loc_t, hdb_final_test$price))

print(modelPerformance_loc_t)

# Age category by size of apartment
model_lnr_price_lg_lnr_4 <- lm(log(price) ~ . + y30_34Years*floor_area_sqm  + 
                                 y80_84Years*floor_area_sqm , data=hdb_final)
summary(model_lnr_price_lg_lnr_4)
plot(model_lnr_price_lg_lnr_4)

# Test prediction accuracy - validation dataset
# Predictions
predictions_size_v <- model_lnr_price_lg_lnr_4 %>% 
  predict(hdb_final_valid)
# Model Performance
modelPerformance_size_v <- data.frame(
  RMSE = RMSE(predictions_size_v, hdb_final_valid$price),
  R2 = R2(predictions_size_v, hdb_final_valid$price))

print(modelPerformance_size_v)

# Test prediction accuracy - test dataset
# Predictions
predictions_size_t <- model_lnr_price_lg_lnr_4 %>% 
  predict(hdb_final_test)
# Model Performance
modelPerformance_size_t <- data.frame(
  RMSE = RMSE(predictions_size_t, hdb_final_test$price),
  R2 = R2(predictions_size_t, hdb_final_test$price))

print(modelPerformance_size_t)

# Age category by remaining lease
model_lnr_price_lg_lnr_5 <- lm(log(price) ~ . + y30_34Years*remaining_lease  + 
                                 y80_84Years*remaining_lease , data=hdb_final)
summary(model_lnr_price_lg_lnr_5)
plot(model_lnr_price_lg_lnr_5)

# Test prediction accuracy - validation dataset
# Predictions
predictions_lease_v <- model_lnr_price_lg_lnr_5 %>% 
  predict(hdb_final_valid)
# Model Performance
modelPerformance_lease_v <- data.frame(
  RMSE = RMSE(predictions_lease_v, hdb_final_valid$price),
  R2 = R2(predictions_lease_v, hdb_final_valid$price))

print(modelPerformance_lease_v)

# Test prediction accuracy - test dataset
# Predictions
predictions_lease_t <- model_lnr_price_lg_lnr_5 %>% 
  predict(hdb_final_test)
# Model Performance
modelPerformance_lease_t <- data.frame(
  RMSE = RMSE(predictions_lease_t, hdb_final_test$price),
  R2 = R2(predictions_lease_t, hdb_final_test$price))

print(modelPerformance_lease_t)

# Highest adj R2 of 0.8028 for this model. Adding the interaction terms with
# remaining lease against age group has highest Adjusted R2 values.

## Polynomial Regression Model

model_poly_price1 <- glm(price ~ y30_34Years, data=hdb_final)
summary(model_poly_price1)
plot(model_poly_price1)

model_poly_price2 <- glm(price ~ y30_34Years + y80_84Years, data=hdb_final)
summary(model_poly_price2)
plot(model_poly_price2)

# Using all variables
model_poly_price3 <- glm(price ~ ., data=hdb_final)
summary(model_poly_price3)
plot(model_poly_price3)

# Seems like the polynomial models show strong heteroskedastic properties by 
# analysing the Q-Q residuals graph. Hence, we should use linear regression model
# for our analysis.

