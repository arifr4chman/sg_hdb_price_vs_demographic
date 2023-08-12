# Libraries
library(rpart)
library('rpart.plot')
library('caret')
library(dplyr)
library(tidyverse)

# Import data
train = read.csv('hdb_train.csv')
valid = read.csv('hdb_validation.csv')
test = read.csv('hdb_test.csv')
summary(train)
summary(valid)
summary(test)
str(train)
str(valid)
str(test)


################# Regression Tree Model with Interaction Terms #################
# Storey Median Interaction
train_storeymedian = train
train_storeymedian$y30_34_storey_median = train_storeymedian$y30_34Years*train_storeymedian$storey_median
train_storeymedian$y80_84_storey_median = train_storeymedian$y80_84Years*train_storeymedian$storey_median

valid_storeymedian = valid
valid_storeymedian$y30_34_storey_median = valid_storeymedian$y30_34Years*valid_storeymedian$storey_median
valid_storeymedian$y80_84_storey_median = valid_storeymedian$y80_84Years*valid_storeymedian$storey_median

# Train tree model
treemodel_storey = rpart(price ~ ., data = train_storeymedian, method = 'anova')
summary(treemodel_storey)

# Plot tree
rpart.plot(treemodel_storey)

# Check for whether pruning is needed
plotcp(treemodel_storey)
min    <- which.min(treemodel_storey$cptable[, "xerror"])
cp <- treemodel_storey$cptable[min, "CP"]
xerror <- treemodel_storey$cptable[min, "xerror"]
cp
xerror

# Importance of Features
treemodel_storey$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Variable Importance with Simple Regression")

# Performance of the model
pred_train <- predict(treemodel_storey, newdata = train_storeymedian)
RMSE(pred = pred_train, obs = train_storeymedian$price)

pred_valid <- predict(treemodel_storey, newdata = valid_storeymedian)
RMSE(pred = pred_valid, obs = valid_storeymedian$price)

data.frame(Predicted = pred_train, Actual = train_storeymedian$price) %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "cadetblue") +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  labs(title = "Predicted vs Actual")



# Location Interaction
train_location = train
train_location$y30_34_central = train_location$y30_34Years*train_location$townCENTRAL.AREA
train_location$y80_84_central = train_location$y80_84Years*train_location$townCENTRAL.AREA

valid_location = valid
valid_location$y30_34_central = valid_location$y30_34Years*valid_location$townCENTRAL.AREA
valid_location$y80_84_central = valid_location$y80_84Years*valid_location$townCENTRAL.AREA

# Train tree model
treemodel_location = rpart(price ~ ., data = train_location, method = 'anova')
summary(treemodel_location)

# Plot tree
rpart.plot(treemodel_location)

# Check for whether pruning is needed
plotcp(treemodel_location)
min    <- which.min(treemodel_location$cptable[, "xerror"])
cp <- treemodel_location$cptable[min, "CP"]
xerror <- treemodel_location$cptable[min, "xerror"]
cp
xerror

# Importance of Features
treemodel_location$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Variable Importance with Simple Regression")

# Performance of the model
pred_train <- predict(treemodel_location, newdata = train_location)
RMSE(pred = pred_train, obs = train_location$price)

pred_valid <- predict(treemodel_location, newdata = valid_location)
RMSE(pred = pred_valid, obs = valid_location$price)

data.frame(Predicted = pred_train, Actual = train_location$price) %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "cadetblue") +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  labs(title = "Predicted vs Actual")



# Apartment Interaction
train_apartment = train
train_apartment$y30_34_sqm = train_apartment$y30_34Years*train_apartment$floor_area_sqm
train_apartment$y80_84_sqm = train_apartment$y80_84Years*train_apartment$floor_area_sqm

valid_apartment = valid
valid_apartment$y30_34_sqm = valid_apartment$y30_34Years*valid_apartment$floor_area_sqm
valid_apartment$y80_84_sqm = valid_apartment$y80_84Years*valid_apartment$floor_area_sqm

# Train tree model
treemodel_sqm = rpart(price ~ ., data = train_apartment, method = 'anova')
summary(treemodel_sqm)

# Plot tree
rpart.plot(treemodel_sqm)

# Check for whether pruning is needed
plotcp(treemodel_sqm)
min    <- which.min(treemodel_sqm$cptable[, "xerror"])
cp <- treemodel_sqm$cptable[min, "CP"]
xerror <- treemodel_sqm$cptable[min, "xerror"]
cp
xerror

# Importance of Features
treemodel_sqm$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Variable Importance with Simple Regression")

# Performance of the model
pred_train <- predict(treemodel_sqm, newdata = train_apartment)
RMSE(pred = pred_train, obs = train_apartment$price)

pred_valid <- predict(treemodel_sqm, newdata = valid_apartment)
RMSE(pred = pred_valid, obs = valid_apartment$price)

data.frame(Predicted = pred_train, Actual = train_apartment$price) %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "cadetblue") +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  labs(title = "Predicted vs Actual")



# remaining_lease Interaction
train_lease = train
train_lease$y30_34_lease = train_lease$y30_34Years*train_lease$remaining_lease
train_lease$y80_84_lease = train_lease$y80_84Years*train_lease$remaining_lease

valid_lease = valid
valid_lease$y30_34_lease = valid_lease$y30_34Years*valid_lease$remaining_lease
valid_lease$y80_84_lease = valid_lease$y80_84Years*valid_lease$remaining_lease

# Train tree model
treemodel_lease = rpart(price ~ ., data = train_lease, method = 'anova')
summary(treemodel_lease)

# Plot tree
rpart.plot(treemodel_lease)

# Check for whether pruning is needed
plotcp(treemodel_lease)
min    <- which.min(treemodel_lease$cptable[, "xerror"])
cp <- treemodel_lease$cptable[min, "CP"]
xerror <- treemodel_lease$cptable[min, "xerror"]
cp
xerror

# Importance of Features
treemodel_lease$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Variable Importance with Simple Regression")

# Performance of the model
pred_train <- predict(treemodel_lease, newdata = train_lease)
RMSE(pred = pred_train, obs = train_lease$price)

pred_valid <- predict(treemodel_lease, newdata = valid_lease)
RMSE(pred = pred_valid, obs = valid_lease$price)

data.frame(Predicted = pred_train, Actual = train_lease$price) %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "cadetblue") +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  labs(title = "Predicted vs Actual")



