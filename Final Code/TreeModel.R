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


################# Regression Tree Model #################

# Train tree model
treemodel = rpart(price ~ ., data = train, method = 'anova')
summary(treemodel)

# Plot tree
rpart.plot(treemodel)

# Check for whether pruning is needed
plotcp(treemodel)
min    <- which.min(treemodel$cptable[, "xerror"])
cp <- treemodel$cptable[min, "CP"]
xerror <- treemodel$cptable[min, "xerror"]
cp
xerror

# Importance of Features
treemodel$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Variable Importance with Simple Regression")

# Performance of the model
pred_train <- predict(treemodel, newdata = train)
RMSE(pred = pred_train, obs = train$price)

pred_valid <- predict(treemodel, newdata = valid)
RMSE(pred = pred_valid, obs = valid$price)

data.frame(Predicted = pred_train, Actual = train$price) %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "cadetblue") +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  labs(title = "Predicted vs Actual")


################# Hyperparameter Tuning #################
dim(train)

hyper_grid <- expand.grid(
  minsplit = seq(2000, 20000, 2000),
  maxdepth = seq(6, 15, 1)
)
head(hyper_grid)

# total number of combinations
nrow(hyper_grid)

models <- list()

# Perform Hyperparameter tuning
for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = price ~ .,
    data    = train,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

# Find top 5 best trees
hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)

# Get Optimal Tree
optimal_tree <- rpart(
  formula = price ~ .,
  data    = train,
  method  = "anova",
  control = list(minsplit = 2000, maxdepth = 7, cp = 0.01)
)

# Plot tree
rpart.plot(optimal_tree)

# Importance of Features
optimal_tree$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Variable Importance with Simple Regression")


# Performance of the model
optimalpred_train <- predict(optimal_tree, newdata = train)
RMSE(pred = optimalpred_train, obs = train$price)

optimalpred_valid <- predict(optimal_tree, newdata = valid)
RMSE(pred = optimalpred_valid, obs = valid$price)

data.frame(Predicted = optimalpred_train, Actual = train$price) %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "cadetblue") +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  labs(title = "Predicted vs Actual")




