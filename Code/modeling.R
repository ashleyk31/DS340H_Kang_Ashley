# TITLE: Modeling Code File
# Author: Ashley Kang
# DS340H Final Project

# ================================================================================
# read in necessary libraries
# 
library(tidyverse)
library(dplyr)
library(readr)
library(car)
library(ggplot2)
library(data.table)
library(MASS)
library(caret)
# ================================================================================
# read in the datasets I need for modeling

user = read.csv("/Users/younakang/Desktop/user.csv", header = TRUE)
station = read.csv("/Users/younakang/Desktop/station.csv", header = TRUE)

# remove columns that have unique values for the individual stations
station_data = station %>% dplyr::select (-name, -number, -station.id)

# ================================================================================
# Make a full model

full_model = lm(total_trips~., data = station_data)
summary(full_model)

vif(full_model)

# VIF for population is around 53. This means that this variable has multicollinearity issues.
# Remove population and remodel the full model. 

station_data = station_data %>% dplyr::select (-population)

full_model = lm(total_trips~., data = station_data)
summary(full_model)

vif(full_model)
# now there are no multiicollinearity issues with the existing columns

# ================================================================================
# Variable Selection

# 0. Skipped All Subset Selection because of the large dataset

# Since we won't be doing an all-subset comparison, we won't have all the possible 
# models with different sizes based on certain rules, but we have other models to 
# compare with the full model. 

# 1. Automatic Selection (AIC v BIC)
n = nrow(station_data)

result.AIC = step(full_model, direction="both", k = 2)
result.BIC = step(full_model,direction="both",k=log(n))


model.AIC = lm(total_trips ~ year + seasonal.status + Municipality + total_docks + 
                 n_institutions_1km + n_mbta_1km + Lat + Long + avg_precip + 
                 membership_total, data = station_data)
# 23 predictors


model.BIC = lm(total_trips ~ seasonal.status + Municipality + total_docks + 
                 n_institutions_1km + n_mbta_1km + Lat + Long + membership_total,
               data = station_data)
# 21 predictors

# ================================================================================
# Cross Validation

# 10 Fold Cross Validation
set.seed(123)
train_control = trainControl(method = "cv", number = 10)

# Training
# Full model (26 predictors)
full_model_cv = train(total_trips ~., 
                     data = station_data, 
                     method = "lm", 
                     trControl = train_control)

# Model.AIC (23 predictors)
model_AIC_cv = train(total_trips ~ year + seasonal.status + Municipality + 
                       total_docks + n_institutions_1km + n_mbta_1km + Lat + 
                       Long + avg_precip + membership_total, 
                     data = station_data, 
                     method = "lm", 
                     trControl = train_control)

# Model.BIC (21 predictors)
model_BIC_cv = train(total_trips ~ seasonal.status + Municipality + 
                       total_docks + n_institutions_1km + n_mbta_1km + Lat + 
                       Long + membership_total, data = station_data, 
                     method = "lm",  
                     trControl = train_control)


print(full_model_cv)
# RMSE      Rsquared   MAE     
# 18.79842  0.9647012  11.53904

print(model_AIC_cv)
# RMSE      Rsquared   MAE     
# 18.82897  0.9645491  11.54424

print(model_BIC_cv)
# RMSE      Rsquared   MAE     
# 18.83145  0.9644592  11.51533


# The RMSE and Rsquared are similar to each other, and both have a high Rsqured, 
# which suggests that both models are good. However, since the MAE value for the 
# BIC model is lower (average absolute error between predicted and actual values is small), 
# and since there are less predictors for the BIC model (simplicity), 
# I will choose the BIC model over the AIC model. 

# Interpretation of RMSE: the error difference between the predicted and actual 
# values are around 18 trips

# ================================================================================
# Visualize the distribution of the model chosen from above

par(mfrow = c(1,2), cex = 0.5)
par(mfrow = c(1,1), cex = 0.5)
plot(model.BIC, which = 1, pch = 16)  
mtext("non transformed BIC model\n", side = 3, line = 1)
plot(model.BIC, which = 2, pch = 16, cex = 0.7)  
mtext("non transformed BIC model\n", side = 3, line = 1, cex = 0.7, font = 2)

# There is an outlier, so I will try to transform the data and see if that can improve
# the distribution of the model

# ================================================================================
# Box-Cox Transformation

boxcox_data = station_data

boxcox_data$total_trips = boxcox_data$total_trips + 1

shifted_model = lm(total_trips ~ seasonal.status + Municipality + total_docks + 
                     n_institutions_1km + n_mbta_1km + Lat + Long + 
                     membership_total, data = boxcox_data)

boxcox_result = boxcox(shifted_model)

best_lambda_shifted = boxcox_result$x[which.max(boxcox_result$y)]
print(best_lambda_shifted)

boxcox_data$total_trips_transformed = (boxcox_data$total_trips + 1)^best_lambda_shifted-1 / 
  best_lambda_shifted
# From this, we can see that the 95% interval is too small because the sample size is too large. 

# ======================
# Make the Box-Cox model
# ======================
# Make the model to check the distribution
model.BIC.boxcox = lm(total_trips_transformed ~ seasonal.status + Municipality + 
                        total_docks + n_institutions_1km + n_mbta_1km + Lat + 
                        Long + membership_total, data = boxcox_data)

summary(model.BIC.boxcox)

# Make predictions from the model
predictions_transformed = predict(model.BIC.boxcox, newdata = boxcox_data)
# Convert the predictions back to the original scale
predictions_original_scale = ((predictions_transformed * best_lambda_shifted)+1)^
  (1 / best_lambda_shifted)

# ======================
# Plot the Box-Cox model
# ======================
par(mfrow = c(1,2), cex = 0.5)

plot(model.BIC.boxcox, which = 1, pch = 16, cex = 0.7)  
mtext("boxcox transformed BIC model\n", side = 3, line = 1, cex = 0.7, font = 2)
plot(model.BIC.boxcox, which = 2, pch = 16, cex = 0.7)  
mtext("boxcox transformed BIC model\n", side = 3, line = 1, cex = 0.7, font = 2)

# The transformation did not improve the distribution, rather it made it worse. 
# So I will keep the original non transformed BIC model. 

# Check the CV score for the boxcox model
model_BIC_boxcox_cv = train(total_trips_transformed ~ seasonal.status + Municipality + 
                              total_docks + n_institutions_1km + n_mbta_1km + Lat + Long + 
                              membership_total, data = boxcox_data, 
                     method = "lm",  
                     trControl = train_control)

print(model_BIC_boxcox_cv)

# And I will look the outlier separately to see if there is anything significant with the outlier.
cooks_d = cooks.distance(model.BIC.boxcox)
n = nrow(boxcox_data)
p_reduced = length(coef(model.BIC.boxcox))
which(cooks_d >= qf(0.05, df1 = p_reduced, df2 = n - p_reduced))

# none of the outliers appear to be concerning

model_BIC_boxcox_cv = train(total_trips ~ seasonal.status + Municipality + 
                       total_docks + n_institutions_1km + n_mbta_1km + Lat + 
                       Long + membership_total, data = station_data, 
                     method = "lm",  
                     trControl = train_control)

print(model_BIC_boxcox_cv)

# ================================================================================
# Go back to the original non transformed BIC model.
# Identify the outlier and check if it's a significant outlier

cooks_d = cooks.distance(model.BIC)
n = nrow(station_data)
p_reduced = length(coef(model.BIC))
which(cooks_d >= qf(0.05, df1 = p_reduced, df2 = n - p_reduced))

# none seem to be concerning, but I will remove the ones that are labeled in the plot
outlier_rows = c(2546, 2547)
station_data_no_outlier = station_data[-outlier_rows, ]

model.BIC.removed = lm(total_trips ~ seasonal.status + Municipality + 
                         total_docks + n_institutions_1km + n_mbta_1km + Lat + 
                         Long + membership_total, data = station_data_no_outlier)

summary(model.BIC.removed)

model_BIC_outlier_removed_cv = train(total_trips ~ seasonal.status + 
                                       Municipality + total_docks + 
                                       n_institutions_1km + n_mbta_1km + Lat + 
                                       Long + membership_total, 
                                     data = station_data_no_outlier, 
                                     method = "lm",  
                                     trControl = train_control)

print(model_BIC_outlier_removed_cv)

# ===============================
# Plot the Model with Removed Outliers
# ===============================
par(mfrow = c(1,2), cex = 0.5)

plot(model.BIC.removed, which = 1, pch = 16, cex = 0.7)  
mtext("BIC model with outliers removed\n", side = 3, line = 1, cex = 0.7, font = 2)
plot(model.BIC.removed, which = 2, pch = 16, cex = 0.7)  
mtext("BIC model with outliers removed\n", side = 3, line = 1, cex = 0.7, font = 2)


# ================================================================================
# Final Model decision

# =========        BIC Model with two outliers removed        =========

# ================================================================================
