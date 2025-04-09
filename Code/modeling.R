# TITLE: Modeling Code File
# Author: Ashley Kang
# DS340H Final Project

# ================================================================================
# read in necessary libraries

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
attach(station_data)
n = nrow(station_data)

result.AIC = step(full_model, direction="both", k = 2)
result.BIC = step(full_model,direction="both",k=log(n))


model.AIC = lm(total_trips ~ year + seasonal.status + Municipality + total_docks + 
                 n_institutions_1km + n_mbta_1km + Lat + Long + avg_precip + 
                 membership_total)
# 10 predictors


model.BIC = lm(total_trips ~ seasonal.status + Municipality + total_docks + 
                 n_institutions_1km + n_mbta_1km + Lat + Long + membership_total)
# 8 predictors

# ================================================================================
# Cross Validation

# 10 Fold Cross Validation
set.seed(123)
train_control = trainControl(method = "cv", number = 10)

# Training
# Model.AIC (10 predictors)
model_AIC_cv = train(total_trips ~ year + seasonal.status + Municipality + 
                       total_docks + n_institutions_1km + n_mbta_1km + Lat + 
                       Long + avg_precip + membership_total, 
                     data = station_data, 
                     method = "lm", 
                     trControl = train_control)

# Model.BIC (8 predictors)
model_BIC_cv = train(total_trips ~ seasonal.status + Municipality + 
                       total_docks + n_institutions_1km + n_mbta_1km + Lat + 
                       Long + membership_total, data = station_data, 
                     method = "lm",  
                     trControl = train_control)

print(model_AIC_cv)
# RMSE      Rsquared   MAE     
# 18.79621  0.9647123  11.53878

print(model_BIC_cv)
# RMSE      Rsquared   MAE     
# 18.84385  0.9644902  11.52019


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

plot(model.BIC, which = 1, pch = 16, cex = 0.7)  
mtext("non transformed BIC model\n", side = 3, line = 1, cex = 0.7, font = 2)
plot(model.BIC, which = 2, pch = 16, cex = 0.7)  
mtext("non transformed BIC model\n", side = 3, line = 1, cex = 0.7, font = 2)

# There is an outlier, so I will try to transform the data and see if that can improve
# the distribution of the model

# ================================================================================
# Box-Cox Transformation

boxcox_data = station_data

boxcox_data$total_trips = boxcox_data$total_trips + 1

shifted_model = lm(total_trips ~ seasonal.status + Municipality + 
                     total_docks + n_institutions_1km + n_mbta_1km + Lat + 
                     Long + membership_total, data = boxcox_data)

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

# And I will look the outlier separately to see if there is anything significant with the outlier.

# ================================================================================
# Identify the outlier and check if it's a significant outlier

outlier_rows = c(2546, 2547)
station_data_no_outlier = station_data[-outlier_rows, ]

model.BIC.removed = lm(total_trips ~ seasonal.status + Municipality + 
                         total_docks + n_institutions_1km + n_mbta_1km + Lat + 
                         Long + membership_total, data = station_data_no_outlier)

summary(model.BIC.removed)

# ===============================
# Plot the Model with Removed Outliers
# ===============================
par(mfrow = c(1,2), cex = 0.5)

plot(model.BIC.removed, which = 1, pch = 16, cex = 0.7)  
mtext("boxcox transformed BIC model\n", side = 3, line = 1, cex = 0.7, font = 2)
plot(model.BIC.removed, which = 2, pch = 16, cex = 0.7)  
mtext("boxcox transformed BIC model\n", side = 3, line = 1, cex = 0.7, font = 2)

# ================================================================================
# Model Comparison for Mallow's Cp (BIC Model (removed outliers) v. Full Model)

RSS_full = sum(residuals(full_model)^2)
RSS_reduced = sum(residuals(model.BIC.removed)^2)

p_full = length(coef(full_model))
p_reduced = length(coef(model.BIC.removed))
n = nrow(station_data)

# Compute Mallow's Cp
Cp_full = sum(residuals(full_model)^2) / (nrow(station_data) - length(coef(full_model)))
Cp_reduced = (RSS_reduced / Cp_full) + 2 * p_reduced - nrow(station_data)

data.frame(Model = c("BIC model", "Full Model"),p = c(p_reduced-1, p_full-1), 
           p_plus_1 =c(p_reduced, p_full),
           Cp = c(Cp_reduced, Cp_full))

#     Model                           p     p_plus_1    Cp
# 1   BIC model (removed outliers)    21    22          -80.6809
# 2   Full Model                      26    27          354.89183

# From this table above, we can see that the BIC model's Mallow's Cp is closer to its p+1 value, 
# which indicates that the BIC model has a better balance between complexity and fit.

# ================================================================================
# Final Model decision

# =========        BIC Model with two outliers removed        =========

# ================================================================================
