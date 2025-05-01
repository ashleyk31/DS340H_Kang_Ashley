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

# VIF for population is around 53. This means that this variable has multicollinearity 
# issues. Remove population and remodel the full model. 

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
model_AIC_cv = train(total_trips ~ year + seasonal.status + Municipality + total_docks + 
                       n_institutions_1km + n_mbta_1km + Lat + Long + avg_precip + 
                       membership_total, 
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


# The RMSE and Rsquared values for all three models are similar to each other, 
# and they all have a high Rsqured, which suggests that all three models are good. 
# However, since the MAE value for the BIC model is lower (average absolute error 
# between predicted and actual values is small), and since there are less predictors 
# for the BIC model (simplicity), I will choose the BIC model over the AIC model
# and the full model. 

# ================================================================================
# Visualize the distribution of the model chosen from above

par(mfrow = c(1,2), cex = 0.5)
# par(mfrow = c(1,1), cex = 0.5)
plot(model.BIC, which = 1, pch = 16)  
mtext("BIC model\n", side = 3, line = 1, , cex = 0.7, font = 2)
plot(model.BIC, which = 2, pch = 16, cex = 0.7)  
mtext("BIC model\n", side = 3, line = 1, cex = 0.7, font = 2)

# There are outliers, so I will try to determine if any of them are influencial 
# points and then conduct a box-cox transformation because the plot above seems to 
# be violating the constant-variance assumption. 

# ================================================================================
# Identify the outlier and check if it's a significant outlier

cooks_d = cooks.distance(model.BIC)
n = nrow(station_data)
p_reduced = length(coef(model.BIC))
# I will use the 4/n as my threshold. 
threshold = 4/n
influential_indices = which(cooks_d > threshold)
length(influential_indices)
# there are a total of 272 influential indices

# new dataset
station_data_cleaned = station_data[-influential_indices, ]

model.BIC.removed = lm(total_trips ~ seasonal.status + Municipality + 
                         total_docks + n_institutions_1km + n_mbta_1km + Lat + 
                         Long + membership_total, data = station_data_cleaned)

summary(model.BIC.removed)

model_BIC_outlier_removed_cv = train(total_trips ~ seasonal.status + Municipality + 
                                       total_docks + n_institutions_1km + n_mbta_1km + Lat + 
                                       Long + membership_total, 
                                     data = station_data_cleaned, 
                                     method = "lm",  
                                     trControl = train_control)

print(model_BIC_outlier_removed_cv)
# RMSE      Rsquared   MAE     
# 13.92168  0.9729583  9.325847

# ===============================
# Plot the Model with Removed Outliers
# ===============================
par(mfrow = c(1,2), cex = 0.5)
plot(model.BIC.removed, which = 1, pch = 16, cex = 0.7)  
mtext("BIC model without outliers\n", side = 3, line = 1, cex = 0.7, font = 2)
plot(model.BIC.removed, which = 2, pch = 16, cex = 0.7)  
mtext("BIC model without outliers\n", side = 3, line = 1, cex = 0.7, font = 2)

# ================================================================================
# Box-Cox Transformation

boxcox_data = station_data_cleaned
boxcox_data$total_trips = boxcox_data$total_trips + 1

shifted_model = lm(total_trips ~ seasonal.status + Municipality + 
                     total_docks + n_institutions_1km + n_mbta_1km + Lat + 
                     Long + membership_total, data = boxcox_data)

boxcox_result = boxcox(shifted_model)

best_lambda_shifted = boxcox_result$x[which.max(boxcox_result$y)]
print(best_lambda_shifted)

if (abs(best_lambda_shifted) < 1e-5) {
  boxcox_data$total_trips_transformed = log(boxcox_data$total_trips)
} else {
  boxcox_data$total_trips_transformed = ((boxcox_data$total_trips)^best_lambda_shifted - 1) / best_lambda_shifted
}

model.BIC.boxcox = lm(total_trips_transformed ~ seasonal.status + Municipality + 
                                    total_docks + n_institutions_1km + n_mbta_1km + Lat + 
                                    Long + membership_total, data = boxcox_data)

summary(model.BIC.boxcox)

# make predictions and transform it back
predictions_transformed = predict(model.BIC.boxcox, newdata = boxcox_data)

if (abs(best_lambda_shifted) < 1e-5) {
  predictions_original_scale = exp(predictions_transformed)
} else {
  predictions_original_scale = ((predictions_transformed * best_lambda_shifted) + 1)^(1 / best_lambda_shifted)
}

# If needed, subtract 1 to undo the original shift:
predictions_original_scale = predictions_original_scale - 1

# ======================
# Plot the Box-Cox model
# ======================
par(mfrow = c(1,2), cex = 0.5)

plot(model.BIC.boxcox, which = 1, pch = 16, cex = 0.7)  
mtext("boxcox transformed BIC model\n", side = 3, line = 1, cex = 0.7, font = 2)
plot(model.BIC.boxcox, which = 2, pch = 16, cex = 0.7)  
mtext("boxcox transformed BIC model\n", side = 3, line = 1, cex = 0.7, font = 2)

# The transformation is better as the vertical spread is more stable compared to 
# the one before transformation. 


# The best non-interactive model is the BIC-selected model, with outliers removed 
# and transformed using the Box-Cox method.

# ================================================================================ 

# further consideration
# interaction model because the variables might be related

# ================================================================================ 
# Variable Selection

# 1. Automatic Selection (AIC v BIC)
n = nrow(station_data)

result.AIC.interaction = step(full_model, scope = . ~ .^2, direction="both", k = 2)
result.BIC.interaction = step(full_model, scope = . ~ .^2, direction="both",k=log(n))


model.AIC.interaction = lm(total_trips ~ year + seasonal.status + Municipality + total_docks + 
                             n_institutions_1km + n_mbta_1km + Lat + Long + month + avg_TMIN + 
                             avg_TMAX + avg_precip + membership_total + Municipality:membership_total + 
                             Lat:membership_total + Long:membership_total + n_mbta_1km:membership_total + 
                             n_institutions_1km:n_mbta_1km + n_institutions_1km:Long + 
                             Municipality:n_institutions_1km + total_docks:Lat + avg_TMIN:avg_TMAX + 
                             n_institutions_1km:membership_total + Municipality:total_docks + 
                             Lat:Long + avg_precip:membership_total + total_docks:n_institutions_1km + 
                             total_docks:n_mbta_1km + year:Municipality + year:Long + 
                             month:avg_TMIN + seasonal.status:n_mbta_1km + seasonal.status:n_institutions_1km + 
                             n_mbta_1km:Lat + n_institutions_1km:avg_precip + year:membership_total + 
                             avg_TMAX:avg_precip + Municipality:avg_TMAX + year:avg_precip + 
                             year:month + avg_TMIN:avg_precip + Municipality:Long + Municipality:Lat + 
                             Municipality:n_mbta_1km + n_mbta_1km:Long + seasonal.status:Lat + 
                             year:seasonal.status + year:avg_TMIN + year:avg_TMAX + seasonal.status:Long + 
                             year:n_institutions_1km + n_mbta_1km:avg_precip, data = station_data)
# 146 predictors

model.BIC.interaction = lm(total_trips ~ year + seasonal.status + Municipality + total_docks + 
                 n_institutions_1km + n_mbta_1km + Lat + Long + month + avg_TMIN + 
                 avg_TMAX + avg_precip + membership_total + Municipality:membership_total + 
                 Lat:membership_total + Long:membership_total + n_mbta_1km:membership_total + 
                 n_institutions_1km:n_mbta_1km + n_institutions_1km:Long + 
                 n_institutions_1km:membership_total + total_docks:Lat + avg_TMIN:avg_TMAX + 
                 avg_precip:membership_total + Lat:Long + n_institutions_1km:Lat + 
                 month:avg_TMIN + seasonal.status:n_mbta_1km + total_docks:n_institutions_1km + 
                 total_docks:n_mbta_1km + n_institutions_1km:avg_precip + 
                 year:membership_total + avg_TMAX:avg_precip + avg_TMIN:avg_precip + 
                 year:avg_precip + year:Long + year:Lat + year:month + Long:avg_TMAX + 
                 seasonal.status:n_institutions_1km,
               data = station_data)
# 65 predictors

# ================================================================================
# Cross Validation

# 10 Fold Cross Validation
set.seed(123)
train_control = trainControl(method = "cv", number = 10)

# Training

# Model.AIC (146 predictors)
model_AIC_interaction_cv = train(total_trips ~ year + seasonal.status + Municipality + total_docks + 
                       n_institutions_1km + n_mbta_1km + Lat + Long + month + avg_TMIN + 
                       avg_TMAX + avg_precip + membership_total + Municipality:membership_total + 
                       Lat:membership_total + Long:membership_total + n_mbta_1km:membership_total + 
                       n_institutions_1km:n_mbta_1km + n_institutions_1km:Long + 
                       Municipality:n_institutions_1km + total_docks:Lat + avg_TMIN:avg_TMAX + 
                       n_institutions_1km:membership_total + Municipality:total_docks + 
                       Lat:Long + avg_precip:membership_total + total_docks:n_institutions_1km + 
                       total_docks:n_mbta_1km + year:Municipality + year:Long + 
                       month:avg_TMIN + seasonal.status:n_mbta_1km + seasonal.status:n_institutions_1km + 
                       n_mbta_1km:Lat + n_institutions_1km:avg_precip + year:membership_total + 
                       avg_TMAX:avg_precip + Municipality:avg_TMAX + year:avg_precip + 
                       year:month + avg_TMIN:avg_precip + Municipality:Long + Municipality:Lat + 
                       Municipality:n_mbta_1km + n_mbta_1km:Long + seasonal.status:Lat + 
                       year:seasonal.status + year:avg_TMIN + year:avg_TMAX + seasonal.status:Long + 
                       year:n_institutions_1km + n_mbta_1km:avg_precip, 
                     data = station_data, 
                     method = "lm", 
                     trControl = train_control)

# Model.BIC (65)
model_BIC_interaction_cv = train(total_trips ~ year + seasonal.status + Municipality + total_docks + 
                       n_institutions_1km + n_mbta_1km + Lat + Long + month + avg_TMIN + 
                       avg_TMAX + avg_precip + membership_total + Municipality:membership_total + 
                       Lat:membership_total + Long:membership_total + n_mbta_1km:membership_total + 
                       n_institutions_1km:n_mbta_1km + n_institutions_1km:Long + 
                       n_institutions_1km:membership_total + total_docks:Lat + avg_TMIN:avg_TMAX + 
                       avg_precip:membership_total + Lat:Long + n_institutions_1km:Lat + 
                       month:avg_TMIN + seasonal.status:n_mbta_1km + total_docks:n_institutions_1km + 
                       total_docks:n_mbta_1km + n_institutions_1km:avg_precip + 
                       year:membership_total + avg_TMAX:avg_precip + avg_TMIN:avg_precip + 
                       year:avg_precip + year:Long + year:Lat + year:month + Long:avg_TMAX + 
                       seasonal.status:n_institutions_1km, data = station_data, 
                     method = "lm",  
                     trControl = train_control)


print(model_AIC_interaction_cv)
# RMSE      Rsquared   MAE     
# 15.07098  0.9773024  9.474431

print(model_BIC_interaction_cv)
# RMSE      Rsquared   MAE     
# 15.46955  0.9760552  9.741837

# model AIC with interaction seems to be a better model

# ================================================================================
# Visualize the distribution of the model chosen from above

par(mfrow = c(1,2), cex = 0.5)
# par(mfrow = c(1,1), cex = 0.5)
plot(model.AIC.interaction, which = 1, pch = 16)  
mtext("AIC interaction model\n", side = 3, line = 1, cex = 0.7, font = 2)
plot(model.AIC.interaction, which = 2, pch = 16, cex = 0.7)  
mtext("BIC interaction model\n", side = 3, line = 1, cex = 0.7, font = 2)

# ================================================================================
# the interaction doesn't seem to be better because there is a pattern? 

cooks_d = cooks.distance(model.AIC.interaction)
n = nrow(station_data)
p_reduced = length(coef(model.AIC.interaction))
# remodel the model after removing the influencial points
threshold = 4/n
influential_indices = which(cooks_d > threshold)

# new dataset
station_data_cleaned = station_data[-influential_indices, ]

model_AIC_interaction_outlier_removed = lm(total_trips ~ year + seasonal.status + Municipality + total_docks + 
                                             n_institutions_1km + n_mbta_1km + Lat + Long + month + avg_TMIN + 
                                             avg_TMAX + avg_precip + membership_total + Municipality:membership_total + 
                                             Lat:membership_total + Long:membership_total + n_mbta_1km:membership_total + 
                                             n_institutions_1km:n_mbta_1km + n_institutions_1km:Long + 
                                             Municipality:n_institutions_1km + total_docks:Lat + avg_TMIN:avg_TMAX + 
                                             n_institutions_1km:membership_total + Municipality:total_docks + 
                                             Lat:Long + avg_precip:membership_total + total_docks:n_institutions_1km + 
                                             total_docks:n_mbta_1km + year:Municipality + year:Long + 
                                             month:avg_TMIN + seasonal.status:n_mbta_1km + seasonal.status:n_institutions_1km + 
                                             n_mbta_1km:Lat + n_institutions_1km:avg_precip + year:membership_total + 
                                             avg_TMAX:avg_precip + Municipality:avg_TMAX + year:avg_precip + 
                                             year:month + avg_TMIN:avg_precip + Municipality:Long + Municipality:Lat + 
                                             Municipality:n_mbta_1km + n_mbta_1km:Long + seasonal.status:Lat + 
                                             year:seasonal.status + year:avg_TMIN + year:avg_TMAX + seasonal.status:Long + 
                                             year:n_institutions_1km + n_mbta_1km:avg_precip, data = station_data_cleaned)

# summary(model_AIC_interaction_outlier_removed)

model_AIC_interaction_outlier_removed_cv = train(total_trips ~ year + seasonal.status + Municipality + total_docks + 
                                                   n_institutions_1km + n_mbta_1km + Lat + Long + month + avg_TMIN + 
                                                   avg_TMAX + avg_precip + membership_total + Municipality:membership_total + 
                                                   Lat:membership_total + Long:membership_total + n_mbta_1km:membership_total + 
                                                   n_institutions_1km:n_mbta_1km + n_institutions_1km:Long + 
                                                   Municipality:n_institutions_1km + total_docks:Lat + avg_TMIN:avg_TMAX + 
                                                   n_institutions_1km:membership_total + Municipality:total_docks + 
                                                   Lat:Long + avg_precip:membership_total + total_docks:n_institutions_1km + 
                                                   total_docks:n_mbta_1km + year:Municipality + year:Long + 
                                                   month:avg_TMIN + seasonal.status:n_mbta_1km + seasonal.status:n_institutions_1km + 
                                                   n_mbta_1km:Lat + n_institutions_1km:avg_precip + year:membership_total + 
                                                   avg_TMAX:avg_precip + Municipality:avg_TMAX + year:avg_precip + 
                                                   year:month + avg_TMIN:avg_precip + Municipality:Long + Municipality:Lat + 
                                                   Municipality:n_mbta_1km + n_mbta_1km:Long + seasonal.status:Lat + 
                                                   year:seasonal.status + year:avg_TMIN + year:avg_TMAX + seasonal.status:Long + 
                                                   year:n_institutions_1km + n_mbta_1km:avg_precip, data = station_data_cleaned, 
                                     method = "lm",  
                                     trControl = train_control)

print(model_AIC_interaction_outlier_removed_cv)
# RMSE     Rsquared   MAE     
# 11.1547  0.9839224  7.621244
# ===============================
# Plot the Model with Removed Outliers
# ===============================
par(mfrow = c(1,2), cex = 0.5)
par(mfrow = c(1,1), cex = 0.5)
plot(model_AIC_interaction_outlier_removed, which = 1, pch = 16, cex = 0.7)  
mtext("AIC interaction model without outliers\n", side = 3, line = 1, cex = 0.7, font = 2)
plot(model_AIC_interaction_outlier_removed, which = 2, pch = 16, cex = 0.7)  
mtext("AIC interaction model without outliers\n", side = 3, line = 1, cex = 0.7, font = 2)

# ================================================================================
# Box-Cox Transformation on the AIC interaction model

boxcox_data = station_data_cleaned
boxcox_data$total_trips = boxcox_data$total_trips + 1

shifted_model = lm(total_trips ~ year + seasonal.status + Municipality + total_docks + 
                      n_institutions_1km + n_mbta_1km + Lat + Long + month + avg_TMIN + 
                      avg_TMAX + avg_precip + membership_total + Municipality:membership_total + 
                      Lat:membership_total + Long:membership_total + n_mbta_1km:membership_total + 
                      n_institutions_1km:n_mbta_1km + n_institutions_1km:Long + 
                      Municipality:n_institutions_1km + total_docks:Lat + avg_TMIN:avg_TMAX + 
                      n_institutions_1km:membership_total + Municipality:total_docks + 
                      Lat:Long + avg_precip:membership_total + total_docks:n_institutions_1km + 
                      total_docks:n_mbta_1km + year:Municipality + year:Long + 
                      month:avg_TMIN + seasonal.status:n_mbta_1km + seasonal.status:n_institutions_1km + 
                      n_mbta_1km:Lat + n_institutions_1km:avg_precip + year:membership_total + 
                      avg_TMAX:avg_precip + Municipality:avg_TMAX + year:avg_precip + 
                      year:month + avg_TMIN:avg_precip + Municipality:Long + Municipality:Lat + 
                      Municipality:n_mbta_1km + n_mbta_1km:Long + seasonal.status:Lat + 
                      year:seasonal.status + year:avg_TMIN + year:avg_TMAX + seasonal.status:Long + 
                      year:n_institutions_1km + n_mbta_1km:avg_precip, data = boxcox_data)

boxcox_result = boxcox(shifted_model)

best_lambda_shifted = boxcox_result$x[which.max(boxcox_result$y)]
print(best_lambda_shifted)

if (abs(best_lambda_shifted) < 1e-5) {
  boxcox_data$total_trips_transformed = log(boxcox_data$total_trips)
} else {
  boxcox_data$total_trips_transformed = ((boxcox_data$total_trips)^best_lambda_shifted - 1) / best_lambda_shifted
}

model.AIC.interaction.boxcox = lm(total_trips_transformed ~ year + seasonal.status + Municipality + total_docks + 
                                     n_institutions_1km + n_mbta_1km + Lat + Long + month + avg_TMIN + 
                                     avg_TMAX + avg_precip + membership_total + Municipality:membership_total + 
                                     Lat:membership_total + Long:membership_total + n_mbta_1km:membership_total + 
                                     n_institutions_1km:n_mbta_1km + n_institutions_1km:Long + 
                                     Municipality:n_institutions_1km + total_docks:Lat + avg_TMIN:avg_TMAX + 
                                     n_institutions_1km:membership_total + Municipality:total_docks + 
                                     Lat:Long + avg_precip:membership_total + total_docks:n_institutions_1km + 
                                     total_docks:n_mbta_1km + year:Municipality + year:Long + 
                                     month:avg_TMIN + seasonal.status:n_mbta_1km + seasonal.status:n_institutions_1km + 
                                     n_mbta_1km:Lat + n_institutions_1km:avg_precip + year:membership_total + 
                                     avg_TMAX:avg_precip + Municipality:avg_TMAX + year:avg_precip + 
                                     year:month + avg_TMIN:avg_precip + Municipality:Long + Municipality:Lat + 
                                     Municipality:n_mbta_1km + n_mbta_1km:Long + seasonal.status:Lat + 
                                     year:seasonal.status + year:avg_TMIN + year:avg_TMAX + seasonal.status:Long + 
                                     year:n_institutions_1km + n_mbta_1km:avg_precip, 
                                   data = boxcox_data)

summary(model.AIC.interaction.boxcox)

# make predictions and transform it back
predictions_transformed = predict(model.AIC.interaction.boxcox, newdata = boxcox_data)

if (abs(best_lambda_shifted) < 1e-5) {
  predictions_original_scale = exp(predictions_transformed)
} else {
  predictions_original_scale = ((predictions_transformed * best_lambda_shifted) + 1)^(1 / best_lambda_shifted)
}

# If needed, subtract 1 to undo the original shift:
predictions_original_scale = predictions_original_scale - 1

# ======================
# Plot the Box-Cox model
# ======================
par(mfrow = c(1,2), cex = 0.5)

plot(model.AIC.interaction.boxcox, which = 1, pch = 16, cex = 0.7)  
mtext("boxcox transformed AIC interaction model without outliers\n", side = 3, line = 1, cex = 0.7, font = 2)
plot(model.AIC.interaction.boxcox, which = 2, pch = 16, cex = 0.7)  
mtext("boxcox transformed AIC interaction model without outliers\n", side = 3, line = 1, cex = 0.7, font = 2)


# ================================================================================

# partial F-test to compare the BIC model and the AIC interaction model
# The two models are nested, so the partial F-test works. 


anova(model.BIC, model.AIC.interaction)
# Res.Df     RSS  Df Sum of Sq      F    Pr(>F)    
# 1   8858 3147771                                   
# 2   8733 1982167 125   1165605 41.083 < 2.2e-16 ***


# Under the null hypothesis, the F-statistic follows a F distribution with 
# df1 = 125 and df2 = 8733 And we get an F - statistic of

#       (3147771 − 1982167)/125
# F = ----------------------------- = 41.0832
#          (1982167/8733)


# The F-stat is approximately 41.0832 with a small p-value. 

1-pf(41.0832,df1=125,df2=8733)
# this is 0

# Because the p-value is smaller than the conventional cut off 0.05 and other possible 
# significance level, we have enough evidence to reject the null hypothesis. 
# Since we reject the null hypothesis, this allows us to conclude that the 
# additional predictors in the AIC interaction model do significantly improve the 
# model fit and is prefered over the BIC model. 

# Our final model is the AIC interaction model with outliers removed and transformed
# using the box-cox method. 

summary(model.AIC.interaction.boxcox)
