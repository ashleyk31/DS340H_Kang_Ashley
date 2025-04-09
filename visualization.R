# TITLE: Visualization Code File
# Author: Ashley Kang
# DS340H Final Project

# ================================================================================
# read in necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

# ================================================================================
# read in the datasets that I cleaned
user = read.csv("/Users/younakang/Desktop/user.csv", header = TRUE)
station = read.csv("/Users/younakang/Desktop/station.csv", header = TRUE)
stationNofactor = read.csv("/Users/younakang/Desktop/STATION_FINAL.csv", header = TRUE)


# ================================================================================
# ====== PLOT 1 ======
# ================================================================================

# heatmap of number of trips by hour of the day (Subscriber)
heatmap_data = user  %>%
  filter(usertype == "Subscriber") %>%
  count(year, start_hour) 

ggplot(heatmap_data, aes(x = factor(start_hour), y = factor(year), fill = n)) +
  geom_tile() +  
  scale_fill_gradient(low = "white", high = "blue") + 
  labs(x = "Hour of the Day", y = "Year", fill = "Frequency") + 
  theme_minimal()


# heatmap of number of trips by hour of the day (Non-Subscriber)
heatmap_data_n = user  %>%
  filter(usertype != "Subscriber") %>%
  count(year, start_hour) 

ggplot(heatmap_data_n, aes(x = factor(start_hour), y = factor(year), fill = n)) +
  geom_tile() +  
  scale_fill_gradient(low = "white", high = "blue") + 
  labs(x = "Hour of the Day", y = "Year", fill = "Frequency") + 
  theme_minimal()


# stack the plots together
heatmap_data = user %>%
  mutate(user_type = ifelse(usertype == "Subscriber", 
                            "Subscriber", "Non-Subscriber")) %>%
  count(year, start_hour, user_type)

ggplot(heatmap_data, aes(x = factor(start_hour), y = factor(year), fill = n)) +
  geom_tile() +  
  scale_fill_gradient(low = "white", high = "blue", limits = c(0, 1500)) + 
  labs(title = "Number of Trips by Hour of the Day by Usertype", 
       x = "Hour of the Day", y = "Year", fill = "Number of Trips") + 
  theme_minimal() +
  facet_grid(user_type ~ ., scales = "free_y")


# ================================================================================
# ====== PLOT 2 ======
# ================================================================================

barplot_data = user %>% 
  mutate(year_month = paste(year, month, sep = "-")) %>%
  count(year, month, year_month, usertype)

ggplot(barplot_data, aes(x = year_month, y = n, fill = as.factor(usertype))) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  scale_fill_manual(values = c("Customer" = "#be96ff", "Subscriber" = "#4e0fff")) +
  labs(title = "Total Trips per Month by User Type",
       x = "Month-Year",
       y = "Trip Count",
       fill = "User Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ================================================================================
# ====== PLOT 3 ======
# ================================================================================

# line graph for the number of trips by year by Subscribers
line_graph_data = user  %>%
  filter(usertype == "Subscriber") %>%
  count(year, month) 

ggplot(line_graph_data, aes(x = month, y = n, group = year, color = factor(year))) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("blue", "green", "red", "purple", "orange")) +
  scale_x_continuous(breaks = 6:9) +  # Set x-axis breaks for months 6-9
  labs(title = "Total Trips v. Month of the year (Subscribers)", 
       x = "Month of the Year", y = "Number of Trips", color = "Year") +
  xlim(6, 9) + ylim(2750, 4400)
theme_minimal() 

# line graph for the number of trips by year by Non-Subscribers
line_graph_data_nonsub = user  %>%
  filter(usertype != "Subscriber") %>%
  count(year, month) 

ggplot(line_graph_data_nonsub, aes(x = month, y = n, group = year, color = factor(year))) + 
  geom_line(size = 1) +  # Plot a line for each year
  scale_color_manual(values = c("blue", "green", "red", "purple", "orange")) +
  scale_x_continuous(breaks = 6:9) +  
  labs(title = "Total Trips v. Month of the year (Non-Subscribers)", 
       x = "Month of the Year", y = "Number of Trips", color = "Year") +
  theme_minimal()  


# ================================================================================
# ====== PLOT 4 ======
# ================================================================================

# time series with number of trips, average max temperature, average min temperature, 
# average precipitation

below = read.csv("~/Downloads/USC00193890.csv", header = TRUE) 
#dataset for cities below the Charles River
# Boston, Watertown, Brookline

above = read.csv("~/Downloads/USC00194502.csv", header = TRUE) 
# Salem, Arlington, Cambridge, Somerville, Revere, Malden, Everett, Chelsea, Medford, Newton

# For the two weather datasets, add columns that I need, convert the weather to F
weather = c("below", "above")
for (data in weather) {
  d = get(data)
  
  d = d %>%
    dplyr::select (DATE, PRCP, TMAX, TMIN)
  
  # elevation for above is always 25.6
  # elevation for below is always 29
  
  dates = as.Date(d$DATE)
  
  filtered_dates = dates[format(dates, "%Y") %in% 2020:2024 & 
                            as.numeric(format(dates, "%m")) %in% 6:9]
  
  filtered_data = d[dates %in% filtered_dates,]
  
  filtered_data = filtered_data %>%
    mutate(
      TMIN = (TMIN/10 * 9/5) + 32,
      TMAX = (TMAX/10 * 9/5) + 32
    )
  
  assign(data, filtered_data)
}

# Manually input the data points (official data)
# below dataset
indices = which(is.na(below$TMIN))
below[indices, "TMAX"] = c(73, 74, 87, 77, 70, 59, 84, 80)
below[indices, "TMIN"] = c(54, 58, 63, 64, 51, 48, 59, 68)

p_indices = which(is.na(below$PRCP))
below[p_indices, "PRCP"] = c(0, 0.01, 0, 0.29, 0.05, 0.02, 0.01)

# above dataset
indices = which(is.na(above$TMIN))
above[indices, "TMAX"] = c(77, 79, 76, 81, 91, 94, 96, 95, 92, 76, 66, 71, 85, 
                            77, 80, 87, 75, 74, 97, 99, 100, 85, 69, 60, 78, 74, 
                            82, 78, 79, 82, 82, 84, 83, 57, 55, 61, 75, 66, 65, 
                            63, 65, 75, 81, 70)
above[indices, "TMIN"] = c(54, 58, 60, 66, 63, 70, 73, 75, 74, 59, 57, 60, 62, 
                            63, 60, 69, 59, 59, 75, 79, 72, 68, 59, 57, 63, 64, 
                            62, 66, 71, 65, 66, 59, 57, 47, 47, 51, 57, 56, 55, 
                            53, 54, 54, 62, 60)

p_indices = which(is.na(above$PRCP))
above[p_indices, "PRCP"] = c(0.03, 1.21)

# Add a date column to the dataset
user$date = as.Date(paste(user$year, user$month, user$day, sep = "-"))

# =====================================================
# Merge starting with the start municipality location
# =====================================================

above_subset = user %>%
  filter(!start.municipality %in% c("Boston", "Watertown", "Brookline"))

above_clean = above %>%
  mutate(date = as.Date(DATE)) %>% dplyr::select (-DATE)

merged_above = above_subset %>%
  left_join(above_clean, by = "date")

merged_above = merged_above %>% 
  rename(
    start.prcp = PRCP, 
    start.tmax = TMAX, 
    start.tmin = TMIN
  )

below_subset = user %>%
  filter(start.municipality %in% c("Boston", "Watertown", "Brookline"))

below_clean = below %>%
  mutate(date = as.Date(DATE)) %>% dplyr::select (-DATE)

merged_below = below_subset %>%
  left_join(below_clean, by = "date")

merged_below = merged_below %>% 
  rename(
    start.prcp = PRCP, 
    start.tmax = TMAX, 
    start.tmin = TMIN
  )

merged_final = bind_rows(merged_above, merged_below)

# drop NA values because it's not a significant amount. 
merged_final = merged_final %>% drop_na()

# =====================================================
# Merge the end municipality location
# =====================================================

above_subset_end = merged_final %>%
  filter(!end.municipality %in% c("Boston", "Watertown", "Brookline"))

merged_above_end = above_subset_end %>%
  left_join(above_clean, by = "date")

merged_above_end = merged_above_end %>% 
  rename(
    end.prcp = PRCP, 
    end.tmax = TMAX, 
    end.tmin = TMIN
  )

below_subset_end = merged_final %>%
  filter(end.municipality %in% c("Boston", "Watertown", "Brookline"))

merged_below_end = below_subset_end %>%
  left_join(below_clean, by = "date")

merged_below_end = merged_below_end %>% 
  rename(
    end.prcp = PRCP, 
    end.tmax = TMAX, 
    end.tmin = TMIN
  )

merged_final = bind_rows(merged_above_end, merged_below_end)

# Again, drop the NA values because it's not a significant amount. 
merged_final = merged_final %>% drop_na()

# write.csv(merged_final, "userdata_with_weather.csv", row.names = FALSE)

# I will be using this dataset for PLOT 4
# ================================================================================

# Use this subset since I will need the frequency 
sub_divided_subset = merged_final %>%
  group_by(date) %>%
  summarize(
    total_usage = n(),  # Total trips per day
    total_subscribers = sum(usertype == "Subscriber"),  # Count trips made by subscribers
    total_nonsubscribers = sum(usertype=="Customer"),
    avg_prcp = mean(start.prcp, na.rm = TRUE),  # Average precipitation
    avg_tmax = mean(start.tmax, na.rm = TRUE),  # Average maximum temperature
    avg_tmin = mean(start.tmin, na.rm = TRUE)   # Average minimum temperature
  )

# Time series for the year 2020
data2020 = sub_divided_subset %>%
  filter(format(date, "%Y") == "2020")

ggplot(data2020) +
  geom_line(aes(x = date, y = avg_prcp * 10, color = "Precipitation (scaled)")) +  
  geom_line(aes(x = date, y = avg_tmax * 10, color = "Max Temp (scaled)")) +
  geom_line(aes(x = date, y = avg_tmin * 10, color = "Min Temp (scaled)")) +
  geom_line(aes(x = date, y = total_subscribers, color = "Subscriber Usage")) +
  geom_line(aes(x = date, y = total_nonsubscribers, color = "Non-subscriber Usage")) +
  scale_color_manual(values = c("Precipitation (scaled)" = "green", 
                                "Max Temp (scaled)" = "red", 
                                "Subscriber Usage" = "blue", 
                                "Non-subscriber Usage" = "purple")) +
  labs(title = "Usage and Weather Patterns for Subscribers and Non-subscribers in 2020", 
       x = "Date", 
       y = "Value") +
  theme_minimal()


# Time series for year 2022
# this year had an interesting pattern of the trip usage compared to the other years
divided_2022 = sub_divided_subset %>%
  filter(format(date, "%Y") == "2022")

ggplot(divided_2022) +
  geom_line(aes(x = date, y = avg_prcp * 10, color = "Precipitation (scaled)")) +  
  geom_line(aes(x = date, y = avg_tmax * 10, color = "Max Temp (scaled)")) +
  geom_line(aes(x = date, y = avg_tmin * 10, color = "Min Temp (scaled)")) +
  geom_line(aes(x = date, y = total_subscribers, color = "Subscriber Usage")) +
  geom_line(aes(x = date, y = total_nonsubscribers, color = "Non-subscriber Usage")) +
  scale_color_manual(values = c("Precipitation (scaled)" = "green", 
                                "Max Temp (scaled)" = "red", 
                                "Subscriber Usage" = "blue", 
                                "Non-subscriber Usage" = "purple")) +
  labs(title = "Usage and Weather Patterns for Subscribers and Non-subscribers in 2020", 
       x = "Date", 
       y = "Value") +
  theme_minimal()

# But we don't see any change in pattern of the usage based on the change in precipitation 
# and the weather. So these plots show that weather doesn't have a significant effect
# on the bike usage of users.

# ================================================================================
# ====== PLOT 5 ======
# ================================================================================

# Heatmap of the number of trips that start and end at a station in a certain Municipality
heatmap_data = user %>%
  group_by(start.municipality, end.municipality) %>%
  summarise(trip_count = n())

ggplot(heatmap_data, aes(x = start.municipality, y = end.municipality, 
                         fill = trip_count)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red", trans = "log10") +
  theme_minimal() +
  labs(title = "Trip Start and End Stations Heatmap", 
       x = "Start Municipality", 
       y = "End Municipality", 
       fill = "Trip Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Most trips start and end at the same place
# This suggests that the users have a steady trip pattern of start location (going to work) 
# and end location (coming back from work), or tourists traveling from the hotel and back to the hotel. 

# But for trips that were made in the non big municipalities, there are more trips that started
# at one of those stations and end at a different station. 
