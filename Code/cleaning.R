# TITLE: Cleaning Code File
# Author: Ashley Kang
# DS340H Final Project

# ================================================================================
# read in necessary libraries
library(readxl)
library(tidyverse)
library(geosphere)
library(fastDummies) #for factoring
library(sf) # for spacial data reading
# ================================================================================
# read in necessary datasets
d2006 = read.csv("/Users/younakang/Downloads/202006-bluebikes-tripdata.csv")
d2007 = read.csv("/Users/younakang/Downloads/202007-bluebikes-tripdata.csv")
d2008 = read.csv("/Users/younakang/Downloads/202008-bluebikes-tripdata.csv")
d2009 = read.csv("/Users/younakang/Downloads/202009-bluebikes-tripdata.csv")

d2106 = read.csv("/Users/younakang/Downloads/202106-bluebikes-tripdata.csv")
d2107 = read.csv("/Users/younakang/Downloads/202107-bluebikes-tripdata.csv")
d2108 = read.csv("/Users/younakang/Downloads/202108-bluebikes-tripdata.csv")
d2109 = read.csv("/Users/younakang/Downloads/202109-bluebikes-tripdata.csv")

d2206 = read.csv("/Users/younakang/Downloads/202206-bluebikes-tripdata.csv")
d2207 = read.csv("/Users/younakang/Downloads/202207-bluebikes-tripdata.csv")
d2208 = read.csv("/Users/younakang/Downloads/202208-bluebikes-tripdata.csv")
d2209 = read.csv("/Users/younakang/Downloads/202209-bluebikes-tripdata.csv")

d2306 = read.csv("/Users/younakang/Downloads/202306-bluebikes-tripdata.csv")
d2307 = read.csv("/Users/younakang/Downloads/202307-bluebikes-tripdata.csv")
d2308 = read.csv("/Users/younakang/Downloads/202308-bluebikes-tripdata.csv")
d2309 = read.csv("/Users/younakang/Downloads/202309-bluebikes-tripdata.csv")

d2406 = read.csv("/Users/younakang/Downloads/202406-bluebikes-tripdata.csv")
d2407 = read.csv("/Users/younakang/Downloads/202407-bluebikes-tripdata.csv")
d2408 = read.csv("/Users/younakang/Downloads/202408-bluebikes-tripdata.csv")
d2409 = read.csv("/Users/younakang/Downloads/202409-bluebikes-tripdata.csv")

# read in the station data
station = read_xlsx("/Users/younakang/Downloads/station.xlsx", col_names = FALSE)
colnames(station) = station[2, ] 
station = station[-c(1, 2), ] 
# ================================================================================
# sample and remove columns that are not needed

datasets = c("d2006", "d2007", "d2008", "d2009",
             "d2106", "d2107", "d2108", "d2109",
             "d2206", "d2207", "d2208", "d2209",
             "d2306", "d2307", "d2308", "d2309",
             "d2406", "d2407", "d2408", "d2409")

# For each dataset,
# 1. Remove the 'postal code' and 'tripduration' columns
# 2. Randomly sample 5000 rows from the data
# 3. Add the necessary columns that are required for further analysis
for (data in datasets) {
  d = get(data)
  d = d %>% 
    dplyr::select(-contains("postal.code"), -contains("tripduration"))
  d = d %>% sample_n(5000)
  d$tripDisplacement = NA;
  d$tripDuration = NA;
  d$year = NA;
  d$month = NA;
  d$start_hour = NA;
  d$end_hour = NA;
  assign(data, d)
}


# years 2023 and 2024 have different columns, so I will impute separately.
d2324dataset = c("d2306", "d2307", "d2308", "d2309",
                 "d2406", "d2407", "d2408", "d2409")

# For each dataset,
# Rename the columns so that they are consistent
for (data in d2324dataset) {
  d = get(data)
  d = d %>% rename(
    start.station.name = start_station_name, 
    end.station.name = end_station_name,
    start.station.latitude = start_lat,
    start.station.longitude = start_lng,
    end.station.latitude = end_lat,
    end.station.longitude = end_lng,
    usertype = member_casual, 
    starttime = started_at, 
    stoptime = ended_at
  )
  assign(data, d)
}

# For each dataset, 
for (data in datasets) {
  d = get(data)
  # 1. Calculate the trip duration using the starated_at and ended_at variables
  d$starttime = as.POSIXct(d$starttime, format = "%Y-%m-%d %H:%M:%S")
  d$stoptime = as.POSIXct(d$stoptime, format = "%Y-%m-%d %H:%M:%S")
  tripduration = as.numeric(difftime(d$stoptime, d$starttime, units="secs"))
  d$tripDuration = tripduration
  
  # 2. Add in year, month, day, start_hour, and end_hour values
  d$year = year(d$starttime)
  d$month = month(d$starttime)
  d$day = day(d$starttime)
  d$start_hour = hour(d$starttime)
  d$end_hour = hour(d$stoptime)
  
  d = d %>% 
    dplyr::select(-contains("starttime"), -contains("stoptime"))
  
  assign(data, d)
}


# For each dataset, 
# Calculate the trip displacement (in km)
for (data in datasets) {
  d = get(data)
  d = d %>%
    mutate(
      tripDisplacement = 
        distHaversine(matrix(c(start.station.longitude, start.station.latitude), 
                             ncol = 2),
                      matrix(c(end.station.longitude, end.station.latitude), 
                             ncol = 2)) / 1000)
  assign(data, d)
}


firstsubset = c("d2006", "d2007", "d2008", "d2009",
                "d2106", "d2107", "d2108", "d2109",
                "d2206", "d2207", "d2208", "d2209")

# For the first 3 years, 
# the rideable type variable is missing, so add that column to the datasets
for (data in firstsubset) {
  d = get(data)
  d$rideable_type = "classic_bike";
  assign(data, d)
}

# For each dataset, 
# reorder the variables
for (data in datasets) {
  d = get(data)

  # rearrange the order of the variables
  d = d %>% dplyr::select(year, month, day, start_hour, end_hour, tripDisplacement,
                   tripDuration, usertype,
                   start.station.name, end.station.name, rideable_type)

  assign(data, d)
}

# ================================================================================
# merge the monthly datasets
merged = bind_rows(mget(datasets))

# Unify the label for rideable type
merged = merged %>%
  mutate(rideable_type = case_when(
    rideable_type %in% c("classic_bike", "docked_bike") ~ "classic_bike",
    TRUE ~ rideable_type))

# Unify the label for usertype
merged = merged %>% mutate(usertype = case_when(
  usertype == "casual" ~ "Customer",
  usertype == "member" ~ "Subscriber", TRUE ~ usertype))

# merge this trip dataset with the station dataset
# match information for the start.station
start_merge = merged %>%
  left_join(station, by = c("start.station.name" = "NAME"))

# rename the columns for merging
start_merge = start_merge %>% rename(
  start.station.id = `Station ID (to match to historic system data)`,
  start.station.number = Number,
  start.total.docks = `Total Docks`,
  start.municipality = Municipality, 
  start.seasonal.status = `Seasonal Status`, 
  start.station.lat = Lat, 
  start.station.long = Long) %>% 
  dplyr::select (year, month, day, start_hour, end_hour, tripDisplacement, tripDuration, 
          usertype, rideable_type, start.station.number, start.station.id, 
          start.station.lat, start.station.long, start.seasonal.status, 
          start.municipality, start.total.docks, end.station.name)

# Merge the start_merge dataset again with the station dataset
# but this time, match information for the end.station
end_merge = start_merge %>%
  left_join(station, by = c("end.station.name" = "NAME"))

# Rename the columns
final_merge = end_merge %>% rename(
  end.station.id = `Station ID (to match to historic system data)`,
  end.station.number = Number,
  end.total.docks = `Total Docks`,
  end.municipality = Municipality, 
  end.seasonal.status = `Seasonal Status`, 
  end.station.lat = Lat, 
  end.station.long = Long) %>% 
  dplyr::select(-end.station.name)

head(final_merge, 2)
dim(final_merge)

# convert the column types to numeric if necessary
final_merge$start.station.id = as.numeric(final_merge$start.station.id)
final_merge$end.station.id = as.numeric(final_merge$end.station.id)

final_merge$start.station.lat = as.numeric(final_merge$start.station.lat)
final_merge$end.station.lat = as.numeric(final_merge$end.station.lat)

final_merge$start.station.long = as.numeric(final_merge$start.station.long)
final_merge$end.station.long = as.numeric(final_merge$end.station.long)

final_merge$start.total.docks = as.numeric(final_merge$start.total.docks)
final_merge$end.total.docks = as.numeric(final_merge$end.total.docks)

final_merge = final_merge %>% drop_na()
# NA values are a result of converting character variables to numeric types.
# It's acceptable to remove these rows since we have enough number of samples

# ===============================================================================
# factor the categorical variables if necessary
final_merge_factored = dummy_cols(final_merge, select_columns = "usertype", 
                                  remove_first_dummy = FALSE)

final_merge_factored = dummy_cols(final_merge_factored, 
                                  select_columns = "rideable_type", 
                                  remove_first_dummy = FALSE)

final_merge_factored = dummy_cols(final_merge_factored, 
                                  select_columns = "start.seasonal.status", 
                                  remove_first_dummy = FALSE)

final_merge_factored = dummy_cols(final_merge_factored, 
                                  select_columns = "start.municipality", 
                                  remove_first_dummy = FALSE)

final_merge_factored = dummy_cols(final_merge_factored, 
                                  select_columns = "end.seasonal.status", 
                                  remove_first_dummy = FALSE)

final_merge_factored = dummy_cols(final_merge_factored, 
                                  select_columns = "end.municipality", 
                                  remove_first_dummy = FALSE)

final_merge_factored = final_merge_factored %>% 
  dplyr::select (-usertype, -rideable_type, -start.seasonal.status, 
                 -start.municipality, -end.seasonal.status, -end.municipality)

final_merge_factored$start.station.id = as.numeric(final_merge_factored$start.station.id)
final_merge_factored$end.station.id = as.numeric(final_merge_factored$end.station.id)

final_merge_factored$start.station.lat = as.numeric(final_merge_factored$start.station.lat)
final_merge_factored$end.station.lat = as.numeric(final_merge_factored$end.station.lat)

final_merge_factored$start.station.long = as.numeric(final_merge_factored$start.station.long)
final_merge_factored$end.station.long = as.numeric(final_merge_factored$end.station.long)

final_merge_factored$start.total.docks = as.numeric(final_merge_factored$start.total.docks)
final_merge_factored$end.total.docks = as.numeric(final_merge_factored$end.total.docks)

# drop the NA values
final_merge_factored = final_merge_factored %>% drop_na()

# ===============================================================================
# View the shape of the dataframes
dim(final_merge)
summary(final_merge)

dim(final_merge_factored)
summary(final_merge_factored)

# ===============================================================================
# Export the dataset for future use
write.csv(final_merge, "user.csv", row.names = FALSE)
write.csv(final_merge_factored, "user_factored.csv", row.names = FALSE)


# ===============================================================================
# Now clean the station dataset
# dataset is called "station"

# Remove unnecessary rows and rename columns
station = station %>% 
  rename(
    number = Number, 
    name = NAME, 
    seasonal.status = `Seasonal Status`,
    station.id = `Station ID (to match to historic system data)`,
    total_docks = `Total Docks`
  )

station$Lat = as.numeric(station$Lat)
station$Long = as.numeric(station$Long)


# ======================
# Add in the institution information
# ======================

# only include the institution name (for debugging purpose), LAT, and LONG
schooldata = read.csv("/Users/younakang/Downloads/hd2023.csv", header = TRUE)
schooldata = schooldata %>% filter(STABBR=="MA")
schooldata = schooldata %>% dplyr::select(INSTNM, CITY, LONGITUD, LATITUDE)

# Function to calcualte the distance from the station to each institution
count_institutions_within_1km = function(station_lat, station_lon, schooldata) {
  distances = distHaversine(c(station_lon, station_lat), 
                             cbind(schooldata$LONGITUD, schooldata$LATITUDE)) / 1000
  # Distance is in km
  
  # Count institutions within 1km
  num_institutions_within_1km = sum(distances <= 1)
  return(num_institutions_within_1km)
}

# Apply the function to each station 
station$n_institutions_1km = mapply(count_institutions_within_1km,
                                     station$Lat, 
                                     station$Long, 
                                     MoreArgs = list(schooldata))
head(station)


# ======================
# Add in the mbta station information
# ======================

shapefile_data = st_read("~/Downloads/mbta_rapid_transit/MBTA_NODE.shp")

# Convert the spatial data into a dataframe that contains the LAT and LONG values 
# for each MBTA station
mbta = st_transform(shapefile_data, crs = 4326)
station_sf = st_as_sf(station, coords = c("Long", "Lat"), crs = 4326)
mbta_data = st_coordinates(station_sf)
mbta_data_df = as.data.frame(mbta_data)

# X: long Y: lat
# Calculate distance from the station to each MBTA station
count_mbta_within_1km = function(station_lat, station_lon, mbta_data_df) {
  distances = distHaversine(c(station_lon, station_lat), 
                             cbind(mbta_data_df$X, mbta_data_df$Y)) / 1000
  # Distance is in km
  
  # Count MBTA stations within 1km
  num_mbta_within_1km = sum(distances <= 1)
  return(num_mbta_within_1km)
}

# Again, apply the function to each station
station$n_mbta_1km = mapply(count_mbta_within_1km,
                             station$Lat, 
                             station$Long, 
                             MoreArgs = list(mbta_data_df))

head(station)

# ======================
# Add in the population data per year
# ======================

# add a year column
expanded_df = station %>%
  crossing(year = 2020:2024) %>% 
  dplyr::select(year, number, name, seasonal.status, Municipality, total_docks, 
                station.id, n_institutions_1km, n_mbta_1km, Lat, Long)

# Manually add in the population data (dataset not available)
expanded_df = expanded_df %>%
  mutate(population = case_when(
    Municipality == "Arlington" & year == 2020 ~ 45379,
    Municipality == "Arlington" & year == 2021 ~ 46045,
    Municipality == "Arlington" & year == 2022 ~ 45906,
    Municipality == "Arlington" & year == 2023 ~ 46161,
    Municipality == "Arlington" & year == 2024 ~ 46416,
    
    Municipality == "Boston" & year == 2020 ~ 675466,
    Municipality == "Boston" & year == 2021 ~ 657283,
    Municipality == "Boston" & year == 2022 ~ 653243,
    Municipality == "Boston" & year == 2023 ~ 653833,
    Municipality == "Boston" & year == 2024 ~ 646622,
    
    Municipality == "Brookline" & year == 2020 ~ 59223,
    Municipality == "Brookline" & year == 2021 ~ 62620,
    Municipality == "Brookline" & year == 2022 ~ 62698,
    Municipality == "Brookline" & year == 2023 ~ 63028,
    Municipality == "Brookline" & year == 2024 ~ 63359,
    
    Municipality == "Cambridge" & year == 2020 ~ 118218,
    Municipality == "Cambridge" & year == 2021 ~ 117275,
    Municipality == "Cambridge" & year == 2022 ~ 117420,
    Municipality == "Cambridge" & year == 2023 ~ 118214,
    Municipality == "Cambridge" & year == 2024 ~ 118212,
    
    Municipality == "Chelsea" & year == 2020 ~ 40585,
    Municipality == "Chelsea" & year == 2021 ~ 39069,
    Municipality == "Chelsea" & year == 2022 ~ 38602,
    Municipality == "Chelsea" & year == 2023 ~ 38319,
    Municipality == "Chelsea" & year == 2024 ~ 37563,
    
    Municipality == "Everett" & year == 2020 ~ 48949,
    Municipality == "Everett" & year == 2021 ~ 48730,
    Municipality == "Everett" & year == 2022 ~ 49600,
    Municipality == "Everett" & year == 2023 ~ 50318,
    Municipality == "Everett" & year == 2024 ~ 50774,
    
    Municipality == "Malden" & year == 2020 ~ 66076,
    Municipality == "Malden" & year == 2021 ~ 65339,
    Municipality == "Malden" & year == 2022 ~ 65069,
    Municipality == "Malden" & year == 2023 ~ 65133,
    Municipality == "Malden" & year == 2024 ~ 64818,
    
    Municipality == "Medford" & year == 2020 ~ 59496,
    Municipality == "Medford" & year == 2021 ~ 58854,
    Municipality == "Medford" & year == 2022 ~ 58691,
    Municipality == "Medford" & year == 2023 ~ 58744,
    Municipality == "Medford" & year == 2024 ~ 58493,
    
    Municipality == "Newton" & year == 2020 ~ 88863,
    Municipality == "Newton" & year == 2021 ~ 88074,
    Municipality == "Newton" & year == 2022 ~ 87852,
    Municipality == "Newton" & year == 2023 ~ 88415,
    Municipality == "Newton" & year == 2024 ~ 88265,
    
    Municipality == "Revere" & year == 2020 ~ 61876,
    Municipality == "Revere" & year == 2021 ~ 59521,
    Municipality == "Revere" & year == 2022 ~ 58476,
    Municipality == "Revere" & year == 2023 ~ 57954,
    Municipality == "Revere" & year == 2024 ~ 56646,
    
    Municipality == "Salem" & year == 2020 ~ 43692,
    Municipality == "Salem" & year == 2021 ~ 44633,
    Municipality == "Salem" & year == 2022 ~ 44504,
    Municipality == "Salem" & year == 2023 ~ 44744,
    Municipality == "Salem" & year == 2024 ~ 45094,
    
    Municipality == "Somerville" & year == 2020 ~ 80879,
    Municipality == "Somerville" & year == 2021 ~ 80246,
    Municipality == "Somerville" & year == 2022 ~ 80178,
    Municipality == "Somerville" & year == 2023 ~ 80407,
    Municipality == "Somerville" & year == 2024 ~ 80249,
    
    Municipality == "Watertown" & year == 2020 ~ 35364,
    Municipality == "Watertown" & year == 2021 ~ 35297,
    Municipality == "Watertown" & year == 2022 ~ 35210,
    Municipality == "Watertown" & year == 2023 ~ 35256,
    Municipality == "Watertown" & year == 2024 ~ 35220
  ))


# ======================
# Add in the average max temp, average min temp, and average precipitation 
# to the dataset
# ======================

expanded_df = expanded_df %>%
  crossing(month = 6:9)
#total 10640 rows

east_mun = c("Boston", "Cambridge", "Chelsea", "Everett", "Malden", 
              "Medford", "Revere", "Salem", "Somerville")

avg_TMAX_values_east = c(77.8, 82.8, 81.9, 73.8, 
                          83.6, 79.0, 83.7, 76.7, 
                          76.2, 86.5, 85.2, 72.2, 
                          72.4, 82.5, 78.5, 73.3, 
                          80.1, 84.1, 79.4, 72.7)

avg_TMIN_values_east = c(60.6, 67.8, 66.3, 57.3, 
                          65.1, 65.7, 70.0, 62.6, 
                          59.6, 68.5, 68.1, 58.2, 
                          59.4, 68.3, 65.0, 61.6, 
                          61.8, 68.0, 64.6, 59.0)

avg_precip_east = c(0.09, 0.06, 0.07, 0.03, 
                     0.09, 0.32, 0.23, 0.25,
                     0.08, 0.02, 0.05, 0.09, 
                     0.11, 0.34, 0.21, 0.13, 
                     0.13, 0.04, 0.12, 0.04)

west_mun = c("Watertown", "Newton", "Brookline", "Arlington")

avg_TMAX_values_west = c(77.7, 83.5, 81.2, 71.9, 
                          79.3, 75.7, 79.7, 71.5, 
                          74.6, 82.1, 83.0, 69.5, 
                          73.2, 81.6, 76.3, 71.7, 
                          79.0, 82.7, 77.7, 72.4)

avg_TMIN_values_west = c(58.4, 65.8, 63.1, 54.4, 
                          60.7, 61.4, 65.3, 56.5, 
                          56.7, 64.5, 64.7, 54.2, 
                          56.5, 65.7, 61.5, 58.0, 
                          59.4, 66.8, 61.5, 55.9)

avg_precip_west = c(0.08, 0.05, 0.15, 0.08, 
                     0.05, 0.45, 0.17, 0.25, 
                     0.11, 0.11, 0.09, 0.20, 
                     0.16, 0.40, 0.19, 0.29, 
                     0.08, 0.08, 0.13, 0.03)



expanded_df = expanded_df %>% mutate(
  avg_TMIN = case_when(
    # east mun
    Municipality %in% east_mun & year == 2020 & month == 6 ~ avg_TMIN_values_east[1],
    Municipality %in% east_mun & year == 2020 & month == 7 ~ avg_TMIN_values_east[2],
    Municipality %in% east_mun & year == 2020 & month == 8 ~ avg_TMIN_values_east[3],
    Municipality %in% east_mun & year == 2020 & month == 9 ~ avg_TMIN_values_east[4],
    
    Municipality %in% east_mun & year == 2021 & month == 6 ~ avg_TMIN_values_east[5],
    Municipality %in% east_mun & year == 2021 & month == 7 ~ avg_TMIN_values_east[6],
    Municipality %in% east_mun & year == 2021 & month == 8 ~ avg_TMIN_values_east[7],
    Municipality %in% east_mun & year == 2021 & month == 9 ~ avg_TMIN_values_east[8],
    
    Municipality %in% east_mun & year == 2022 & month == 6 ~ avg_TMIN_values_east[9],
    Municipality %in% east_mun & year == 2022 & month == 7 ~ avg_TMIN_values_east[10],
    Municipality %in% east_mun & year == 2022 & month == 8 ~ avg_TMIN_values_east[11],
    Municipality %in% east_mun & year == 2022 & month == 9 ~ avg_TMIN_values_east[12],
    
    Municipality %in% east_mun & year == 2023 & month == 6 ~ avg_TMIN_values_east[13],
    Municipality %in% east_mun & year == 2023 & month == 7 ~ avg_TMIN_values_east[14],
    Municipality %in% east_mun & year == 2023 & month == 8 ~ avg_TMIN_values_east[15],
    Municipality %in% east_mun & year == 2023 & month == 9 ~ avg_TMIN_values_east[16],
    
    Municipality %in% east_mun & year == 2024 & month == 6 ~ avg_TMIN_values_east[17],
    Municipality %in% east_mun & year == 2024 & month == 7 ~ avg_TMIN_values_east[18],
    Municipality %in% east_mun & year == 2024 & month == 8 ~ avg_TMIN_values_east[19],
    Municipality %in% east_mun & year == 2024 & month == 9 ~ avg_TMIN_values_east[20], 
    
    # west mun
    Municipality %in% west_mun & year == 2020 & month == 6 ~ avg_TMIN_values_west[1],
    Municipality %in% west_mun & year == 2020 & month == 7 ~ avg_TMIN_values_west[2],
    Municipality %in% west_mun & year == 2020 & month == 8 ~ avg_TMIN_values_west[3],
    Municipality %in% west_mun & year == 2020 & month == 9 ~ avg_TMIN_values_west[4],
    
    Municipality %in% west_mun & year == 2021 & month == 6 ~ avg_TMIN_values_west[5],
    Municipality %in% west_mun & year == 2021 & month == 7 ~ avg_TMIN_values_west[6],
    Municipality %in% west_mun & year == 2021 & month == 8 ~ avg_TMIN_values_west[7],
    Municipality %in% west_mun & year == 2021 & month == 9 ~ avg_TMIN_values_west[8],
    
    Municipality %in% west_mun & year == 2022 & month == 6 ~ avg_TMIN_values_west[9],
    Municipality %in% west_mun & year == 2022 & month == 7 ~ avg_TMIN_values_west[10],
    Municipality %in% west_mun & year == 2022 & month == 8 ~ avg_TMIN_values_west[11],
    Municipality %in% west_mun & year == 2022 & month == 9 ~ avg_TMIN_values_west[12],
    
    Municipality %in% west_mun & year == 2023 & month == 6 ~ avg_TMIN_values_west[13],
    Municipality %in% west_mun & year == 2023 & month == 7 ~ avg_TMIN_values_west[14],
    Municipality %in% west_mun & year == 2023 & month == 8 ~ avg_TMIN_values_west[15],
    Municipality %in% west_mun & year == 2023 & month == 9 ~ avg_TMIN_values_west[16],
    
    Municipality %in% west_mun & year == 2024 & month == 6 ~ avg_TMIN_values_west[17],
    Municipality %in% west_mun & year == 2024 & month == 7 ~ avg_TMIN_values_west[18],
    Municipality %in% west_mun & year == 2024 & month == 8 ~ avg_TMIN_values_west[19],
    Municipality %in% west_mun & year == 2024 & month == 9 ~ avg_TMIN_values_west[20]), 
  
  avg_TMAX = case_when(
    # east mun
    Municipality %in% east_mun & year == 2020 & month == 6 ~ avg_TMAX_values_east[1],
    Municipality %in% east_mun & year == 2020 & month == 7 ~ avg_TMAX_values_east[2],
    Municipality %in% east_mun & year == 2020 & month == 8 ~ avg_TMAX_values_east[3],
    Municipality %in% east_mun & year == 2020 & month == 9 ~ avg_TMAX_values_east[4],
    
    Municipality %in% east_mun & year == 2021 & month == 6 ~ avg_TMAX_values_east[5],
    Municipality %in% east_mun & year == 2021 & month == 7 ~ avg_TMAX_values_east[6],
    Municipality %in% east_mun & year == 2021 & month == 8 ~ avg_TMAX_values_east[7],
    Municipality %in% east_mun & year == 2021 & month == 9 ~ avg_TMAX_values_east[8],
    
    Municipality %in% east_mun & year == 2022 & month == 6 ~ avg_TMAX_values_east[9],
    Municipality %in% east_mun & year == 2022 & month == 7 ~ avg_TMAX_values_east[10],
    Municipality %in% east_mun & year == 2022 & month == 8 ~ avg_TMAX_values_east[11],
    Municipality %in% east_mun & year == 2022 & month == 9 ~ avg_TMAX_values_east[12],
    
    Municipality %in% east_mun & year == 2023 & month == 6 ~ avg_TMAX_values_east[13],
    Municipality %in% east_mun & year == 2023 & month == 7 ~ avg_TMAX_values_east[14],
    Municipality %in% east_mun & year == 2023 & month == 8 ~ avg_TMAX_values_east[15],
    Municipality %in% east_mun & year == 2023 & month == 9 ~ avg_TMAX_values_east[16],
    
    Municipality %in% east_mun & year == 2024 & month == 6 ~ avg_TMAX_values_east[17],
    Municipality %in% east_mun & year == 2024 & month == 7 ~ avg_TMAX_values_east[18],
    Municipality %in% east_mun & year == 2024 & month == 8 ~ avg_TMAX_values_east[19],
    Municipality %in% east_mun & year == 2024 & month == 9 ~ avg_TMAX_values_east[20], 
    
    # west mun
    Municipality %in% west_mun & year == 2020 & month == 6 ~ avg_TMAX_values_west[1],
    Municipality %in% west_mun & year == 2020 & month == 7 ~ avg_TMAX_values_west[2],
    Municipality %in% west_mun & year == 2020 & month == 8 ~ avg_TMAX_values_west[3],
    Municipality %in% west_mun & year == 2020 & month == 9 ~ avg_TMAX_values_west[4],
    
    Municipality %in% west_mun & year == 2021 & month == 6 ~ avg_TMAX_values_west[5],
    Municipality %in% west_mun & year == 2021 & month == 7 ~ avg_TMAX_values_west[6],
    Municipality %in% west_mun & year == 2021 & month == 8 ~ avg_TMAX_values_west[7],
    Municipality %in% west_mun & year == 2021 & month == 9 ~ avg_TMAX_values_west[8],
    
    Municipality %in% west_mun & year == 2022 & month == 6 ~ avg_TMAX_values_west[9],
    Municipality %in% west_mun & year == 2022 & month == 7 ~ avg_TMAX_values_west[10],
    Municipality %in% west_mun & year == 2022 & month == 8 ~ avg_TMAX_values_west[11],
    Municipality %in% west_mun & year == 2022 & month == 9 ~ avg_TMAX_values_west[12],
    
    Municipality %in% west_mun & year == 2023 & month == 6 ~ avg_TMAX_values_west[13],
    Municipality %in% west_mun & year == 2023 & month == 7 ~ avg_TMAX_values_west[14],
    Municipality %in% west_mun & year == 2023 & month == 8 ~ avg_TMAX_values_west[15],
    Municipality %in% west_mun & year == 2023 & month == 9 ~ avg_TMAX_values_west[16],
    
    Municipality %in% west_mun & year == 2024 & month == 6 ~ avg_TMAX_values_west[17],
    Municipality %in% west_mun & year == 2024 & month == 7 ~ avg_TMAX_values_west[18],
    Municipality %in% west_mun & year == 2024 & month == 8 ~ avg_TMAX_values_west[19],
    Municipality %in% west_mun & year == 2024 & month == 9 ~ avg_TMAX_values_west[20]), 
  
  avg_precip = case_when(
    # east mun
    Municipality %in% east_mun & year == 2020 & month == 6 ~ avg_precip_east[1],
    Municipality %in% east_mun & year == 2020 & month == 7 ~ avg_precip_east[2],
    Municipality %in% east_mun & year == 2020 & month == 8 ~ avg_precip_east[3],
    Municipality %in% east_mun & year == 2020 & month == 9 ~ avg_precip_east[4],
    
    Municipality %in% east_mun & year == 2021 & month == 6 ~ avg_precip_east[5],
    Municipality %in% east_mun & year == 2021 & month == 7 ~ avg_precip_east[6],
    Municipality %in% east_mun & year == 2021 & month == 8 ~ avg_precip_east[7],
    Municipality %in% east_mun & year == 2021 & month == 9 ~ avg_precip_east[8],
    
    Municipality %in% east_mun & year == 2022 & month == 6 ~ avg_precip_east[9],
    Municipality %in% east_mun & year == 2022 & month == 7 ~ avg_precip_east[10],
    Municipality %in% east_mun & year == 2022 & month == 8 ~ avg_precip_east[11],
    Municipality %in% east_mun & year == 2022 & month == 9 ~ avg_precip_east[12],
    
    Municipality %in% east_mun & year == 2023 & month == 6 ~ avg_precip_east[13],
    Municipality %in% east_mun & year == 2023 & month == 7 ~ avg_precip_east[14],
    Municipality %in% east_mun & year == 2023 & month == 8 ~ avg_precip_east[15],
    Municipality %in% east_mun & year == 2023 & month == 9 ~ avg_precip_east[16],
    
    Municipality %in% east_mun & year == 2024 & month == 6 ~ avg_precip_east[17],
    Municipality %in% east_mun & year == 2024 & month == 7 ~ avg_precip_east[18],
    Municipality %in% east_mun & year == 2024 & month == 8 ~ avg_precip_east[19],
    Municipality %in% east_mun & year == 2024 & month == 9 ~ avg_precip_east[20], 
    
    # west mun
    Municipality %in% west_mun & year == 2020 & month == 6 ~ avg_precip_west[1],
    Municipality %in% west_mun & year == 2020 & month == 7 ~ avg_precip_west[2],
    Municipality %in% west_mun & year == 2020 & month == 8 ~ avg_precip_west[3],
    Municipality %in% west_mun & year == 2020 & month == 9 ~ avg_precip_west[4],
    
    Municipality %in% west_mun & year == 2021 & month == 6 ~ avg_precip_west[5],
    Municipality %in% west_mun & year == 2021 & month == 7 ~ avg_precip_west[6],
    Municipality %in% west_mun & year == 2021 & month == 8 ~ avg_precip_west[7],
    Municipality %in% west_mun & year == 2021 & month == 9 ~ avg_precip_west[8],
    
    Municipality %in% west_mun & year == 2022 & month == 6 ~ avg_precip_west[9],
    Municipality %in% west_mun & year == 2022 & month == 7 ~ avg_precip_west[10],
    Municipality %in% west_mun & year == 2022 & month == 8 ~ avg_precip_west[11],
    Municipality %in% west_mun & year == 2022 & month == 9 ~ avg_precip_west[12],
    
    Municipality %in% west_mun & year == 2023 & month == 6 ~ avg_precip_west[13],
    Municipality %in% west_mun & year == 2023 & month == 7 ~ avg_precip_west[14],
    Municipality %in% west_mun & year == 2023 & month == 8 ~ avg_precip_west[15],
    Municipality %in% west_mun & year == 2023 & month == 9 ~ avg_precip_west[16],
    
    Municipality %in% west_mun & year == 2024 & month == 6 ~ avg_precip_west[17],
    Municipality %in% west_mun & year == 2024 & month == 7 ~ avg_precip_west[18],
    Municipality %in% west_mun & year == 2024 & month == 8 ~ avg_precip_west[19],
    Municipality %in% west_mun & year == 2024 & month == 9 ~ avg_precip_west[20])
)

head(expanded_df)

# ===============================================================================
# Add in the number of total trips per month
# Also, add in the number of total trips made by subscribers per month

start_counts = final_merge %>%
  group_by(start.station.id, year) %>%
  summarise(
    total_trips = n(),
    subscriber_trips = sum(usertype == "Subscriber", na.rm = TRUE)
  )

end_counts = final_merge %>%
  group_by(end.station.id, year) %>%
  summarise(
    total_trips = n(),
    subscriber_trips = sum(usertype == "Subscriber", na.rm = TRUE)
  )

expanded_df$station.id = as.numeric(expanded_df$station.id)
expanded_df = expanded_df %>% drop_na()

station_data = expanded_df %>%
  left_join(start_counts, by = c("station.id" = "start.station.id", "year" = "year")) %>%
  left_join(end_counts, by = c("station.id" = "end.station.id", "year" = "year"))

# Count the number of total trips made at that station
station_data$total_trips = station_data$total_trips.x + station_data$total_trips.y

# Count the number of total trips made at that station by a subscriber
station_data$membership_total = station_data$subscriber_trips.x + 
  station_data$subscriber_trips.y

station_data = station_data %>% dplyr::select (-total_trips.x, -total_trips.y, 
                                               -subscriber_trips.x, 
                                               -subscriber_trips.y)

station_data$total_docks = as.numeric(station_data$total_docks)

station_data = station_data %>%
  replace_na(list(total_trips = 0, membership_total = 0))

# ===============================================================================
# export final station dataset for future use
write.csv(station_data, "station.csv", row.names = FALSE)
