# DS340H_Kang_Ashley
This is a repository for the DS340H capstone project. 
It contains the cleaned Bluebike 2020, 2021, 2022, 2023, and 2024 datasets, code for data cleaning, data analysis, and data visualization, and the final poster of the project. 

## Folder: Cleaned Data
user.csv: merged Bluebike 2020, 2021, 2022, 2023, 2024 summer months dataset
- Source: [https://bluebikes.com/system-data](https://s3.amazonaws.com/hubway-data/index.html)
- 2020 summer months data
  - 202006-bluebikes-tripdata.zip
  - 202007-bluebikes-tripdata.zip
  - 202008-bluebikes-tripdata.zip
  - 202009-bluebikes-tripdata.zip
- 2021 summer months data
  - 202106-bluebikes-tripdata.zip
  - 202107-bluebikes-tripdata.zip
  - 202108-bluebikes-tripdata.zip
  - 202109-bluebikes-tripdata.zip
- 2022 summer months data
  - 202206-bluebikes-tripdata.zip
  - 202207-bluebikes-tripdata.zip
  - 202208-bluebikes-tripdata.zip
  - 202209-bluebikes-tripdata.zip
- 2023 summer months data
  - 202306-bluebikes-tripdata.zip
  - 202307-bluebikes-tripdata.zip
  - 202308-bluebikes-tripdata.zip
  - 202309-bluebikes-tripdata.zip
- 2024 summer months data
  - 202406-bluebikes-tripdata.zip
  - 202407-bluebikes-tripdata.zip
  - 202408-bluebikes-tripdata.zip
  - 202409-bluebikes-tripdata.zip
 
station.csv: Bluebike station data
- Source: https://bluebikes.com/system-data (original dataset saved in DS340H_Kang_Ashley/Original Datasets/ as station.xlsx
  - saved in DS340H_Kang_Ashley/Cleaned Data/ as station.csv

## Folder: Original Datasets
institution.csv: dataset with institution information, including the name, latitude, and longitude values
- Source: https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=2023&surveyNumber=1&sid=a60b5f75-667e-4b8f-8642-361210a33fdd&rtid=7
  - saved in DS340H_Kang_Ashley/Original Datasets/ as institution.csv

MBTA_NODE.shp: spatial dataset of the MBTA subway station information, including name, latitude, longitude, line, and route
- Source: https://www.mass.gov/info-details/massgis-data-mbta-rapid-transit
  - saved in DS340H_Kang_Ashley/Original Datasets/mbta_rapid_transit.zip as MBTA_NODE.shp

above_weather.csv: weather data for the municipalies above the Charles River
- Source: https://www.ncei.noaa.gov/access/search/data-search/daily-summaries?pageSize=10&pageNum=10&dataTypes=TMIN&dataTypes=TMAX&dataTypes=PRCP&bbox=42.887,-73.499,41.238,-69.918&place=State%20or%20Province:38
  - saved in DS340H_Kang_Ashley/Original Datasets/ as above_weather.csv

below_weather.csv: weather data for the municipalities below the Charles River
- Source: same as above_weather.csv
  - saved in DS340H_Kang_Ashley/Original Datasets/ as below_weather.csv

## Folder: Code
1. cleaning.R: R script for cleaning the data
2. visualization.R: R script for visualizing the data
3. model.R: R script for creating a model for my research question


  
