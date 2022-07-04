#Installing essential packages
install.packages("tidyverse")
install.packages("VIM")
install.packages("dplyr")
install.packages("writexl")
install.packages("hms")
install.packages("lubridate")

#Importing installed packages
library(tidyverse)
library(VIM)
library(dplyr)
library(writexl)
library(hms)
library(lubridate)

#Setting up of working directory
setwd("E://Bike Sharing")
getwd()

#Importing data sets
may <- read_csv("202205-divvy-tripdata.csv")
april <- read_csv("202204-divvy-tripdata.csv")
march <- read_csv("202203-divvy-tripdata.csv")
feburary <- read_csv("202202-divvy-tripdata.csv")
january <- read_csv("202201-divvy-tripdata.csv")

#Verifying data is imported successfully
head(may)
head(april)
head(march)
head(feburary)
head(january)

#Checking the name of the columns
colnames(may)
colnames(april)
colnames(march)
colnames(feburary)
colnames(january)

#Binding all the datasets together into the tripdata data set
tripdata <- rbind(january, feburary, march, april, may)

#Checking the dimension of the tripdata dataset
dim(tripdata)

#Viewing top 10 rows of the tripdata dataset
head(tripdata,10)

#Renaming Column names
names(tripdata)[names(tripdata) == 'rideable_type'] <- 'bike_type'
names(tripdata)[names(tripdata) == 'started_at'] <- 'ride_start_time'
names(tripdata)[names(tripdata) == 'ended_at'] <- 'ride_end_time'
names(tripdata)[names(tripdata) == 'start_lat'] <- 'start_latitude'
names(tripdata)[names(tripdata) == 'start_lng'] <- 'start_longitude'
names(tripdata)[names(tripdata) == 'end_lat'] <- 'end_latitude'
names(tripdata)[names(tripdata) == 'end_lng'] <- 'end_longitude'
names(tripdata)[names(tripdata) == 'member_casual'] <- 'membership_type'

#Checking column names
colnames(tripdata)

#Missing Value analysis
MissingValues <- data.frame(sapply(tripdata, function(x) sum(is.na(x))))
names(MissingValues)[1] <- "Missing_Percentage"
MissingValues

#Calculate Missing Value Percentage
MissingValues$Missing_Percentage <- (MissingValues$Missing_Percentage/nrow(tripdata))*100
MissingValues

#Arranging in Descending Order
MissingValues <- MissingValues[order(MissingValues$Missing_Percentage),]
MissingValues

#Inspecting the tripdataset where end station latitude and longitude is missing
filter(tripdata, is.na(end_latitude) == TRUE & is.na(end_station_name) == FALSE)

#Deleting records from tripdata where end latitude and end longitude is missing
tripdata <- tripdata[!is.na(tripdata$end_latitude), ]  

#Checking the Dimensions of the dataset after deleting the records.
dim(tripdata)

#Analyzing the Missing Value after deleting the records for which end station cannot be predicted or imputed.
MissingValues = data.frame(sapply(tripdata, function(x) sum(is.na(x))))
names(MissingValues)[1] = "Missing_Percentage"
MissingValues

#Analyzing the data type of the tripdata dataset
str(tripdata)

#Adding a new column named as ride_length
tripdata <- tripdata %>% 
       mutate(ride_length = as_hms(difftime(ride_end_time, ride_start_time)))

#Analyzing the unique values of bike_type column
unique(tripdata$bike_type)

#Filtering and exporting data to impute missing values for Electric bikes using Spreadsheet
file1 <- tripdata %>% 
  filter(bike_type == "electric_bike")

#Filtering and exporting data to impute missing values for Classic bikes using Spreadsheet
file2 <- tripdata %>% 
  filter(bike_type == "classic_bike")

#Filtering and exporting data to impute missing values for Docked bikes using Spreadsheet
file3 <- tripdata %>% 
  filter(bike_type == "docked_bike")

#Verifying the records of the new files and the total records in the tripdata dataset
nrow(file1) + nrow(file2) + nrow(file3)
nrow(tripdata)

#Checking Missing values for file1.
MissingValues = data.frame(sapply(file1, function(x) sum(is.na(x))))
names(MissingValues)[1] = "Missing_Percentage"
MissingValues

#Checking Missing values for file2
MissingValues = data.frame(sapply(file2, function(x) sum(is.na(x))))
names(MissingValues)[1] = "Missing_Percentage"
MissingValues

#Checking missing values for file3
MissingValues = data.frame(sapply(file3, function(x) sum(is.na(x))))
names(MissingValues)[1] = "Missing_Percentage"
MissingValues

#Combining file1 and file2 which contains records with missing data
CombinedFile <- tripdata %>% 
  filter(is.na(end_station_name) == TRUE | is.na(start_station_name) == TRUE)

#Verifying the dimensions of Combined File
dim(CombinedFile)

#Exporting the data to the of file2 in order to impute the missing values in the Spreadsheet
write_xlsx(CombinedFile,"MissingData.xlsx")

#Filter to get the station details based on the latitude and longitude
#Start Station Filters
View(unique(tripdata %>%
  filter(start_latitude == 41.68 & start_longitude >= -87.64) %>%
  select(start_station_id,start_station_name,start_latitude,start_longitude) %>%
    arrange(start_latitude)))

View(tripdata %>%
  filter(start_station_name == "Parnell Ave & 119th St"))

#End Station Filters
View(unique(tripdata %>%
              filter(end_latitude == 41.68 & end_longitude >= -87.64) %>%
              select(end_station_id,end_station_name,end_latitude,end_longitude) %>%
              arrange(end_latitude, end_longitude)))

View(tripdata %>%
       filter(end_station_name == "Parnell Ave & 119th St"))
