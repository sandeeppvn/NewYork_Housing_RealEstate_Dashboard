rm(list = ls())

# library(dplyr)
# library(leaflet)
library(sp)
library(ggmap)
library(readxl)
# library(purr)
library(readr)

# Set current working directory to the location of the script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# api_key <- "AIzaSyAfu0Rs7yrm3n3-AzVsYWVuLuidOkGJ6_U"

# datacommunication607@gmail.com
register_google(key = "AIzaSyA-StcS6eRfgWRQuhBT7fLwAfHd1SbHrSI") 

# Load the excel file
df <- read_excel("../input/rollingsales_manhattan.xlsx")

for (i in 1:nrow(df)) {
  print(paste0("Processing row: ", i, "/", nrow(df)))
  # Catch any warnings from geocode
  lat_long <- geocode(
    paste0(df$ADDRESS[i], ", Manhattan, NY, ", df$'ZIP CODE'[i], ", USA"),
    output = "latlon"
  )
  df$LATITUDE[i] <- lat_long[2]
  df$LONGITUDE[i] <- lat_long[1]
}



# Create a copy of the data frame
df_tmp <- df

# Convert Latitude and Longitude to numeric
df_tmp$LATITUDE <- as.numeric(df_tmp$LATITUDE)
df_tmp$LONGITUDE <- as.numeric(df_tmp$LONGITUDE)

# Convert all columns to character
df_tmp <- apply(df_tmp, 2, as.character)

# Save the table as a csv file, leave NA as blank
write.table(
  df_tmp,
  file = "../input/data.csv",
  sep = ",",
  row.names = FALSE
)

# Read the csv again
df_new <- read_csv("../input/data.csv")