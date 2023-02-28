# CLEAN ENVIRONMENT 
rm(list = ls());

##############################################################################

# LOAD PACKAGES
library(readxl) # to read excel
library(lubridate) # handle date time
library(magrittr) # to use pipeline
library(ggplot2)
library(gridExtra)
library(tidyverse) # to handle duplicate rows
library(openxlsx) # to handle date

##############################################################################

# Set current working directory to the location of the script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


##############################################################################

# LOAD DATA
# Read the data (change path to where your file is located)
# Note that we had to remove the first 4 rows from the original data because
# the skip parameter for read_excel did not seem to work
data <- read_csv('../input/data.csv')
glimpse(data);

##############################################################################

# DEFINE PARAMETERS
remove_columns <- c("borough","easement","apartment_number", "address", "zip_code","block","lot");
missing_rate <- 0.6;

##############################################################################

# CHANGE COLUMN NAMES TO LOWER CASE AND REPLACE SPACE BETWEEN WORDS TO '_'.
colnames(data) <- gsub("\\ ", "_", colnames(data));
colnames(data) <- tolower(colnames(data));

##############################################################################

# REMOVING USELESS COLUMNS
data <- data[,!(names(data) %in% remove_columns)];

##############################################################################

# REMOVE DUPLICATE ROWS, IF ANY.
duplicates<- sum(duplicated(data));
data <- data %>% distinct();

##############################################################################

# HANDLE COLUMN TYPES
# convert to factor
data$neighborhood <- as.factor(data$neighborhood);
data$building_class_category <- as.factor(data$building_class_category);
data$tax_class_at_present <- as.factor(data$tax_class_at_present);
data$building_class_at_present <- as.factor(data$building_class_at_present);
data$tax_class_at_time_of_sale <- as.factor(data$tax_class_at_time_of_sale);
data$building_class_at_time_of_sale <- as.factor(data$building_class_at_time_of_sale);
glimpse(data);

##############################################################################

# REMOVE ROWS WITH SALE PRICE AS 0— DENOTES TRANSFER OF OWNERSHIP
data<-data[data$sale_price!=0,];

##############################################################################

# DESCRIBING CATEGORICAL AND NUMERICAL COLUMNS
categorical_attributes <- c('neighborhood', 'building_class_category',
                            'tax_class_at_present', 'block', 
                            'building_class_at_present', 'lot', 'zip_code',
                            'tax_class_at_time_of_sale', 'building_class_at_time_of_sale', 'sale_date');
numerical_attributes <-c('year_built', 'sale_price', 'longitude', 'latitude');


# HANDLE MISSING VALUES (EXCLUDE 0)
count_missing<- function(data){
  data.frame(sapply(data, function(y) sum(length(which(is.na(y))))));
}
count_missing_data <- count_missing(data);


# create data frame where land square feet and gross square feet value is present.
data_SQRFT<-data[!is.na(data$gross_square_feet) , ];

count_missing_data_SQRFT <- count_missing(data_SQRFT);

# We now have 3 columns- year_built, longitude and latitude with missing values. 
# Since the % of missing values is less than 4%, we simply remove them.
data_SQRFT <- data_SQRFT %>%
  drop_na('year_built');
count_missing_data_SQRFT <- count_missing(data_SQRFT);


###############################################################################
# DETECTING OUTLIERS— ONLY MISTAKES (NOT MISTAKES)

# Remove data where commercial units are less than 0
data_SQRFT <- data_SQRFT %>%
  filter(commercial_units >= 0);

# find outlier based on percentile
lower_bound <- quantile(data_SQRFT$sale_price, 0.007)
outlier_ind_sale_price <- which(data_SQRFT$sale_price < lower_bound)


data_SQRFT_final <- data_SQRFT[-outlier_ind_sale_price,];


###############################################################################

# SAVE THE CLEANED DATA

write.csv(data_SQRFT_final, "../input/data_SQRFT.csv", row.names=TRUE);
