# Import libraries
library(ggmap)
library(ggplot2)
library(DescTools)
library(dplyr)
library(tidyverse)
library(lubridate)

# CLEAN ENVIRONMENT 
rm(list = ls())
set.seed(2022)

##############################################################################

# Set current working directory to the location of the script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

##############################################################################

# Read the input file
data <- read_csv("../input/data_SQRFT.csv")
data <- data[, 2:ncol(data)]


##############################################################################
# Filter out only residential properties
data <- data %>%
  filter(residential_units > 0)

##############################################################################
# Overlay kernel density estimates on histograms
numeric_cols <- c("sale_price", "residential_units", "commercial_units", "year_built", "gross_square_feet", "land_square_feet", "total_units")
data %>%
  select(numeric_cols) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(aes(y = ..density..),colour = 1, fill = "white") +
  geom_density(alpha = 0.2, fill = "red") +
  # Add x-axis labels
  labs(x = "Value", y = "Density") +
  # Make x-axis labels vertical
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~key, scales = "free")
##############################################################################

# Apply log transformation to the numeric columns
data_log <- data %>%
  mutate_at(numeric_cols, log)

data_log %>%
  select(numeric_cols) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(aes(y = ..density..),colour = 1, fill = "white") +
  geom_density(alpha = 0.2, fill = "red") +
  # Add x-axis labels
  labs(x = "Value", y = "Density") +
  # Make x-axis labels vertical
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~key, scales = "free")

##############################################################################
# Apply robust standardization to the numeric columns
data_rob <- data %>%
  mutate_at(numeric_cols, RobScale)

data_rob %>%
  select(numeric_cols) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(aes(y = ..density..),colour = 1, fill = "white") +
  geom_density(alpha = 0.2, fill = "red") +
  # Add x-axis labels
  labs(x = "Value", y = "Density") +
  # Make x-axis labels vertical
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~key, scales = "free")

##############################################################################
# Apply log transformation and robust standardization to the numeric columns
data_log_rob <- data %>%
  mutate_at(numeric_cols, log) %>%
  mutate_at(numeric_cols, RobScale)

data_log_rob %>%
  select(numeric_cols) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(aes(y = ..density..),colour = 1, fill = "white") +
  geom_density(alpha = 0.2, fill = "red") +
  # Add x-axis labels
  labs(x = "Value", y = "Density") +
  # Make x-axis labels vertical
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~key, scales = "free")


##############################################################################
# For categorical columns, generate bar charts
categorical_cols <- c("neighborhood", "building_class_category")
data %>%
  select(categorical_cols) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_bar() +
  # Add x-axis labels
  labs(x = "Value", y = "Count") +
  # Make x-axis labels vertical
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~key, scales = "free")

##############################################################################
