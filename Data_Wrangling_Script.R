#------- This code covers the data pre=processing, cleaning and mutation --------
#Required Libraries

library(readr) # Loads the 'readr' package to enable reading CSV files
library(janitor) # Loads 'janitor' package to clean and standardize data
library(readxl) # Loads 'readxl' package to read Excel files


# Identify and set the working directory
setwd("D:/Rdirectory_24/INF4000_DataVisualisation")# Set the working directory


# Read the 'GHG total' data from the Excel file
GHG_total_test1212 <- read_excel("GHG_rawdata.xlsx", 
                        sheet = "GHG total", 
                        range = "A8:Y42")
