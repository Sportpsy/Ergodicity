#Niklas Neumann
#16062020
#new calc of central tendencies and correlations

-----------------------------------------------------------------------------
#install packages
  
--------------------------------------------------------------------------------
#load libraries
library(tibble)
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)

--------------------------------------------------------------------------
#load data set
Resilience <- read.csv(file = 
                           "C:\\Users\\Neuma\\surfdrive\\Shared\\anonymous_processed_output\\simulation_data_Niklas\\preprocessed_1day_history.csv")

#check out data set
dim(Resilience)
glimpse(Resilience)
table(Resilience$day_count)
sort(table(Resilience$Speler))
---------------------------------------------------------------------------
#new data set
ErgodicityDataSet <- Resilience %>% 
  filter(LOAD_sum_prev_01_days_noMissing > 0) %>%
  filter(day_count > 949 & day_count < 1011)

#check out new data set
dim(ErgodicityDataSet)
glimpse(ErgodicityDataSet)
table(ErgodicityDataSet$day_count)
sort(table(ErgodicityDataSet$Speler))

#possible periods for analysis
#1 -> 777-840; 63 days in real; 63 days in data set; 63 players
#2 -> 949-1010; 61 days in real; 61 days in data set; 61 players
