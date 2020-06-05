#30042020
#Niklas Neumann
#Test for Temporal Dependence

#install packages

#load libraries
library(tibble)
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)


#Load data set
Resilience <- read.csv(file = 
                         "C:\\Users\\Neuma\\surfdrive\\Shared\\anonymous_processed_output\\simulation_data_Niklas\\preprocessed_1day_history.csv")

#Durbin-Watson test for temporal dependance
install.packages("lmtest")
library(lmtest)

?dwtest

TempDep <- Resilience %>%
  group_by(Speler)
