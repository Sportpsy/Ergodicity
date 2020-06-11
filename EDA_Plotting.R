#30.04.2020
#Niklas Neumann
#Aim: This scripts aims at exploring the data set: central tendencies (mean, median, mode, sd, var, modality, skewness), outliers, max., min., plotting 
#Aim2: get a clean data set

--------------------------------------------------------------------------------
#install packages

--------------------------------------------------------------------------------
#load libraries
library(tibble)
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)

#load data set
Resilience <- read.csv(file = 
                         "C:\\Users\\Neuma\\surfdrive\\Shared\\anonymous_processed_output\\simulation_data_Niklas\\preprocessed_1day_history.csv")

------------------------------------------------------------------------------
#How many observations and variables are in the data set?
dim(Resilience)

#Summary
SummaryAll <-  print(summary(Resilience))
print(SummarySpeler <- summary(Resilience$Speler))

#Plot data set to look for outliers
ggplot(Resilience, aes(LOAD_sum_prev_01_days_noMissing, EMH_1st_session_first_fut_01_trainingdays))+
geom_point()

#check levels of recovery
levels(Resilience$EMH_1st_session_first_fut_01_trainingdays)

#create table
table(Resilience$EMH_1st_session_first_fut_01_trainingdays, Resilience$LOAD_sum_prev_01_days_noMissing)

table(Resilience$day_count)

#print data set to console
Resilience

#bar charts, histograms, density plots
ggplot(Resilience, aes(x = EMH_1st_session_first_fut_01_trainingdays, fill = Team)) + 
  geom_bar(position = "fill") +
  ylab("proportion")

ggplot(Resilience, aes(x=EMH_1st_session_first_fut_01_trainingdays))+
  geom_histogram()

ggplot(Resilience, aes(x=LOAD_sum_prev_01_days_noMissing))+
  geom_histogram()

ggplot(Resilience, aes(x=LOAD_sum_prev_01_days_noMissing))+
  geom_density()

Resilience %>%
  ggplot(aes(x=EMH_1st_session_first_fut_01_trainingdays))+
  geom_histogram(binwidth = 0.5)

#Learn about the data set
str(Resilience)

#Filter
Resilience %>%
  filter(day_count == 568) %>%
  ggplot(aes(EMH_1st_session_first_fut_01_trainingdays))+
  geom_histogram()


#Boxplot
Resilience %>%
  ggplot(aes(x = EMH_1st_session_first_fut_01_trainingdays, y = Positie)) +
  geom_boxplot()

#Histogram
ggplot(Resilience, aes(x = EMH_1st_session_first_fut_01_trainingdays))+
  geom_histogram(binwidth = 1)

#Density
ggplot(Resilience, aes(x = EMH_1st_session_first_fut_01_trainingdays))+
  geom_density(alpha = .6)

#How many NAs for EMH
table(MissingEMH)['TRUE']

#Number of entries per player
print(summary(Resilience$Speler))

#Average number of entries for Players
print(Resilience$Speler)

#Number of day_count entries
table(!duplicated(Resilience$day_count))

#Count Load and EMH
ggplot(Resilience, aes(x = EMH_1st_session_first_fut_01_trainingdays))+
  geom_histogram()

ggplot(Resilience, aes(x = LOAD_sum_prev_01_days_noMissing))+
  geom_histogram()


print(Resilience$Speler)

#number of observations per day
sort(table(Resilience$day_count))
table(Resilience$day_count)

#Number of observations per speler per day
sort(table(Resilience$Speler))

#data et without Zeros
Resiliencewithout0 <- Resilience %>%
  filter(LOAD_sum_prev_01_days_noMissing > 0)

#new characteristics of new data set
dim(Resiliencewithout0)
sort(table(Resiliencewithout0$Speler))
table(Resiliencewithout0$day_count)
sort(table(Resiliencewithout0$day_count))

#new data set
Selection1 <- Resiliencewithout0 %>%
  filter(day_count > 548 & day_count < 1111)

#calc with that data set
AggAn <- Selection1 %>%
  group_by(day_count) %>%
  summarise(meanLOAD = mean(LOAD_sum_prev_01_days_noMissing), sdLOAD = sd(LOAD_sum_prev_01_days_noMissing) )

MeanAggAn <- AggAn %>%
  summarise(mean(meanLOAD), mean(sdLOAD, na.rm = TRUE))

AnAgg <- Selection1 %>%
  group_by(Speler) %>%
  summarise(meanLOAD = mean(LOAD_sum_prev_01_days_noMissing), sdLOAD = sd(LOAD_sum_prev_01_days_noMissing))

MeanAnAgg <- AnAgg %>%
  summarise(mean(meanLOAD), mean(sdLOAD, na.rm = TRUE))
