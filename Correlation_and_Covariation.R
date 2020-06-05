#30042020
#Niklas Neumann
#Correlation analysis all at once, individual, and group level

#install packages
install.packages("pcor.R")
install.packages("ppcor")
install.packages("rmcorr")
install.packages("readr")
install.packages("tibble")
install.packages("tidyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")

#load libraries
library(tibble)
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)
library(ppcor)
library(rmcorr)

-------------------------------------------------------------------------------

#Load data set
Resilience <- read.csv(file = 
                           "C:\\Users\\Neuma\\surfdrive\\Shared\\anonymous_processed_output\\simulation_data_Niklas\\preprocessed_1day_history.csv")

-------------------------------------------------------------------------------

#Correlation and Covariation all at once
cor(Resilience$LOAD_sum_prev_01_days_noMissing, Resilience$EMH_1st_session_first_fut_01_trainingdays, method = "pearson", use = "pairwise.complete.obs")

print(cov(Resilience$LOAD_sum_prev_01_days_noMissing, Resilience$EMH_1st_session_first_fut_01_trainingdays, use = "pairwise.complete.obs"))

-------------------------------------------------------------------------------

#Correlation AggAnn, in order to get each days correlation
#1 Option, -0.18
AggAn1 <- rmcorr(day_count, LOAD_sum_prev_01_days_noMissing, EMH_1st_session_first_fut_01_trainingdays, Resilience)


#2 Option
AggAn <- Resilience %>%
  filter(!is.na(EMH_1st_session_first_fut_01_trainingdays)) %>%
  group_by(day_count) %>%
  summarize(N = n(), r = cor(LOAD_sum_prev_01_days_noMissing, EMH_1st_session_first_fut_01_trainingdays, use = "pairwise.complete.obs", method = "pearson"))

range(AggAn$r, na.rm = TRUE)

#-0.17
MeanAggAn <- AggAn %>% 
  filter(!is.na(r)) %>%
  summarise(mean(r), sd(r))

#plot
ggplot(AggAn, aes(x = r)) +
  geom_histogram(binwidth = 0.1)

-------------------------------------------------------------------------------

#Correlation AnAgg, in order to get each Spelers correlation
#1 Option, -0.26
AnAgg1 <- rmcorr(Speler, LOAD_sum_prev_01_days_noMissing, EMH_1st_session_first_fut_01_trainingdays, Resilience)

#2 Option
AnAgg <- Resilience %>%
  filter(!is.na(EMH_1st_session_first_fut_01_trainingdays)) %>%
  group_by(Speler) %>%
  summarise(N = n(), r = cor(LOAD_sum_prev_01_days_noMissing, EMH_1st_session_first_fut_01_trainingdays, use = "pairwise.complete.obs", method = "pearson"))

range(AnAgg$r)

#-0.26
MeanAnAgg <- AnAgg %>% 
  filter(!is.na(r)) %>%
  summarise(mean(r), sd(r))


#Plot
ggplot(AnAgg, aes(x = r)) +
  geom_histogram(binwidth = 0.1)

-------------------------------------------------------------------------------

#Data set without 0 Load
ZeroLoad <- Resilience %>%
  filter(!is.na(EMH_1st_session_first_fut_01_trainingdays), LOAD_sum_prev_01_days > 0)


#Corr all at once 0 Load
#-0.20
cor(ZeroLoad$LOAD_sum_prev_01_days_noMissing, ZeroLoad$EMH_1st_session_first_fut_01_trainingdays, use = "pairwise.complete.obs", method = "pearson")


#AggAn zero Load
#1 Option, -0.19
AggAn1.1 <- cor(day_count, LOAD_sum_prev_01_days_noMissing, EMH_1st_session_first_fut_01_trainingdays, ZeroLoad)

#2 Option, -0.14
AggAn <- ZeroLoad %>%
  group_by(day_count) %>%
  summarize(N = n(), r = cor(LOAD_sum_prev_01_days_noMissing, EMH_1st_session_first_fut_01_trainingdays, use = "pairwise.complete.obs", method = "pearson"))

range(AggAn$r, na.rm = TRUE)

MeanAggAn <- AggAn %>% 
  filter(!is.na(r)) %>%
  summarise(mean(r), sd(r))

#plot
ggplot(AggAn, aes(x = r)) +
  geom_histogram(binwidth = 0.1)

#3 Option
#rmcorr Days
#-0.19
rmcorr(day_count, LOAD_sum_prev_01_days_noMissing, EMH_1st_session_first_fut_01_trainingdays, ZeroLoad)


#AnAgg zero Load
#1 Option, -0.2
AnAgg1.1 <- cor(Speler, LOAD_sum_prev_01_days_noMissing, EMH_1st_session_first_fut_01_trainingdays, ZeroLoad)

#2 Option, -0.2
AnAgg <- ZeroLoad %>%
  group_by(Speler) %>%
  summarize(N = n(), r = cor(LOAD_sum_prev_01_days_noMissing, EMH_1st_session_first_fut_01_trainingdays, use = "pairwise.complete.obs", method = "pearson"))

range(AnAgg$r, na.rm = TRUE)

MeanAnAgg <- AnAgg %>% 
  filter(!is.na(r)) %>%
  summarise(mean(r), sd(r))

ggplot(AnAgg, aes(x = r)) +
  geom_histogram(binwidth = 0.1)

#rmcorr Speler
#-0.2
rmcorr(Speler, LOAD_sum_prev_01_days_noMissing, EMH_1st_session_first_fut_01_trainingdays, ZeroLoad)

