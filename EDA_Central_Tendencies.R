#30042020
#Niklas Neumann
#Central Tendencies, all at once, group, and individual level

#install packages (see libraries)
install.packages("readxl")

#Load libraries
library(tibble)
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)

-------------------------------------------------------------------------------
  
#Load data set
Resilience <- read.csv(file = 
                         "C:\\Users\\Neuma\\surfdrive\\Shared\\anonymous_processed_output\\simulation_data_Niklas\\preprocessed_1day_history.csv")
-------------------------------------------------------------------------------

#Summary
print(SummaryAll <- summary(Resilience))

#The mode of EMH (14) and load (1350) of all measures
table(Resilience$EMH_1st_session_first_fut_01_trainingdays)
table(Resilience$LOAD_sum_prev_01_days_noMissing)

#OR different method (plus variationa and SD but without mode)
#Central Tendencies of all EMH and LOAD measures
CentralTend <- Resilience %>% 
  filter(!is.na(EMH_1st_session_first_fut_01_trainingdays), LOAD_sum_prev_01_days_noMissing > 0) %>% 
  summarise(mean(EMH_1st_session_first_fut_01_trainingdays), mean(LOAD_sum_prev_01_days_noMissing), var(EMH_1st_session_first_fut_01_trainingdays), var(LOAD_sum_prev_01_days_noMissing), sd(EMH_1st_session_first_fut_01_trainingdays), sd(LOAD_sum_prev_01_days_noMissing), median(Resilience$EMH_1st_session_first_fut_01_trainingdays, na.rm = TRUE), median(Resilience$LOAD_sum_prev_01_days_noMissing))

#Min (6), max (20) EMH, Min(0), max(5660) Load
min(Resilience$EMH_1st_session_first_fut_01_trainingdays, na.rm = TRUE)
max(Resilience$EMH_1st_session_first_fut_01_trainingdays, na.rm = TRUE)

min(Resilience$LOAD_sum_prev_01_days_noMissing)
max(Resilience$LOAD_sum_prev_01_days_noMissing)


-------------------------------------------------------------------------------

#1 First aggregate, then analyse (Central Tend. of EMH and LOAD)
AggAn <- Resilience %>%
  filter(!is.na(EMH_1st_session_first_fut_01_trainingdays), LOAD_sum_prev_01_days > 0) %>% 
  group_by(day_count) %>%
  summarize(mean(EMH_1st_session_first_fut_01_trainingdays), var(EMH_1st_session_first_fut_01_trainingdays), sd(EMH_1st_session_first_fut_01_trainingdays), median(EMH_1st_session_first_fut_01_trainingdays), 
            mean(LOAD_sum_prev_01_days_noMissing), var(LOAD_sum_prev_01_days_noMissing), sd(LOAD_sum_prev_01_days_noMissing))

#Min. 11.75, max. 18.25 EMH, Min. 270, max. 3448.5 Load
table(AggAn$`mean(EMH_1st_session_first_fut_01_trainingdays)`)
table(AggAn$`mean(LOAD_sum_prev_01_days_noMissing)`)

#2 Build mean of mean and sd
MeanAggAn <- AggAn %>% 
  filter(!is.na(`sd(EMH_1st_session_first_fut_01_trainingdays)`)) %>%
  summarise(mean(`mean(EMH_1st_session_first_fut_01_trainingdays)`) , mean(`mean(LOAD_sum_prev_01_days_noMissing)`), mean(`sd(EMH_1st_session_first_fut_01_trainingdays)`), mean(`sd(LOAD_sum_prev_01_days_noMissing)`))

-------------------------------------------------------------------------------


#1 First analyse, then aggregate (Central Tend. of EMH and LOAD)
AnAgg <- Resilience %>% 
  filter(!is.na(EMH_1st_session_first_fut_01_trainingdays), LOAD_sum_prev_01_days > 0) %>%
  group_by(Speler) %>%
  summarise(mean(EMH_1st_session_first_fut_01_trainingdays), var(EMH_1st_session_first_fut_01_trainingdays), sd(EMH_1st_session_first_fut_01_trainingdays), median(EMH_1st_session_first_fut_01_trainingdays),
            mean(LOAD_sum_prev_01_days_noMissing), var(LOAD_sum_prev_01_days_noMissing), sd(LOAD_sum_prev_01_days_noMissing), median(LOAD_sum_prev_01_days_noMissing))

#EMH min. 12.76; max. 17.4, Load min. 1332.13, max. 1954.75
table(AnAgg$'mean(EMH_1st_session_first_fut_01_trainingdays)')
table(AnAgg$`mean(LOAD_sum_prev_01_days_noMissing)`)

#2 Build mean of mean and sd
MeanAnAgg <- AnAgg %>%
  summarise(mean(`mean(EMH_1st_session_first_fut_01_trainingdays)`) , mean(`mean(LOAD_sum_prev_01_days_noMissing)`), mean(`sd(EMH_1st_session_first_fut_01_trainingdays)`), mean(`sd(LOAD_sum_prev_01_days_noMissing)`))

