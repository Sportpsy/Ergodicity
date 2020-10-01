#01072020
#Niklas Neumann
#New Data set analysis

#load libraries ----
library(tibble)
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)
library(rmcorr)

#load data set ----
Resilience <- read.csv(file = 
                         "C:\\Users\\Neuma\\surfdrive\\Shared\\anonymous_processed_output\\simulation_data_Niklas\\preprocessed_1day_history.csv")
#Why can't I load the data set here and have to copy it into the console to make it work? Applies to almost all codes

#Data set with no NAs (for EMH and LOAD) ----
NoNAs <- Resilience %>%
  filter(!is.na(EMH_1st_session_first_fut_01_trainingdays), !is.na(LOAD_sum_prev_01_days))

sort(table(NoNAs$Speler))
  
#first select all players possible ----

SelectPlayers <- NoNAs %>%
  filter(Speler != "f54204d2fdca86277f4da0158d217143") %>%
  filter(Speler != "8dc12291626ff81d422ecfebd4c3dea0") %>%
  filter(Speler != "846a4bb1063435a68662b554ae827d11") %>%
  filter(Speler != "edf226f502129ed94e40760136d796fb") %>%
  filter(Speler != "e370a7d179ae97b3a348ab64204bb105") %>%
  filter(Speler != "35f7149aa29b3e351d54e13420351254") %>%
  filter(Speler != "83420727600bbe1a6e52d06730f39ed7") %>%
  filter(Speler != "429ccd9ea352fec5bebeceb02b0e7583") %>%
  filter(Speler != "6c93a0d7ee85ee75826d2c6b431fcae4") %>%
  filter(Speler != "65904fe8c1525bd166b0ed99c827682f") %>%
  filter(Speler != "95042e2a0366ff12c05431fce137b747") %>%
  filter(Speler != "22b428baab267bef2713948fd73f0e0f") %>%
  filter(Speler != "a1b0c972393b4c5bd7ee100d16a9abed") %>%
  filter(Speler != "b311613c4902520e27a2cf86145606cd") %>%
  filter(Speler != "a2d6432dde00603790ac55f68b8dbd99") %>%
  arrange(Speler)

sort(table(SelectPlayers$Speler))

NewDayCount <- SelectPlayers %>% arrange(day_count) %>% 
  group_by(Speler) %>% 
  mutate(row_number = row_number()) %>%
  arrange(Speler)

SquareData <- NewDayCount %>%
  filter(row_number > 0 & row_number < 70)

#Central tend All ----
#AggAn 
AggAnAll <- SquareData %>%
  group_by(row_number) %>%
  summarise(meanEMH = mean(EMH_1st_session_first_fut_01_trainingdays), meanLOAD = mean(LOAD_sum_prev_01_days), sdEMH = sd(EMH_1st_session_first_fut_01_trainingdays), sdLOAD = sd(LOAD_sum_prev_01_days), medianEMH = median(EMH_1st_session_first_fut_01_trainingdays), medianLOAD = median(LOAD_sum_prev_01_days))

MeanAggAnAll <- AggAnAll %>%
  summarise(mean(meanEMH), mean(meanLOAD), mean(sdEMH), mean(sdLOAD), mean(medianEMH), mean(medianLOAD))

----------------------------------------------------------------
#AnAgg 
AnAggAll <- SquareData %>%
  group_by(Speler) %>%
  summarise(meanEMH = mean(EMH_1st_session_first_fut_01_trainingdays), meanLOAD = mean(LOAD_sum_prev_01_days), sdEMH = sd(EMH_1st_session_first_fut_01_trainingdays), sdLOAD = sd(LOAD_sum_prev_01_days), medianEMH = median(EMH_1st_session_first_fut_01_trainingdays), medianLOAD = median(LOAD_sum_prev_01_days))

MeanAnAggAll <- AnAggAll %>%
  summarise(mean(meanEMH), mean(meanLOAD), mean(sdEMH), mean(sdLOAD), mean(medianEMH), mean(medianLOAD))

----------------------------------------------------------------
#All at once
AllatonceAll <- SquareData %>%
  summarise(meanEMH = mean(EMH_1st_session_first_fut_01_trainingdays), meanLOAD = mean(LOAD_sum_prev_01_days), sdEMH = sd(EMH_1st_session_first_fut_01_trainingdays), sdLOAD = sd(LOAD_sum_prev_01_days))

MeanAllatonceAll <- AllatonceAll %>%
  summarise(mean(meanEMH), mean(meanLOAD), mean(sdEMH), mean(sdLOAD))

-----------------------------------------
#Corr all 69 ----
CorrAggAn69 <- SquareData %>%
  group_by(row_number) %>%
  summarize(N = n(), r = cor(LOAD_sum_prev_01_days, EMH_1st_session_first_fut_01_trainingdays, use = "pairwise.complete.obs", method = "spearman")) 

MeanCorrAggAn69 <- CorrAggAn69 %>%
  filter(!is.na(r)) %>%
  summarise(mean(r), sd(r), median(r))

CIr(r = -0.2192531, n = 69, level = .95)

#this is the inter-individual approach all at once
spearman69 <- cor.test(SquareData$LOAD_sum_prev_01_days, SquareData$EMH_1st_session_first_fut_01_trainingdays, method = "spearman")

CIr(r = -0.2008837, n = 69, level = .95)

#this now is the intra-individual approach with rmcorr
rmcorr69 <- rmcorr(Speler, EMH_1st_session_first_fut_01_trainingdays, LOAD_sum_prev_01_days_noMissing, SquareData)

CorrAnAgg69 <- SquareData %>%
  group_by(Speler) %>%
  summarize(N = n(), r = cor(LOAD_sum_prev_01_days, EMH_1st_session_first_fut_01_trainingdays, use = "pairwise.complete.obs", method = "pearson"))

MeanCorrAnAgg69 <- CorrAnAgg69 %>% 
  summarise(mean(r), sd(r), median(r))

CIr(r = -0.1843496, n = 69, level = .95)

#do the same above only for each single team ----
#select team
All1f <- NoNAs %>%
  filter(Team == "1f63943fcc3909be531e7939840ac071")

sort(table(All1f$Speler))

SelectPlayersAll1f <- All1f %>%
  filter(Speler != "4fb4cb0b67b28e5c932714ce5ca99f5d") %>%
  filter(Speler != "f54204d2fdca86277f4da0158d217143") %>%
  filter(Speler != "edf226f502129ed94e40760136d796fb") %>%
  filter(Speler != "e370a7d179ae97b3a348ab64204bb105") %>%
  arrange(Speler)

sort(table(SelectPlayersAll1f$Speler))

NewDayCountAll1f <- SelectPlayersAll1f %>% arrange(day_count) %>% 
  group_by(Speler) %>% 
  mutate(row_number = row_number()) %>%
  arrange(Speler)

SquareDataAll1f <- NewDayCountAll1f %>%
  filter(row_number > 0 & row_number < 30)

sort(table(SquareDataAll1f$Speler))

#Central tend 1f ----
#AggAn 
AggAnAll1f <- SquareDataAll1f %>%
  group_by(row_number) %>%
  summarise(meanEMH = mean(EMH_1st_session_first_fut_01_trainingdays), meanLOAD = mean(LOAD_sum_prev_01_days), sdEMH = sd(EMH_1st_session_first_fut_01_trainingdays), sdLOAD = sd(LOAD_sum_prev_01_days))

MeanAggAnAll1f <- AggAnAll1f %>%
  summarise(mean(meanEMH), mean(meanLOAD), mean(sdEMH), mean(sdLOAD))

----------------------------------------------------------------
#AnAgg 
AnAggAll1f <- SquareDataAll1f %>%
  group_by(Speler) %>%
  summarise(meanEMH = mean(EMH_1st_session_first_fut_01_trainingdays), meanLOAD = mean(LOAD_sum_prev_01_days), sdEMH = sd(EMH_1st_session_first_fut_01_trainingdays), sdLOAD = sd(LOAD_sum_prev_01_days))

MeanAnAggAll1f <- AnAggAll1f %>%
  summarise(mean(meanEMH), mean(meanLOAD), mean(sdEMH), mean(sdLOAD))

----------------------------------------------------------------
#All at once
AllatonceAll1f <- SquareDataAll1f %>%
  summarise(meanEMH = mean(EMH_1st_session_first_fut_01_trainingdays), meanLOAD = mean(LOAD_sum_prev_01_days), sdEMH = sd(EMH_1st_session_first_fut_01_trainingdays), sdLOAD = sd(LOAD_sum_prev_01_days))

MeanAllatonceAll1f <- AllatonceAll1f %>%
  summarise(mean(meanEMH), mean(meanLOAD), mean(sdEMH), mean(sdLOAD))

#Corr all 1f ----
CorrAggAn291f <- SquareDataAll1f %>%
  group_by(row_number) %>%
  summarize(N = n(), r = cor(LOAD_sum_prev_01_days, EMH_1st_session_first_fut_01_trainingdays, use = "pairwise.complete.obs", method = "pearson")) 

MeanCorrAggAn291f <- CorrAggAn291f %>%
  filter(!is.na(r)) %>%
  summarise(mean(r), sd(r), median(r)) 

CorrAnAgg291f <- SquareDataAll1f %>%
  group_by(Speler) %>%
  summarize(N = n(), r = cor(LOAD_sum_prev_01_days, EMH_1st_session_first_fut_01_trainingdays, use = "pairwise.complete.obs", method = "pearson"))

MeanCorrAnAgg291f <- CorrAnAgg291f %>% 
  summarise(mean(r), sd(r), median(r))

#select team 2d ----
All2d <- NoNAs %>%
  filter(Team == "2d64fe64ed4b5f2d59ffbd31501cd294")

sort(table(All2d$Speler))

SelectPlayersAll2d <- All2d %>%
  filter(Speler != "18f141d875cf6d722a187236cfa4482e") %>%
  filter(Speler != "343dfbbcb3cf7a3869a8ea628cd880f1") %>%
  filter(Speler != "377996da64aca7f40df934a4388f7dc8") %>%
  arrange(Speler)

sort(table(SelectPlayersAll2d$Speler))

NewDayCountAll2d <- SelectPlayersAll2d %>% arrange(day_count) %>% 
  group_by(Speler) %>% 
  mutate(row_number = row_number()) %>%
  arrange(Speler)

SquareDataAll2d <- NewDayCountAll2d %>%
  filter(row_number > 0 & row_number < 31)

sort(table(SquareDataAll2d$Speler))

#Central tend 2d ----
#AggAn 
AggAnAll2d <- SquareDataAll2d %>%
  group_by(row_number) %>%
  summarise(meanEMH = mean(EMH_1st_session_first_fut_01_trainingdays), meanLOAD = mean(LOAD_sum_prev_01_days), sdEMH = sd(EMH_1st_session_first_fut_01_trainingdays), sdLOAD = sd(LOAD_sum_prev_01_days))

MeanAggAnAll2d <- AggAnAll2d %>%
  summarise(mean(meanEMH), mean(meanLOAD), mean(sdEMH), mean(sdLOAD))

----------------------------------------------------------------
#AnAgg 
AnAggAll2d <- SquareDataAll2d %>%
  group_by(Speler) %>%
  summarise(meanEMH = mean(EMH_1st_session_first_fut_01_trainingdays), meanLOAD = mean(LOAD_sum_prev_01_days), sdEMH = sd(EMH_1st_session_first_fut_01_trainingdays), sdLOAD = sd(LOAD_sum_prev_01_days))

MeanAnAggAll2d <- AnAggAll2d %>%
  summarise(mean(meanEMH), mean(meanLOAD), mean(sdEMH), mean(sdLOAD))

----------------------------------------------------------------
#All at once
AllatonceAll2d <- SquareDataAll2d %>%
  summarise(meanEMH = mean(EMH_1st_session_first_fut_01_trainingdays), meanLOAD = mean(LOAD_sum_prev_01_days), sdEMH = sd(EMH_1st_session_first_fut_01_trainingdays), sdLOAD = sd(LOAD_sum_prev_01_days))

MeanAllatonceAll2d <- AllatonceAll2d %>%
  summarise(mean(meanEMH), mean(meanLOAD), mean(sdEMH), mean(sdLOAD))

#select team 8f ----
All8f <- NoNAs %>%
  filter(Team == "8f5a6405211400c9e4befdc9f184046b")

sort(table(All8f$Speler))

SelectPlayersAll8f <- All8f %>%
  filter(Speler != "8dc12291626ff81d422ecfebd4c3dea0") %>%
  filter(Speler != "846a4bb1063435a68662b554ae827d11") %>%
  filter(Speler != "35f7149aa29b3e351d54e13420351254") %>%
  filter(Speler != "3ea36b1288a2c68e06aea0fe63510558") %>%
  arrange(Speler)

sort(table(SelectPlayersAll8f$Speler))

NewDayCountAll8f <- SelectPlayersAll8f %>% arrange(day_count) %>% 
  group_by(Speler) %>% 
  mutate(row_number = row_number()) %>%
  arrange(Speler)

SquareDataAll8f <- NewDayCountAll8f %>%
  filter(row_number > 0 & row_number < 33)

sort(table(SquareDataAll8f$Speler))

#Central tend 8f ----
#AggAn 
AggAnAll8f <- SquareDataAll8f %>%
  group_by(row_number) %>%
  summarise(meanEMH = mean(EMH_1st_session_first_fut_01_trainingdays), meanLOAD = mean(LOAD_sum_prev_01_days), sdEMH = sd(EMH_1st_session_first_fut_01_trainingdays), sdLOAD = sd(LOAD_sum_prev_01_days))

MeanAggAnAll8f <- AggAnAll8f %>%
  summarise(mean(meanEMH), mean(meanLOAD), mean(sdEMH), mean(sdLOAD))

----------------------------------------------------------------
#AnAgg 
AnAggAll8f <- SquareDataAll8f %>%
  group_by(Speler) %>%
  summarise(meanEMH = mean(EMH_1st_session_first_fut_01_trainingdays), meanLOAD = mean(LOAD_sum_prev_01_days), sdEMH = sd(EMH_1st_session_first_fut_01_trainingdays), sdLOAD = sd(LOAD_sum_prev_01_days))

MeanAnAggAll8f <- AnAggAll8f %>%
  summarise(mean(meanEMH), mean(meanLOAD), mean(sdEMH), mean(sdLOAD))

----------------------------------------------------------------
#All at once
AllatonceAll8f <- SquareDataAll8f %>%
  summarise(meanEMH = mean(EMH_1st_session_first_fut_01_trainingdays), meanLOAD = mean(LOAD_sum_prev_01_days), sdEMH = sd(EMH_1st_session_first_fut_01_trainingdays), sdLOAD = sd(LOAD_sum_prev_01_days))

MeanAllatonceAll8f <- AllatonceAll8f %>%
  summarise(mean(meanEMH), mean(meanLOAD), mean(sdEMH), mean(sdLOAD))
  

dim(SquareData$Speler)
glimpse(SquareData$Speler)
sort(SquareData$Speler)
sort(SquareDataAll1f$Speler)
glimpse(NewDayCount$Speler)
sort(table(NewDayCount$Speler))
(80+81+81+95+102+102+111+112+114+115+116+116+116+117+119+119+120+120+121+122+124+124+125+127+127+132+135+135+137+137+139+141+142+143+145+146+149+152+152+154+158+163+165+167+170+172+172+174+174+176+176+176+181+181+184+187+192+196+197+202+202+211+217+228+242+243+245+249+257)/69

mean(NewDayCount$Speler)
count(NewDayCount$Speler)
meanobs <- mean(NewDayCount$Speler)
