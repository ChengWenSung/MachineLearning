##

setwd("D:/wen_sung/psychology/Machine_learning/final_report")

##library packages

pacman::p_load(intsvy,tidyverse,plyr,ltm,gridExtra,coefplot)

##

dta <- read.csv("TIMSS_2011_8th.csv")

dta <- dta%>%mutate(Math = (BSMMAT01+BSMMAT02+BSMMAT03+BSMMAT04+BSMMAT05)/5)%>%dplyr::select(-BSMMAT01,-BSMMAT02,-BSMMAT03,-BSMMAT04,-BSMMAT05)

##

set.seed(20160619) 
idc_train <- sample(c(1,2,3,4), nrow(dta), replace = T, prob = c(0.25,0.25,0.25,0.25))

#use training data for model training
#testing 1 for checking model prediction
#testing 2 for ensemble learning
training_data <- dta[idc_train %in% c(1,2),]
testing1 <- dta[idc_train == 3,]
testing2 <- dta[idc_train == 4,]
